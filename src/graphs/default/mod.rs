use super::graph_trait::{
    EdgeIndex, EdgeRef, Graph as GraphTrait, GraphBuilder as GraphBuilderTrait, NodeIndex, NodeRef,
};

pub struct Graph<N, E> {
    edges: Vec<(usize, E, usize)>,
    nodes: Vec<N>,
}

impl<N, E> Graph<N, E> {
    fn to_edge_ref<'a>(
        &'a self,
        index: EdgeIndex,
        source: NodeIndex,
        contents: &'a E,
        target: NodeIndex,
    ) -> EdgeRef<N, E> {
        EdgeRef::new(
            contents,
            &self.nodes[source.0],
            &self.nodes[target.0],
            index,
            source,
            target,
        )
    }

    fn to_node_ref<'a>(&'a self, index: NodeIndex, contents: &'a N) -> NodeRef<N> {
        NodeRef::new(contents, index)
    }
}

impl<N, E> GraphTrait<N, E> for Graph<N, E> {
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        Box::new(
            self.edges
                .iter()
                .enumerate()
                .map(|(i, (source, contents, target))| {
                    self.to_edge_ref(
                        EdgeIndex(i),
                        NodeIndex(*source),
                        contents,
                        NodeIndex(*target),
                    )
                }),
        )
    }

    fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        Box::new(
            self.nodes
                .iter()
                .enumerate()
                .map(|(i, n)| self.to_node_ref(NodeIndex(i), n)),
        )
    }

    fn edge<'a>(&'a self, index: EdgeIndex) -> Option<EdgeRef<'a, N, E>>
    where
        E: 'a,
        N: 'a,
    {
        self.edges.get(index.0).map(|(source, contents, target)| {
            self.to_edge_ref(index, NodeIndex(*source), contents, NodeIndex(*target))
        })
    }
    fn node<'a>(&'a self, index: NodeIndex) -> Option<NodeRef<'a, N>>
    where
        E: 'a,
        N: 'a,
    {
        self.nodes.get(index.0).map(|n| self.to_node_ref(index, n))
    }
}

impl<N, E> Graph<N, E>
where
    N: Eq,
    E: Eq,
{
    pub fn from_builder() -> GraphBuilder<N, E> {
        GraphBuilder {
            edges: vec![],
            nodes: vec![],
        }
    }

    pub fn to_builder(self) -> GraphBuilder<N, E> {
        GraphBuilder {
            edges: self.edges,
            nodes: self.nodes,
        }
    }
}

pub struct GraphBuilder<N: Eq, E: Eq> {
    edges: Vec<(usize, E, usize)>,
    nodes: Vec<N>,
}

impl<N, E> GraphBuilderTrait<N, E> for GraphBuilder<N, E>
where
    N: Eq + Clone,
    E: Eq + Clone,
{
    type Output = Graph<N, E>;

    fn add_edge(&mut self, source: N, contents: E, target: N) {
        self.add_or_find_edge(source, contents, target);
    }

    fn add_node(&mut self, node: N) {
        self.add_or_find_node(node);
    }

    fn build(self) -> Self::Output {
        Graph {
            edges: self.edges,
            nodes: self.nodes,
        }
    }

    fn clear(&mut self) {
        self.edges.clear();
        self.nodes.clear();
    }

    fn new() -> Self {
        Self {
            edges: vec![],
            nodes: vec![],
        }
    }
}

impl<N: Eq, E: Eq> GraphBuilder<N, E> {
    fn add_or_find_node(&mut self, node: N) -> usize
    where
        N: Clone,
    {
        self.nodes
            .iter()
            .enumerate()
            .find(|(_, n)| node.eq(n))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.nodes.push(node.clone());
                self.nodes.len() - 1
            })
    }

    fn add_or_find_edge(&mut self, source: N, contents: E, target: N) -> usize
    where
        N: Clone,
        E: Clone,
    {
        self.edges
            .iter()
            .enumerate()
            .find(|(_, (s, c, t))| {
                source.eq(&self.nodes[*s]) && contents.eq(c) && target.eq(&self.nodes[*t])
            })
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                let source = self.add_or_find_node(source);
                let target = self.add_or_find_node(target);
                self.edges.push((source, contents.clone(), target));
                self.edges.len() - 1
            })
    }
}

// pub struct RelatedGraph<'a, N, E, G1, G2>
// where
//     G1: GraphTrait<N, E>,
//     G2: GraphTrait<NodeIndex, EdgeIndex>,
// {
//     pub(super) original: &'a G1,
//     pub(super) related: G2,
//     _p: PhantomData<(&'a N, &'a E)>,
// }

// impl<'b, N, E, G1, G2> GraphTrait<N, E> for RelatedGraph<'b, N, E, G1, G2>
// where
//     G1: GraphTrait<N, E>,
//     G2: GraphTrait<NodeIndex, EdgeIndex>,
// {
//     fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
//     where
//         E: 'a,
//         N: 'a,
//     {
//         Box::new(self.related.edges().map(|e| self.edge(e.index()).unwrap()))
//     }

//     fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
//     where
//         E: 'a,
//         N: 'a,
//     {
//         Box::new(self.related.nodes().map(|n| self.node(n.index()).unwrap()))
//     }

//     fn node<'a>(&'a self, index: NodeIndex) -> Option<NodeRef<'a, N>>
//     where
//         E: 'a,
//         N: 'a,
//     {
//         let index = self.related.node(index)?.index();
//         Some(NodeRef::new(self.original.node(index)?.contents(), index))
//     }
//     fn edge<'a>(&'a self, index: EdgeIndex) -> Option<EdgeRef<'a, N, E>>
//     where
//         E: 'a,
//         N: 'a,
//     {
//         let edge = self.related.edge(index)?;
//         Some(EdgeRef::new(
//             self.original.edge(edge.index())?.contents(),
//             self.original.node(edge.source_index())?.contents(),
//             self.original.node(edge.target_index())?.contents(),
//             edge.index(),
//             edge.source_index(),
//             edge.target_index(),
//         ))
//     }
// }
