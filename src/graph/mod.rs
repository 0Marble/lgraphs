#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeIndex {
    index: usize,
}

impl EdgeIndex {
    fn new(index: usize) -> Self {
        Self { index }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIndex {
    index: usize,
}

impl NodeIndex {
    fn new(index: usize) -> Self {
        Self { index }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeRef<'a, N, I> {
    index: EdgeIndex,
    source_index: NodeIndex,
    target_index: NodeIndex,
    item: &'a I,
    source_ref: &'a N,
    target_ref: &'a N,
}

impl<'a, N, I> EdgeRef<'a, N, I> {
    fn new(
        index: EdgeIndex,
        source_index: NodeIndex,
        target_index: NodeIndex,
        item: &'a I,
        source_ref: &'a N,
        target_ref: &'a N,
    ) -> Self {
        Self {
            index,
            source_index,
            target_index,
            item,
            source_ref,
            target_ref,
        }
    }

    pub fn index(&self) -> EdgeIndex {
        self.index
    }
    pub fn target_index(&self) -> NodeIndex {
        self.target_index
    }
    pub fn source_index(&self) -> NodeIndex {
        self.source_index
    }
    pub fn item(&self) -> &'a I {
        self.item
    }
    pub fn source(&self) -> &'a N {
        self.source_ref
    }
    pub fn target(&self) -> &'a N {
        self.target_ref
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeRef<'a, N> {
    index: NodeIndex,
    node_ref: &'a N,
}

impl<'a, N> NodeRef<'a, N> {
    fn new(index: NodeIndex, node_ref: &'a N) -> Self {
        Self { index, node_ref }
    }
    pub fn contents(&self) -> &N {
        self.node_ref
    }
    pub fn index(&self) -> NodeIndex {
        self.index
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InternalEdge<I> {
    source: usize,
    target: usize,
    item: I,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Graph<N, I> {
    edges: Vec<InternalEdge<I>>,
    nodes: Vec<N>,
}

impl<N, I> Graph<N, I>
where
    N: Eq,
{
    pub fn from_builder() -> GraphBuilder<N, I> {
        GraphBuilder {
            edges: vec![],
            nodes: vec![],
        }
    }

    pub fn to_builder(self) -> GraphBuilder<N, I> {
        GraphBuilder {
            edges: self.edges,
            nodes: self.nodes,
        }
    }
}

pub struct GraphBuilder<N, I>
where
    N: Eq,
{
    edges: Vec<InternalEdge<I>>,
    nodes: Vec<N>,
}

impl<N, I> GraphBuilder<N, I>
where
    N: Eq,
{
    pub fn clear(mut self) -> Self {
        self.edges.clear();
        self.nodes.clear();
        self
    }
    pub fn add_named_edge(mut self, edge: (N, I, N)) -> Self {
        let (source, item, target) = edge;
        let source_index = self
            .nodes
            .iter()
            .enumerate()
            .find(|(_, n)| source.eq(n))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.nodes.push(source);
                self.nodes.len() - 1
            });
        let target_index = self
            .nodes
            .iter()
            .enumerate()
            .find(|(_, n)| target.eq(n))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.nodes.push(target);
                self.nodes.len() - 1
            });
        self.edges.push(InternalEdge {
            source: source_index,
            target: target_index,
            item,
        });

        self
    }

    pub fn add_node(mut self, node: N) -> Self {
        let _ = self
            .nodes
            .iter()
            .enumerate()
            .find(|(_, n)| node.eq(n))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.nodes.push(node);
                self.nodes.len() - 1
            });
        self
    }

    pub fn build(self) -> Graph<N, I> {
        Graph {
            edges: self.edges,
            nodes: self.nodes,
        }
    }
}

impl<N, I> Graph<N, I> {
    pub fn edges(&self) -> Edges<'_, N, I> {
        Edges {
            graph: self,
            cur: 0,
        }
    }
    pub fn nodes(&self) -> Nodes<'_, N, I> {
        Nodes {
            graph: self,
            cur: 0,
        }
    }
    pub fn edges_from(&self, node: NodeIndex) -> EdgesFrom<'_, N, I> {
        EdgesFrom {
            graph: self,
            next: 0,
            source: node,
        }
    }
    pub fn targets_of(&self, node: NodeIndex) -> TargetsOf<'_, N, I> {
        TargetsOf {
            graph: self,
            next: 0,
            source: node,
        }
    }
    pub fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b I,
    ) -> EdgesFromWith<'a, 'b, N, I>
    where
        I: Eq,
    {
        EdgesFromWith {
            graph: self,
            next: 0,
            source: node,
            item,
        }
    }
    pub fn targets_of_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b I,
    ) -> TargetsOfWith<'a, 'b, N, I>
    where
        I: Eq,
    {
        TargetsOfWith {
            graph: self,
            next: 0,
            source: node,
            item,
        }
    }
    pub fn node_with(&self, content: &N) -> Option<NodeIndex>
    where
        N: Eq,
    {
        self.nodes()
            .flat_map(|n| self.node_ref(n))
            .find(|n| content.eq(n.contents()))
            .map(|n| n.index())
    }

    pub fn edge_ref(&self, edge: EdgeIndex) -> Option<EdgeRef<'_, N, I>> {
        self.edges.get(edge.index).map(|e| {
            EdgeRef::new(
                edge,
                NodeIndex { index: e.source },
                NodeIndex { index: e.target },
                &e.item,
                &self.nodes[e.source],
                &self.nodes[e.target],
            )
        })
    }
    pub fn node_ref(&self, node: NodeIndex) -> Option<NodeRef<'_, N>> {
        self.nodes
            .get(node.index)
            .map(|n| NodeRef::new(NodeIndex { index: node.index }, n))
    }
    pub fn item(&self, edge: EdgeIndex) -> Option<&I> {
        self.edges.get(edge.index).map(|e| &e.item)
    }
    pub fn target(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges
            .get(edge.index)
            .map(|e| e.target)
            .map(NodeIndex::new)
    }
    pub fn source(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges
            .get(edge.index)
            .map(|e| e.source)
            .map(NodeIndex::new)
    }
    pub fn node(&self, node: NodeIndex) -> Option<&N> {
        self.nodes.get(node.index)
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

pub struct Edges<'a, N, I> {
    graph: &'a Graph<N, I>,
    cur: usize,
}

impl<'a, N, I> Iterator for Edges<'a, N, I> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.graph.edges.len() > self.cur {
            self.cur += 1;
            Some(EdgeIndex {
                index: self.cur - 1,
            })
        } else {
            None
        }
    }
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        if self.graph.edges.len() > n {
            self.cur = n + 1;
            Some(EdgeIndex::new(n))
        } else {
            None
        }
    }
}
pub struct Nodes<'a, N, I> {
    graph: &'a Graph<N, I>,
    cur: usize,
}

impl<'a, N, I> Iterator for Nodes<'a, N, I> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.graph.nodes.len() > self.cur {
            self.cur += 1;
            Some(NodeIndex {
                index: self.cur - 1,
            })
        } else {
            None
        }
    }
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        if self.graph.nodes.len() > n {
            self.cur = n + 1;
            Some(NodeIndex::new(n))
        } else {
            None
        }
    }
}
pub struct EdgesFrom<'a, N, I> {
    graph: &'a Graph<N, I>,
    next: usize,
    source: NodeIndex,
}

impl<'a, N, I> Iterator for EdgesFrom<'a, N, I> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.next;
        for edge in &self.graph.edges[self.next..] {
            if edge.source == self.source.index {
                self.next = i + 1;
                return Some(EdgeIndex { index: i });
            }
            i += 1;
        }

        None
    }
}
pub struct TargetsOf<'a, N, I> {
    graph: &'a Graph<N, I>,
    next: usize,
    source: NodeIndex,
}

impl<'a, N, I> Iterator for TargetsOf<'a, N, I> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.next;
        for edge in &self.graph.edges[self.next..] {
            if edge.source == self.source.index {
                self.next = i + 1;
                return Some(NodeIndex { index: edge.target });
            }
            i += 1;
        }

        None
    }
}
pub struct EdgesFromWith<'a, 'b, N, I> {
    graph: &'a Graph<N, I>,
    next: usize,
    source: NodeIndex,
    item: &'b I,
}

impl<'a, 'b, N, I> Iterator for EdgesFromWith<'a, 'b, N, I>
where
    I: Eq,
{
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.next;
        for edge in &self.graph.edges[self.next..] {
            if edge.source == self.source.index && &edge.item == self.item {
                self.next = i + 1;
                return Some(EdgeIndex { index: i });
            }
            i += 1;
        }

        None
    }
}
pub struct TargetsOfWith<'a, 'b, N, I> {
    graph: &'a Graph<N, I>,
    next: usize,
    source: NodeIndex,
    item: &'b I,
}

impl<'a, 'b, N, I> Iterator for TargetsOfWith<'a, 'b, N, I>
where
    I: Eq,
{
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.next;
        for edge in &self.graph.edges[self.next..] {
            if edge.source == self.source.index && &edge.item == self.item {
                self.next = i + 1;
                return Some(NodeIndex { index: edge.target });
            }
            i += 1;
        }

        None
    }
}

pub struct NodesWith<'a, 'b, N, I> {
    graph: &'a Graph<N, I>,
    node: &'b N,
    cur: usize,
}
impl<'a, 'b, N, I> Iterator for NodesWith<'a, 'b, N, I>
where
    N: Eq,
{
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.cur;
        for node in &self.graph.nodes[self.cur..] {
            if self.node == node {
                self.cur = i + 1;
                return Some(NodeIndex { index: i });
            }
            i += 1;
        }

        None
    }
}
