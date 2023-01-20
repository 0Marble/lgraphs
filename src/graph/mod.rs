use std::{collections::HashSet, hash::Hash, ops::Index};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge<I> {
    from: usize,
    to: usize,
    item: I,
}

impl<I> Edge<I> {
    pub fn new(from: usize, to: usize, item: I) -> Self {
        Self { from, to, item }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node<L> {
    label: L,
}

impl<L> Node<L> {
    pub fn new(label: L) -> Self {
        Self { label }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Graph<L, I> {
    edges: Vec<Edge<I>>,
    nodes: Vec<Node<L>>,
    start_nodes: HashSet<usize>,
    end_nodes: HashSet<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeIndex {
    index: usize,
}

impl NodeIndex {
    pub fn new(index: usize) -> Self {
        Self { index }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EdgeIndex {
    index: usize,
}

impl EdgeIndex {
    pub fn new(index: usize) -> Self {
        Self { index }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct PathRef {
    edges: Vec<EdgeIndex>,
}

impl PathRef {
    pub fn new(edges: impl Into<Vec<EdgeIndex>>) -> Self {
        Self {
            edges: edges.into(),
        }
    }
    pub fn empty(&self) -> bool {
        self.edges.is_empty()
    }
    pub fn push(mut self, edge: EdgeIndex) -> Self {
        self.edges.push(edge);
        self
    }
}
impl Index<usize> for PathRef {
    type Output = EdgeIndex;

    fn index(&self, index: usize) -> &Self::Output {
        &self.edges[index]
    }
}
impl IntoIterator for PathRef {
    type Item = EdgeIndex;

    type IntoIter = std::vec::IntoIter<EdgeIndex>;

    fn into_iter(self) -> Self::IntoIter {
        self.edges.into_iter()
    }
}
impl<'a> IntoIterator for &'a PathRef {
    type Item = &'a EdgeIndex;

    type IntoIter = std::slice::Iter<'a, EdgeIndex>;

    fn into_iter(self) -> Self::IntoIter {
        self.edges.iter()
    }
}

#[derive(Debug, Clone)]
pub struct EdgeRef<'a, I, L> {
    index: usize,
    from_index: usize,
    to_index: usize,
    from: &'a Node<L>,
    to: &'a Node<L>,
    item: &'a I,
}

impl<'a, I, L> EdgeRef<'a, I, L> {
    pub fn source(&self) -> &Node<L> {
        self.from
    }
    pub fn target(&self) -> &Node<L> {
        self.to
    }
    pub fn item(&self) -> &'a I {
        self.item
    }
    pub fn index(&self) -> EdgeIndex {
        EdgeIndex { index: self.index }
    }
    pub fn source_index(&self) -> NodeIndex {
        NodeIndex {
            index: self.from_index,
        }
    }
    pub fn target_index(&self) -> NodeIndex {
        NodeIndex {
            index: self.to_index,
        }
    }
}
pub struct NodeRef<'a, L> {
    index: usize,
    label: &'a L,
}

impl<'a, L> NodeRef<'a, L> {
    pub fn index(&self) -> NodeIndex {
        NodeIndex { index: self.index }
    }
    pub fn label(&self) -> &L {
        self.label
    }
}

impl<L, I> Graph<L, I> {
    pub fn item(&self, edge: EdgeIndex) -> Option<&I> {
        self.edges.get(edge.index).map(|e| &e.item)
    }
    pub fn label(&self, node: NodeIndex) -> Option<&L> {
        self.nodes.get(node.index).map(|n| &n.label)
    }
    pub fn target(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges.get(edge.index).map(|e| e.to).map(NodeIndex::new)
    }
    pub fn source(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges
            .get(edge.index)
            .map(|e| e.from)
            .map(NodeIndex::new)
    }
    pub fn edge_ref(&self, edge: EdgeIndex) -> Option<EdgeRef<'_, I, L>> {
        self.edges.get(edge.index).map(|e| EdgeRef {
            from: &self.nodes[e.from],
            to: &self.nodes[e.to],
            item: &e.item,
            index: edge.index,
            from_index: e.from,
            to_index: e.to,
        })
    }
    pub fn node_ref(&self, node: NodeIndex) -> Option<NodeRef<'_, L>> {
        self.nodes.get(node.index).map(|n| NodeRef {
            index: node.index,
            label: &n.label,
        })
    }

    pub fn nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.nodes
            .iter()
            .enumerate()
            .map(|(i, _n)| NodeIndex::new(i))
    }
    pub fn edges(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.edges
            .iter()
            .enumerate()
            .map(|(i, _e)| EdgeIndex::new(i))
    }
    pub fn edges_from(&self, node: NodeIndex) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.edges
            .iter()
            .enumerate()
            .filter(move |(_, e)| e.from == node.index)
            .map(|(i, _e)| EdgeIndex::new(i))
    }
    pub fn edges_to(&self, node: NodeIndex) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.edges
            .iter()
            .enumerate()
            .filter(move |(_, e)| e.to == node.index)
            .map(|(i, _e)| EdgeIndex::new(i))
    }
    pub fn start_nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.start_nodes.iter().map(|n| NodeIndex::new(*n))
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.end_nodes.iter().map(|n| NodeIndex::new(*n))
    }
    pub fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b I,
    ) -> impl Iterator<Item = EdgeIndex> + 'a
    where
        'b: 'a,
        I: Eq,
    {
        self.edges_from(node)
            .flat_map(|e| self.edge_ref(e))
            .filter(move |e| e.item() == item)
            .map(|e| e.index())
    }

    pub fn is_start_node(&self, node: NodeIndex) -> bool {
        self.start_nodes.contains(&node.index)
    }
    pub fn is_end_node(&self, node: NodeIndex) -> bool {
        self.end_nodes.contains(&node.index)
    }
}

pub trait GraphParser {
    type Label;
    type Item;
    type Error;

    fn parse(&mut self) -> Result<Graph<Self::Label, Self::Item>, Self::Error>;
}

impl<I, L> Graph<L, I> {
    pub fn from_parser<P>(parser: &mut P) -> Result<Self, P::Error>
    where
        P: GraphParser<Label = L, Item = I>,
    {
        parser.parse()
    }
    pub fn from_edges(
        edges: impl IntoIterator<Item = (Node<L>, I, Node<L>)>,
        start_nodes: impl IntoIterator<Item = Node<L>>,
        end_nodes: impl IntoIterator<Item = Node<L>>,
    ) -> Self
    where
        L: Hash + Eq + PartialEq,
    {
        let mut nodes = vec![];
        let mut converted_edges = vec![];
        let mut start_nodes_set = HashSet::new();
        let mut end_nodes_set = HashSet::new();

        for (from, item, to) in edges.into_iter() {
            let from_index = nodes
                .iter()
                .enumerate()
                .find(|(_, n)| from.eq(n))
                .map(|(i, _)| i);

            let from_index = match from_index {
                Some(i) => i,
                None => {
                    nodes.push(from);
                    nodes.len() - 1
                }
            };
            let to_index = nodes
                .iter()
                .enumerate()
                .find(|(_, n)| to.eq(n))
                .map(|(i, _)| i);
            let to_index = match to_index {
                Some(i) => i,
                None => {
                    nodes.push(to);
                    nodes.len() - 1
                }
            };

            converted_edges.push(Edge::new(from_index, to_index, item));
        }

        for node in start_nodes.into_iter() {
            let index = nodes
                .iter()
                .enumerate()
                .find(|(_, n)| node.eq(n))
                .map(|(i, _)| i);
            let index = match index {
                Some(i) => i,
                None => {
                    nodes.push(node);
                    nodes.len() - 1
                }
            };
            start_nodes_set.insert(index);
        }
        for node in end_nodes.into_iter() {
            let index = nodes
                .iter()
                .enumerate()
                .find(|(_, n)| node.eq(n))
                .map(|(i, _)| i);
            let index = match index {
                Some(i) => i,
                None => {
                    nodes.push(node);
                    nodes.len() - 1
                }
            };
            end_nodes_set.insert(index);
        }

        Self {
            edges: converted_edges,
            nodes,
            start_nodes: start_nodes_set,
            end_nodes: end_nodes_set,
        }
    }
}

#[cfg(test)]
pub mod testing_parser {
    use std::collections::HashSet;

    use crate::graph::{Edge, Graph, Node};

    use super::GraphParser;

    pub struct TestingParser<I, L> {
        edges: Vec<Edge<I>>,
        nodes: Vec<Node<L>>,
        start_nodes: HashSet<usize>,
        end_nodes: HashSet<usize>,
    }

    impl<I, L> TestingParser<I, L> {
        pub fn new(
            edges: impl Into<Vec<Edge<I>>>,
            nodes: impl Into<Vec<Node<L>>>,
            start_nodes: impl Into<HashSet<usize>>,
            end_nodes: impl Into<HashSet<usize>>,
        ) -> Self {
            Self {
                edges: edges.into(),
                nodes: nodes.into(),
                start_nodes: start_nodes.into(),
                end_nodes: end_nodes.into(),
            }
        }
    }
    impl<I, L> GraphParser for TestingParser<I, L>
    where
        I: Clone,
        L: Clone,
    {
        type Label = L;
        type Item = I;

        type Error = ();

        fn parse(&mut self) -> Result<crate::graph::Graph<Self::Label, Self::Item>, Self::Error> {
            Ok(Graph {
                edges: self.edges.clone(),
                nodes: self.nodes.clone(),
                start_nodes: self.start_nodes.clone(),
                end_nodes: self.end_nodes.clone(),
            })
        }
    }
}
