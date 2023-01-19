use std::{collections::HashSet, ops::Index};

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

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub struct NodeRef {
    index: usize,
}

impl NodeRef {
    pub fn new(index: usize) -> Self {
        Self { index }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EdgeRef {
    index: usize,
}

impl EdgeRef {
    pub fn new(index: usize) -> Self {
        Self { index }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct PathRef {
    edges: Vec<EdgeRef>,
}

impl PathRef {
    pub fn new(edges: impl Into<Vec<EdgeRef>>) -> Self {
        Self {
            edges: edges.into(),
        }
    }
    pub fn empty(&self) -> bool {
        self.edges.is_empty()
    }
    pub fn push(mut self, edge: EdgeRef) -> Self {
        self.edges.push(edge);
        self
    }
}
impl Index<usize> for PathRef {
    type Output = EdgeRef;

    fn index(&self, index: usize) -> &Self::Output {
        &self.edges[index]
    }
}
impl IntoIterator for PathRef {
    type Item = EdgeRef;

    type IntoIter = std::vec::IntoIter<EdgeRef>;

    fn into_iter(self) -> Self::IntoIter {
        self.edges.into_iter()
    }
}
impl<'a> IntoIterator for &'a PathRef {
    type Item = &'a EdgeRef;

    type IntoIter = std::slice::Iter<'a, EdgeRef>;

    fn into_iter(self) -> Self::IntoIter {
        self.edges.iter()
    }
}

impl<L, I> Graph<L, I> {
    pub fn item(&self, edge: EdgeRef) -> Option<&I> {
        self.edges.get(edge.index).map(|e| &e.item)
    }
    pub fn label(&self, node: NodeRef) -> Option<&L> {
        self.nodes.get(node.index).map(|n| &n.label)
    }
    pub fn target(&self, edge: EdgeRef) -> Option<NodeRef> {
        self.edges.get(edge.index).map(|e| e.to).map(NodeRef::new)
    }
    pub fn source(&self, edge: EdgeRef) -> Option<NodeRef> {
        self.edges.get(edge.index).map(|e| e.from).map(NodeRef::new)
    }

    pub fn nodes(&self) -> impl Iterator<Item = NodeRef> + '_ {
        self.nodes.iter().enumerate().map(|(i, _n)| NodeRef::new(i))
    }
    pub fn edges(&self) -> impl Iterator<Item = EdgeRef> + '_ {
        self.edges.iter().enumerate().map(|(i, _e)| EdgeRef::new(i))
    }
    pub fn edges_from(&self, node: NodeRef) -> impl Iterator<Item = EdgeRef> + '_ {
        self.edges
            .iter()
            .enumerate()
            .filter(move |(_, e)| e.from == node.index)
            .map(|(i, _e)| EdgeRef::new(i))
    }
    pub fn edges_to(&self, node: NodeRef) -> impl Iterator<Item = EdgeRef> + '_ {
        self.edges
            .iter()
            .enumerate()
            .filter(move |(_, e)| e.to == node.index)
            .map(|(i, _e)| EdgeRef::new(i))
    }
    pub fn start_nodes(&self) -> impl Iterator<Item = NodeRef> + '_ {
        self.start_nodes.iter().map(|n| NodeRef::new(*n))
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeRef> + '_ {
        self.end_nodes.iter().map(|n| NodeRef::new(*n))
    }

    pub fn is_start_node(&self, node: NodeRef) -> bool {
        self.start_nodes.contains(&node.index)
    }
    pub fn is_end_node(&self, node: NodeRef) -> bool {
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
    pub fn parse<P>(parser: &mut P) -> Result<Self, P::Error>
    where
        P: GraphParser<Label = L, Item = I>,
    {
        parser.parse()
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
