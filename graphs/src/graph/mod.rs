use std::collections::HashSet;

use crate::path::{Edge, Letter, Node, Path};

pub mod default_graph;
pub mod lgraph;

pub trait Graph<N, L>
where
    N: Node,
    L: Letter,
{
    fn nodes(&self) -> Box<dyn Iterator<Item = &N> + '_>;
    fn edges(&self) -> Box<dyn Iterator<Item = &Edge<N, L>> + '_>;
    fn start_node(&self) -> &N;
    fn end_nodes(&self) -> Box<dyn Iterator<Item = &N> + '_>;

    fn node_count(&self) -> usize {
        self.nodes().count()
    }
    fn is_start_node(&self, node: &N) -> bool {
        self.start_node() == node
    }
    fn is_end_node(&self, node: &N) -> bool {
        self.end_nodes().any(|n| n == node)
    }
    fn edges_from<'a, 'b>(&'a self, node: &'b N) -> Box<dyn Iterator<Item = &Edge<N, L>> + 'a>
    where
        'b: 'a,
        L: 'a,
    {
        Box::new(self.edges().filter(move |e| e.beg() == node))
    }
    fn edges_to<'a, 'b>(&'a self, node: &'b N) -> Box<dyn Iterator<Item = &Edge<N, L>> + 'a>
    where
        'b: 'a,
        L: 'a,
    {
        Box::new(self.edges().filter(move |e| e.end() == node))
    }
    fn has_node(&self, node: &N) -> bool {
        self.nodes().any(|n| n == node)
    }
    fn has_edge(&self, edge: &Edge<N, L>) -> bool {
        self.edges().any(|e| e == edge)
    }
}

pub trait ModifyableGraph<N, L>: Graph<N, L>
where
    N: Node,
    L: Letter,
{
    fn new_empty(start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self;
    fn add_edge(&mut self, edge: Edge<N, L>);

    fn from_paths(paths: impl IntoIterator<Item = Path<N, L>>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut edges = HashSet::new();
        let mut end_nodes = HashSet::new();
        let mut start_node = None;

        for path in paths.into_iter() {
            if start_node.is_none() {
                start_node = Some(path.beg().clone());
            }
            end_nodes.insert(path.end().clone());

            for edge in path.edges() {
                edges.insert(edge.clone());
            }
        }

        start_node.map(|start_node| {
            let mut g = Self::new_empty(start_node, end_nodes);
            for edge in edges {
                g.add_edge(edge);
            }
            g
        })
    }
}
