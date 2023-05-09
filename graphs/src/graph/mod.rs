use crate::path::{Edge, Letter, Node};

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
}
