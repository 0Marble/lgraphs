use std::collections::HashSet;

use super::{
    graph_trait::{Builder, Graph},
    refs::{EdgeRef, NodeRef},
};

#[derive(Debug, Clone)]
struct Edge<E> {
    source: usize,
    target: usize,
    contents: E,
}

#[derive(Debug)]
pub struct DefaultGraph<N, E> {
    nodes: Vec<N>,
    edges: Vec<Edge<E>>,

    start_node: usize,
    end_nodes: HashSet<usize>,
}

impl<N, E> Graph<N, E> for DefaultGraph<N, E> {
    fn start_node(&self) -> NodeRef<'_, N> {
        NodeRef::new(&self.nodes[self.start_node], self.start_node)
    }

    fn is_end_node(&self, node: NodeRef<'_, N>) -> bool {
        self.end_nodes.contains(&node.index())
    }

    fn nodes(&self) -> Box<dyn Iterator<Item = NodeRef<'_, N>> + '_> {
        Box::new(
            self.nodes
                .iter()
                .enumerate()
                .map(|(i, n)| NodeRef::new(n, i)),
        )
    }

    fn edges(&self) -> Box<dyn Iterator<Item = EdgeRef<'_, N, E>> + '_> {
        Box::new(self.edges.iter().enumerate().map(|(i, e)| {
            EdgeRef::new(
                &e.contents,
                &self.nodes[e.source],
                &self.nodes[e.target],
                i,
                e.source,
                e.target,
            )
        }))
    }
}

pub struct DefaultBuilder<N, E>
where
    N: Eq,
    E: Eq,
{
    nodes: Vec<N>,
    edges: Vec<Edge<E>>,
}

impl<N, E> Default for DefaultBuilder<N, E>
where
    N: Eq,
    E: Eq,
{
    fn default() -> Self {
        Self {
            nodes: vec![],
            edges: vec![],
        }
    }
}

impl<N, E> Builder<N, E> for DefaultBuilder<N, E>
where
    N: Eq,
    E: Eq,
{
    type TargetGraph = DefaultGraph<N, E>;

    fn add_edge(&mut self, source: N, edge: E, target: N) {
        self.add_or_find_edge(source, edge, target);
    }

    fn add_node(&mut self, node: N) {
        self.add_or_find_node(node);
    }

    fn clear(&mut self) {
        self.edges.clear();
        self.nodes.clear();
    }

    fn build(
        &mut self,
        start_node: N,
        end_nodes: impl IntoIterator<Item = N>,
    ) -> Self::TargetGraph {
        let start_node = self.add_or_find_node(start_node);
        let end_nodes = end_nodes
            .into_iter()
            .map(|n| self.add_or_find_node(n))
            .collect();

        DefaultGraph {
            nodes: self.nodes.drain(..).collect(),
            edges: self.edges.drain(..).collect(),
            start_node,
            end_nodes,
        }
    }
}

impl<N, E> DefaultBuilder<N, E>
where
    N: Eq,
    E: Eq,
{
    fn add_or_find_node(&mut self, node: N) -> usize {
        self.nodes
            .iter()
            .enumerate()
            .find(|(_, n)| n.eq(&&node))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.nodes.push(node);
                self.nodes.len() - 1
            })
    }

    fn add_or_find_edge(&mut self, source: N, contents: E, target: N) -> usize {
        let source = self.add_or_find_node(source);
        let target = self.add_or_find_node(target);
        self.edges
            .iter()
            .enumerate()
            .find(|(_, e)| e.contents.eq(&contents) && e.target == target && e.source == source)
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                self.edges.push(Edge {
                    source,
                    target,
                    contents,
                });
                self.edges.len() - 1
            })
    }
}
