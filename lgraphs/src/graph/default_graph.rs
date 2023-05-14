use std::collections::HashSet;

use crate::path::{Edge, Letter, Node};

use super::{Graph, ModifyableGraph};

#[derive(Debug, Clone)]
pub struct DefaultGraph<N, L>
where
    N: Node,
    L: Letter,
{
    edges: Vec<Edge<N, L>>,
    nodes: HashSet<N>,
    start_node: N,
    end_nodes: HashSet<N>,
}

impl<N, L> PartialEq for DefaultGraph<N, L>
where
    N: Node + PartialEq,
    L: Letter + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.start_node != other.start_node
            || self.end_nodes != other.end_nodes
            || self.edges.len() != other.edges.len()
        {
            return false;
        }

        let mut accounted = HashSet::new();
        for edge in &self.edges {
            let mut found = false;

            for (i, e) in other.edges.iter().enumerate() {
                if accounted.contains(&i) {
                    continue;
                }
                if e == edge {
                    accounted.insert(i);
                    found = true;
                    break;
                }
            }

            if !found {
                return false;
            }
        }

        true
    }
}

impl<N, L> ModifyableGraph<N, L> for DefaultGraph<N, L>
where
    N: Node,
    L: Letter,
{
    fn new_empty(start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self {
        Self::new(start_node, end_nodes)
    }
    fn add_edge(&mut self, edge: Edge<N, L>) {
        self.nodes.insert(edge.beg().clone());
        self.nodes.insert(edge.end().clone());
        self.edges.push(edge)
    }
}

impl<N, L> Graph<N, L> for DefaultGraph<N, L>
where
    N: Node,
    L: Letter,
{
    fn nodes(&self) -> Box<dyn Iterator<Item = &N> + '_> {
        Box::new(self.nodes.iter())
    }

    fn edges(&self) -> Box<dyn Iterator<Item = &Edge<N, L>> + '_> {
        Box::new(self.edges.iter())
    }

    fn start_node(&self) -> &N {
        &self.start_node
    }

    fn end_nodes(&self) -> Box<dyn Iterator<Item = &N> + '_> {
        Box::new(self.end_nodes.iter())
    }
}

impl<N, L> DefaultGraph<N, L>
where
    N: Node,
    L: Letter,
{
    pub fn new(start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self {
        let mut nodes = HashSet::from([start_node.clone()]);
        let end_nodes = HashSet::from_iter(end_nodes.into_iter());
        for n in &end_nodes {
            nodes.insert(n.clone());
        }
        Self {
            edges: vec![],
            nodes,
            start_node,
            end_nodes,
        }
    }
}
