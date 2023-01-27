use std::marker::PhantomData;

use super::graph_trait::{EdgeIndex, EdgeRef, Graph};

pub struct Path<'a, N, E, G: Graph<N, E>> {
    edges: Vec<EdgeIndex>,
    graph: &'a G,
    _p: PhantomData<(N, E)>,
}

impl<'a, N, E, G: Graph<N, E>> Clone for Path<'a, N, E, G> {
    fn clone(&self) -> Self {
        Self {
            edges: self.edges.clone(),
            graph: self.graph,
            _p: PhantomData,
        }
    }
}

impl<'a, N, E, G: Graph<N, E>> Path<'a, N, E, G> {
    pub fn new(graph: &'a G) -> Self {
        Self {
            edges: vec![],
            graph,
            _p: PhantomData,
        }
    }

    pub fn push(&mut self, edge: EdgeIndex) {
        self.edges.push(edge);
    }

    pub fn edges(&self) -> impl Iterator<Item = EdgeRef<N, E>> {
        self.edges.iter().map(|e| self.graph.edge(*e).unwrap())
    }

    pub fn len(&self) -> usize {
        self.edges.len()
    }

    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }
}
