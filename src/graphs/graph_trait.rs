#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIndex(pub(super) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EdgeIndex(pub(super) usize);

mod refs {
    use super::{EdgeIndex, NodeIndex};

    #[derive(Debug)]
    pub struct NodeRef<'a, N> {
        contents: &'a N,
        index: usize,
    }

    impl<'a, N> Clone for NodeRef<'a, N> {
        fn clone(&self) -> Self {
            Self {
                contents: self.contents,
                index: self.index,
            }
        }
    }

    impl<'a, N> NodeRef<'a, N> {
        pub fn new(contents: &'a N, index: NodeIndex) -> Self {
            Self {
                contents,
                index: index.0,
            }
        }

        pub fn index(&self) -> NodeIndex {
            NodeIndex(self.index)
        }

        pub fn contents(&self) -> &'a N {
            self.contents
        }
    }

    #[derive(Debug)]
    pub struct EdgeRef<'a, N, E> {
        contents: &'a E,
        source: &'a N,
        target: &'a N,
        index: usize,
        source_index: usize,
        target_index: usize,
    }

    impl<'a, N, E> Clone for EdgeRef<'a, N, E> {
        fn clone(&self) -> Self {
            Self {
                contents: self.contents,
                index: self.index,
                source: self.source,
                target: self.target,
                source_index: self.source_index,
                target_index: self.target_index,
            }
        }
    }

    impl<'a, N, E> EdgeRef<'a, N, E> {
        pub fn new(
            contents: &'a E,
            source: &'a N,
            target: &'a N,
            index: EdgeIndex,
            source_index: NodeIndex,
            target_index: NodeIndex,
        ) -> Self {
            Self {
                contents,
                source,
                target,
                index: index.0,
                source_index: source_index.0,
                target_index: target_index.0,
            }
        }

        pub fn contents(&self) -> &'a E {
            self.contents
        }

        pub fn source(&self) -> NodeRef<'a, N> {
            NodeRef {
                contents: self.source,
                index: self.source_index,
            }
        }

        pub fn target(&self) -> NodeRef<'a, N> {
            NodeRef {
                contents: self.target,
                index: self.target_index,
            }
        }

        pub fn index(&self) -> EdgeIndex {
            EdgeIndex(self.index)
        }

        pub fn source_index(&self) -> NodeIndex {
            NodeIndex(self.source_index)
        }

        pub fn target_index(&self) -> NodeIndex {
            NodeIndex(self.target_index)
        }
    }
}

use std::{collections::BTreeSet, marker::PhantomData};

pub use refs::{EdgeRef, NodeRef};

use super::path::Path;

pub trait Graph<N, E> {
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
    where
        E: 'a,
        N: 'a;
    fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
    where
        E: 'a,
        N: 'a;

    fn edges_from<'a>(&'a self, node: NodeIndex) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        Box::new(self.edges().filter(move |e| e.source_index() == node))
    }
    fn edges_to<'a>(&'a self, node: NodeIndex) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        Box::new(self.edges().filter(move |e| e.target_index() == node))
    }
    fn targets_of<'a>(&'a self, node: NodeIndex) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        Box::new(self.edges_from(node).map(|e| e.target()))
    }
    fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b E,
    ) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'b>
    where
        E: 'a + Eq,
        N: 'a,
        'a: 'b,
    {
        Box::new(self.edges_from(node).filter(move |e| e.contents() == item))
    }

    fn node_with_contents<'a>(&'a self, contents: &N) -> Option<NodeRef<'a, N>>
    where
        N: Eq + 'a,
        E: 'a,
    {
        self.nodes().find(|n| n.contents() == contents)
    }
    fn edge_with_contents<'a>(&'a self, contents: &E) -> Option<EdgeRef<'a, N, E>>
    where
        E: Eq + 'a,
        N: 'a,
    {
        self.edges().find(|e| e.contents() == contents)
    }
    fn edge<'a>(&'a self, index: EdgeIndex) -> Option<EdgeRef<'a, N, E>>
    where
        E: 'a,
        N: 'a,
    {
        self.edges().find(|e| e.index() == index)
    }
    fn node<'a>(&'a self, index: NodeIndex) -> Option<NodeRef<'a, N>>
    where
        E: 'a,
        N: 'a,
    {
        self.nodes().find(|n| n.index() == index)
    }

    fn reachable_from<'a>(
        &'a self,
        node: NodeIndex,
    ) -> Box<dyn Iterator<Item = (Path<'a, N, E, Self>, NodeRef<'a, N>)> + 'a>
    where
        Self: Sized,
        E: 'a,
        N: 'a,
    {
        Box::new(ReachableFrom {
            graph: self,
            stack: vec![(Path::new(self), node)],
            reached: BTreeSet::new(),
            _p: PhantomData,
        })
    }
}

struct ReachableFrom<'a, N: 'a, E: 'a, G: Graph<N, E>> {
    graph: &'a G,
    stack: Vec<(Path<'a, N, E, G>, NodeIndex)>,
    reached: BTreeSet<NodeIndex>,
    _p: PhantomData<(N, E)>,
}

impl<'a, N, E, G: Graph<N, E>> Iterator for ReachableFrom<'a, N, E, G> {
    type Item = (Path<'a, N, E, G>, NodeRef<'a, N>);

    fn next(&mut self) -> Option<Self::Item> {
        let (path, node) = self.stack.pop()?;

        for edge in self
            .graph
            .edges_from(node)
            .filter(|e| !self.reached.contains(&e.target_index()))
        {
            let mut path_to_next = path.clone();
            path_to_next.push(edge.index());
            self.stack.push((path_to_next, edge.target_index()));
        }

        self.reached.insert(node);
        Some((path, self.graph.node(node).unwrap()))
    }
}

pub trait GraphBuilder<N, E> {
    type Output;

    fn new() -> Self;

    fn add_edge(&mut self, source: N, contents: E, target: N);
    fn add_node(&mut self, node: N);
    fn build(self) -> Self::Output;
    fn clear(&mut self);

    fn from_graph<G>(g: &G) -> Self
    where
        G: Graph<N, E>,
        Self: Sized,
        N: Clone,
        E: Clone,
    {
        let mut builder = Self::new();

        g.edges().for_each(|e| {
            builder.add_edge(
                e.source().contents().clone(),
                e.contents().clone(),
                e.target().contents().clone(),
            )
        });

        g.nodes()
            .for_each(|n| builder.add_node(n.contents().clone()));

        builder
    }
}
