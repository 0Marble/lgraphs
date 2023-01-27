use std::{collections::BTreeSet, marker::PhantomData};

use super::graph_trait::{EdgeRef, Graph, GraphBuilder, NodeIndex, NodeRef};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bracket {
    index: usize,
    is_opening: bool,
}

impl Bracket {
    pub fn new(index: usize, is_opening: bool) -> Self {
        Self { index, is_opening }
    }
    pub fn index(&self) -> usize {
        self.index
    }
    pub fn is_opening(&self) -> bool {
        self.is_opening
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item<E> {
    item: Option<E>,
    bracket: Bracket,
}

impl<E> Item<E> {
    pub fn new(item: Option<E>, bracket: Bracket) -> Self {
        Self { item, bracket }
    }

    pub fn item(&self) -> Option<&E> {
        self.item.as_ref()
    }

    pub fn bracket(&self) -> &Bracket {
        &self.bracket
    }
}

pub struct LGraph<N, E, G> {
    graph: G,
    start_node: NodeIndex,
    end_nodes: BTreeSet<NodeIndex>,
    _p: PhantomData<(N, E)>,
}

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
{
    pub fn start_node(&self) -> NodeRef<N> {
        self.graph.node(self.start_node).unwrap()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeRef<N>> + '_ {
        self.end_nodes.iter().map(|n| self.graph.node(*n).unwrap())
    }
    pub fn is_ned_node(&self, node: NodeIndex) -> bool {
        self.end_nodes.contains(&node)
    }

    pub fn new_unchecked<B, G0>(
        graph: G0,
        start_node: N,
        end_nodes: impl IntoIterator<Item = N>,
    ) -> Self
    where
        G0: Graph<N, Item<E>>,
        N: Eq + Clone,
        E: Clone,
        B: GraphBuilder<N, Item<E>, Output = G>,
    {
        let mut builder = B::from_graph(&graph);
        builder.add_node(start_node.clone());
        let end_nodes = end_nodes.into_iter().collect::<Vec<_>>();
        for end_node in end_nodes.iter() {
            builder.add_node(end_node.clone());
        }

        let graph = builder.build();

        Self {
            start_node: graph.node_with_contents(&start_node).unwrap().index(),
            end_nodes: end_nodes
                .into_iter()
                .map(|node| graph.node_with_contents(&node).map(|n| n.index()))
                .collect::<Option<BTreeSet<_>>>()
                .unwrap(),
            graph,
            _p: PhantomData,
        }
    }
}

impl<N, E, G> Graph<N, Item<E>> for LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
{
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = EdgeRef<'a, N, Item<E>>> + 'a>
    where
        Item<E>: 'a,
        N: 'a,
    {
        self.graph.edges()
    }

    fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
    where
        Item<E>: 'a,
        N: 'a,
    {
        self.graph.nodes()
    }
}
