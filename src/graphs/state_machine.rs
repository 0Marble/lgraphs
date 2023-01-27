use std::{collections::BTreeSet, marker::PhantomData};

use super::{
    graph_trait::{EdgeRef, Graph as GraphTrait, GraphBuilder, NodeIndex, NodeRef},
    path::Path,
};

pub struct StateMachine<N, E, G: GraphTrait<N, E>> {
    graph: G,
    start_node: NodeIndex,
    end_nodes: BTreeSet<NodeIndex>,
    _p: PhantomData<(N, E)>,
}

impl<N, E, G: GraphTrait<N, E>> StateMachine<N, E, G> {
    pub fn new<B>(graph: G, start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self
    where
        N: Eq + Clone,
        E: Clone,
        B: GraphBuilder<N, E, Output = G>,
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

impl<N, E, G: GraphTrait<N, E>> StateMachine<N, E, G> {
    pub fn start_node(&self) -> NodeRef<N> {
        self.node(self.start_node).unwrap()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeRef<N>> {
        self.end_nodes.iter().map(|n| self.node(*n).unwrap())
    }
    pub fn is_end_node(&self, index: NodeIndex) -> bool {
        self.end_nodes.contains(&index)
    }
}

impl<N, E, G: GraphTrait<N, E>> GraphTrait<N, E> for StateMachine<N, E, G> {
    fn edges<'a>(&'a self) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        self.graph.edges()
    }

    fn nodes<'a>(&'a self) -> Box<dyn Iterator<Item = NodeRef<'a, N>> + 'a>
    where
        E: 'a,
        N: 'a,
    {
        self.graph.nodes()
    }
}

impl<N, E, G: GraphTrait<N, E>> StateMachine<N, E, G> {
    pub fn determine<B>(&self) -> StateMachine<usize, E, B::Output>
    where
        B: GraphBuilder<usize, E>,
        B::Output: GraphTrait<usize, E>,
        E: Eq + Clone,
    {
        todo!()
    }
}

impl<N, E, G: GraphTrait<N, Option<E>>> StateMachine<N, Option<E>, G> {
    pub fn lambda_closure(
        &self,
        node: NodeIndex,
    ) -> impl Iterator<Item = (Path<N, Option<E>, Self>, NodeRef<N>)> + '_ {
        self.reachable_from(node)
            .filter(|(path, _)| path.edges().all(|e| e.contents().is_none()))
    }

    pub fn remove_nones<'a, B>(&'a self) -> StateMachine<&'a N, &'a E, B::Output>
    where
        B: GraphBuilder<&'a N, &'a E>,
        B::Output: GraphTrait<&'a N, &'a E>,
        N: Eq,
    {
        let mut builder = B::new();
        let mut end_nodes = vec![];

        for source in self.nodes() {
            for (contents, target) in self
                .lambda_closure(source.index())
                .map(|(_, n)| n)
                .map(|n| {
                    if self.is_end_node(n.index()) {
                        end_nodes.push(source.contents())
                    }

                    n
                })
                .flat_map(|n| self.edges_from(n.index()))
                .filter_map(|e| e.contents().as_ref().map(|c| (c, e.target().contents())))
            {
                builder.add_edge(source.contents(), contents, target);
            }
        }

        let graph = builder.build();
        StateMachine::new::<B>(graph, self.start_node().contents(), end_nodes)
    }
}
