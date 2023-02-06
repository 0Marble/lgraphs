use std::{fmt::Display, marker::PhantomData};

use super::{
    graph_trait::{Builder, Graph},
    refs::{EdgeRef, NodeRef, Path},
    state_machine::StateMachine,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketType {
    Open,
    Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bracket {
    index: usize,
    open: BracketType,
}

impl Bracket {
    pub fn new(index: usize, open: BracketType) -> Self {
        Self { index, open }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BracketStack {
    stack: Vec<usize>,
}

impl BracketStack {
    pub fn brackets(&self) -> impl Iterator<Item = Bracket> + '_ {
        self.stack.iter().cloned().map(|i| Bracket {
            index: i,
            open: BracketType::Open,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item<E> {
    item: Option<E>,
    bracket: Bracket,
}

impl<E> Display for Item<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.item() {
            Some(item) => write!(f, "{}", item),
            None => write!(f, "_"),
        }?;
        match self.bracket.open {
            BracketType::Open => write!(f, "[{}", self.bracket.index),
            BracketType::Close => write!(f, "]{}", self.bracket.index),
        }
    }
}

impl<E> Item<E> {
    pub fn new(item: Option<E>, bracket: Bracket) -> Self {
        Self { item, bracket }
    }

    pub fn item(&self) -> &Option<E> {
        &self.item
    }

    pub fn bracket(&self) -> Bracket {
        self.bracket
    }

    pub fn as_ref(&self) -> Item<&E> {
        Item {
            item: self.item.as_ref(),
            bracket: self.bracket,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Memory<'a, N> {
    node: &'a N,
    stack: BracketStack,
}

impl<'a, N> Display for Memory<'a, N>
where
    N: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},", self.node())?;
        for bracket in self.stack().brackets() {
            write!(f, "{}", bracket.index)?;
        }

        Ok(())
    }
}

impl<'a, N> Clone for Memory<'a, N> {
    fn clone(&self) -> Self {
        Self {
            node: self.node,
            stack: self.stack.clone(),
        }
    }
}

impl<'a, N> Memory<'a, N> {
    pub fn node(&self) -> &'a N {
        self.node
    }

    pub fn stack(&self) -> &BracketStack {
        &self.stack
    }
}

pub struct LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
{
    graph: G,
    _p: PhantomData<(N, E)>,
}

impl<'a, N, E, G> LGraph<Memory<'a, N>, &'a E, G>
where
    G: Graph<Memory<'a, N>, Item<&'a E>>,
{
    pub fn regular_image<'b, B>(
        &self,
        builder: &mut B,
    ) -> StateMachine<Memory<'a, N>, Option<&'a E>, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Option<&'a E>>,
    {
        builder.clear();

        for edge in self.edges() {
            builder.add_edge(
                edge.source().contents().clone(),
                *edge.contents().item(),
                edge.target().contents().clone(),
            );
        }

        for node in self.nodes() {
            builder.add_node(node.contents().clone());
        }

        StateMachine::new(builder.build(
            self.start_node().contents().clone(),
            self.end_nodes().map(|n| n.contents()).cloned(),
        ))
    }
}

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
{
    pub fn new(graph: G) -> Self {
        Self {
            graph,
            _p: PhantomData,
        }
    }

    pub fn stack_core(&self, w: usize, d: usize) -> impl Iterator<Item = Path<N, E>> {
        [].into_iter()
    }

    pub fn normal_form<'a, B>(
        &'a self,
        builder: &mut B,
    ) -> LGraph<Memory<'a, N>, &'a E, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Item<&'a E>>,
    {
        todo!()
    }
}

impl<N, E, G> Graph<N, Item<E>> for LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
{
    fn start_node(&self) -> NodeRef<'_, N> {
        self.graph.start_node()
    }

    fn is_end_node(&self, node: NodeRef<'_, N>) -> bool {
        self.graph.is_end_node(node)
    }

    fn nodes(&self) -> Box<dyn Iterator<Item = NodeRef<'_, N>> + '_> {
        self.graph.nodes()
    }

    fn edges(&self) -> Box<dyn Iterator<Item = EdgeRef<'_, N, Item<E>>> + '_> {
        self.graph.edges()
    }
}
