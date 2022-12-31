use std::{borrow::Borrow, fmt::Debug, marker::PhantomData};
pub type EdgeIndex = usize;
pub type NodeIndex = usize;
pub type BracketKind = usize;
pub type BracketIndex = usize;

pub trait Token: Eq + Debug + Clone {}
pub trait Bracket: Sized + Eq + Debug + Clone {
    fn is_opening(&self) -> bool;
    fn is_closing(&self) -> bool {
        !self.is_opening()
    }
    fn complete(&self) -> Option<Self>;
    fn is_closed_by(&self, close: &Self) -> bool {
        self.is_opening()
            && close.is_closing()
            && self.kind() == close.kind()
            && self.index() == close.index()
    }
    fn kind(&self) -> BracketKind;
    fn index(&self) -> BracketIndex;
}
pub trait Label: Eq + Debug + Clone {}
pub trait BracketSet<'a, B>: Debug + Clone
where
    B: Bracket + 'a,
{
    type BracketsIter: Iterator<Item = &'a B>;
    fn brackets(&'a self) -> Self::BracketsIter;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetNode {
    End,
    Node(NodeIndex),
}

#[derive(Debug, Clone)]
pub struct EdgeRef<'a, T, B, BS>
where
    T: Token,
    B: Bracket + 'a,
    BS: BracketSet<'a, B>,
{
    pub index: EdgeIndex,
    pub from: NodeIndex,
    pub to: TargetNode,
    pub brackets: &'a BS,
    pub token: &'a Option<T>,
    _p: PhantomData<B>,
}

impl<'a, T, B, BS> EdgeRef<'a, T, B, BS>
where
    T: Token,
    B: Bracket + 'a,
    BS: BracketSet<'a, B>,
{
    pub fn new(
        index: EdgeIndex,
        from: NodeIndex,
        to: TargetNode,
        barckets: &'a BS,
        token: &'a Option<T>,
    ) -> Self {
        Self {
            index,
            from,
            to,
            brackets: barckets,
            token,
            _p: PhantomData,
        }
    }
}

pub struct NodeRef<'a, L>
where
    L: Label,
{
    pub index: NodeIndex,
    pub label: &'a L,
}

pub trait LGraph<'a, T, B, L>: Debug + Clone
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
{
    type EdgeFromToIter: Iterator<Item = EdgeIndex> + 'a;
    type EdgeFromIter: Iterator<Item = EdgeIndex> + 'a;
    type EdgeToIter: Iterator<Item = EdgeIndex> + 'a;
    type EdgeIter: Iterator<Item = EdgeIndex> + 'a;
    type NodeIter: Iterator<Item = NodeIndex> + 'a;
    type NodeWithLabelIter<'b, Q>: Iterator<Item = NodeIndex> + 'a
    where
        Q: ?Sized + 'b,
        L: Borrow<Q> + PartialEq<Q>,
        'b: 'a,
        Self: 'a;
    type BracketSet: BracketSet<'a, B>;

    fn edges_from_to(&'a self, from: NodeIndex, to: TargetNode) -> Self::EdgeFromToIter;
    fn edges_from(&'a self, node: NodeIndex) -> Self::EdgeFromIter;
    fn edges_to(&'a self, node: TargetNode) -> Self::EdgeToIter;
    fn edges(&'a self) -> Self::EdgeIter;

    fn edge_start(&self, edge: EdgeIndex) -> Option<NodeIndex>;
    fn edge_dest(&self, edge: EdgeIndex) -> Option<TargetNode>;
    fn edge_token(&self, edge: EdgeIndex) -> Option<&Option<T>>;
    fn edge_brackets(&self, edge: EdgeIndex) -> Option<&Self::BracketSet>;
    fn edge(&'a self, edge: EdgeIndex) -> Option<EdgeRef<T, B, Self::BracketSet>>;

    fn nodes(&'a self) -> Self::NodeIter;
    fn nodes_with_label<'b, Q>(&'a self, label: &'b Q) -> Self::NodeWithLabelIter<'b, Q>
    where
        Q: ?Sized,
        'b: 'a,
        L: Borrow<Q> + PartialEq<Q>;

    fn node_label(&self, node: NodeIndex) -> Option<&L>;
    fn node(&self, node: NodeIndex) -> Option<NodeRef<L>>;
}
