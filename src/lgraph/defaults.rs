use super::lgraph_trait::{
    Bracket as BracketTrait, BracketIndex, BracketKind, BracketSet as BracketSetTrait, EdgeIndex,
    EdgeRef, LGraph as LGraphTrait, Label as LabelTrait, NodeIndex, NodeRef, TargetNode,
    Token as TokenTrait,
};
use std::{
    borrow::Borrow, collections::HashSet, fmt::Debug, hash::Hash, marker::PhantomData, str::FromStr,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Bracket {
    SquareOpen(usize),
    SquareClose(usize),
    AngleOpen(usize),
    AngleClose(usize),
}

impl LabelTrait for String {}
impl TokenTrait for char {}

impl BracketTrait for Bracket {
    fn is_opening(&self) -> bool {
        matches!(self, Bracket::SquareOpen(_) | Bracket::AngleOpen(_))
    }

    fn complete(&self) -> Option<Self> {
        match self {
            Bracket::SquareOpen(n) => Some(Self::SquareClose(*n)),
            Bracket::AngleOpen(n) => Some(Self::AngleClose(*n)),
            _ => None,
        }
    }

    fn kind(&self) -> BracketKind {
        match self {
            Bracket::SquareOpen(_) => 0,
            Bracket::SquareClose(_) => 0,
            Bracket::AngleOpen(_) => 1,
            Bracket::AngleClose(_) => 1,
        }
    }

    fn index(&self) -> BracketIndex {
        match self {
            Bracket::SquareOpen(n) => *n,
            Bracket::SquareClose(n) => *n,
            Bracket::AngleOpen(n) => *n,
            Bracket::AngleClose(n) => *n,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BracketParseError {
    InvalidBracket(char),
    InvalidIndex(String),
    EmptyString,
}
impl FromStr for Bracket {
    type Err = BracketParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = s.chars().next().ok_or(BracketParseError::EmptyString)?;
        let index = s[kind.len_utf8()..]
            .parse::<usize>()
            .map_err(|e| BracketParseError::InvalidIndex(e.to_string()))?;

        match kind {
            '[' => Ok(Self::SquareOpen(index)),
            ']' => Ok(Self::SquareClose(index)),
            '<' => Ok(Self::AngleOpen(index)),
            '>' => Ok(Self::AngleClose(index)),
            _ => Err(BracketParseError::InvalidBracket(kind)),
        }
    }
}
impl<'a> BracketSetTrait<'a, Bracket> for HashSet<Bracket> {
    type BracketsIter = std::collections::hash_set::Iter<'a, Bracket>;

    fn brackets(&'a self) -> Self::BracketsIter {
        self.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge<'a, T = char, B = Bracket, BS = HashSet<Bracket>> {
    from: NodeIndex,
    to: TargetNode,
    token: Option<T>,
    brackets: BS,
    _p: PhantomData<(B, &'a i32)>,
}

impl<'a, T, B, BS> Edge<'a, T, B, BS> {
    pub fn new(from: NodeIndex, token: Option<T>, brackets: BS, to: TargetNode) -> Self {
        Self {
            from,
            to,
            token,
            brackets,
            _p: PhantomData,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LGraph<'a, T = Option<char>, B = Bracket, L = String, BS = HashSet<Bracket>> {
    edges: Vec<Edge<'a, T, B, BS>>,
    node_names: Vec<L>,
}

impl<'a, T, B, L, BS> LGraph<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    pub fn new(edges: Vec<Edge<'a, T, B, BS>>, node_names: Vec<L>) -> Self {
        Self { edges, node_names }
    }
}

impl<'a, T, B, L, BS> LGraphTrait<'a, T, B, L> for LGraph<'a, T, B, L, BS>
where
    T: TokenTrait + 'a,
    L: LabelTrait + 'a,
    B: BracketTrait + 'a,
    BS: BracketSetTrait<'a, B> + 'a,
{
    type EdgeFromToIter = EdgeFromToIter<'a, T, B, L, BS>;
    type EdgeFromIter = EdgeFromIter<'a, T, B, L, BS>;
    type EdgeToIter = EdgeToIter<'a, T, B, L, BS>;
    type EdgeIter = EdgeIter<'a, T, B, L, BS>;
    type NodeIter = NodeIter<'a, T, B, L, BS>;
    type NodeWithLabelIter<'b, Q> =   NodeWithLabelIter<'a,'b,T,B,Q,L,BS>
    where
        Q: ?Sized + 'b,
        L: Borrow<Q> + PartialEq<Q>,
        'b: 'a,
        Self: 'a;
    type BracketSet = BS;

    fn edges_from_to(&'a self, from: NodeIndex, to: TargetNode) -> Self::EdgeFromToIter {
        EdgeFromToIter {
            graph: self,
            from,
            to,
            i: 0,
        }
    }
    fn edges_from(&'a self, from: NodeIndex) -> Self::EdgeFromIter {
        EdgeFromIter {
            graph: self,
            from,
            i: 0,
        }
    }
    fn edges_to(&'a self, to: TargetNode) -> Self::EdgeToIter {
        EdgeToIter {
            graph: self,
            to,
            i: 0,
        }
    }
    fn edges(&'a self) -> Self::EdgeIter {
        EdgeIter { graph: self, i: 0 }
    }

    fn edge_start(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges.get(edge).map(|e| e.from)
    }
    fn edge_dest(&self, edge: EdgeIndex) -> Option<TargetNode> {
        self.edges.get(edge).map(|e| e.to.clone())
    }
    fn edge_token(&self, edge: EdgeIndex) -> Option<&Option<T>> {
        self.edges.get(edge).map(|e| &e.token)
    }
    fn edge_brackets(&self, edge: EdgeIndex) -> Option<&Self::BracketSet> {
        self.edges.get(edge).map(|e| &e.brackets)
    }
    fn edge(&'a self, edge: EdgeIndex) -> Option<EdgeRef<T, B, Self::BracketSet>> {
        self.edges
            .get(edge)
            .map(|e| EdgeRef::new(edge, e.from, e.to.clone(), &e.brackets, &e.token))
    }

    fn nodes(&'a self) -> Self::NodeIter {
        NodeIter { graph: self, i: 0 }
    }
    fn nodes_with_label<'b, Q>(&'a self, label: &'b Q) -> Self::NodeWithLabelIter<'b, Q>
    where
        Q: ?Sized,
        'b: 'a,
        L: std::borrow::Borrow<Q> + PartialEq<Q>,
    {
        NodeWithLabelIter {
            graph: self,
            label,
            i: 0,
        }
    }

    fn node_label(&self, node: NodeIndex) -> Option<&L> {
        self.node_names.get(node)
    }
    fn node(&self, node: NodeIndex) -> Option<NodeRef<L>> {
        self.node_names
            .get(node)
            .map(|label| NodeRef { index: node, label })
    }
}

pub struct EdgeFromToIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    from: NodeIndex,
    to: TargetNode,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for EdgeFromToIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    type Item = EdgeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.edges[self.i..]
            .iter()
            .enumerate()
            .find(|(_, edge)| edge.from == self.from && edge.to == self.to)
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

pub struct EdgeFromIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    from: NodeIndex,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for EdgeFromIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    type Item = EdgeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.edges[self.i..]
            .iter()
            .enumerate()
            .find(|(_, edge)| edge.from == self.from)
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

pub struct EdgeToIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    to: TargetNode,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for EdgeToIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    type Item = EdgeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.edges[self.i..]
            .iter()
            .enumerate()
            .find(|(_, edge)| edge.to == self.to)
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

pub struct EdgeIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for EdgeIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    type Item = EdgeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.edges[self.i..]
            .iter()
            .enumerate()
            .next()
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

pub struct NodeIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for NodeIter<'a, T, B, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait,
    BS: BracketSetTrait<'a, B>,
{
    type Item = NodeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.node_names[self.i..]
            .iter()
            .enumerate()
            .next()
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

pub struct NodeWithLabelIter<'a, 'b, T, B, Q, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait + Borrow<Q> + PartialEq<Q>,
    BS: BracketSetTrait<'a, B>,
    Q: ?Sized + 'b,
{
    graph: &'a LGraph<'a, T, B, L, BS>,
    label: &'b Q,
    i: usize,
}
impl<'a, 'b, T, B, Q, L, BS> Iterator for NodeWithLabelIter<'a, 'b, T, B, Q, L, BS>
where
    T: TokenTrait,
    B: BracketTrait + 'a,
    L: LabelTrait + Borrow<Q> + PartialEq<Q>,
    BS: BracketSetTrait<'a, B>,
    Q: ?Sized + 'b,
{
    type Item = NodeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        self.graph.node_names[self.i..]
            .iter()
            .enumerate()
            .find(|(_, node)| node.eq(&self.label))
            .map(|(i, _)| {
                let res = i + self.i;
                self.i = res + 1;
                res
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lgraph() {
        let g = LGraph::new(
            vec![
                Edge::new(
                    0,
                    None,
                    HashSet::from([Bracket::AngleOpen(0)]),
                    TargetNode::Node(1),
                ),
                Edge::new(1, Some('a'), HashSet::new(), TargetNode::End),
            ],
            vec!["A".to_string(), "A".to_string()],
        );

        assert_eq!(g.edges().collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(g.edges_from(0).collect::<Vec<_>>(), vec![0]);
        assert_eq!(g.edges_to(TargetNode::End).collect::<Vec<_>>(), vec![1]);
        assert_eq!(
            g.edges_from_to(0, TargetNode::Node(1)).collect::<Vec<_>>(),
            vec![0]
        );

        assert_eq!(
            g.edge_brackets(0),
            Some(&HashSet::from([Bracket::AngleOpen(0)]))
        );
        assert_eq!(g.edge_brackets(1), Some(&HashSet::new()));
        assert_eq!(g.edge_token(0), Some(&None));
        assert_eq!(g.edge_token(1), Some(&Some('a')));

        assert_eq!(g.nodes().collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(g.nodes_with_label("A").collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(g.nodes_with_label("B").collect::<Vec<_>>(), vec![]);
    }
}
