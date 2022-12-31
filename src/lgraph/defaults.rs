use super::lgraph_trait::*;
use std::{
    borrow::Borrow, collections::HashSet, fmt::Debug, hash::Hash, marker::PhantomData, str::FromStr,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DefaultBracket {
    SquareOpen(usize),
    SquareClose(usize),
    AngleOpen(usize),
    AngleClose(usize),
}

impl Label for String {}
impl Bracket for DefaultBracket {
    fn is_opening(&self) -> bool {
        matches!(
            self,
            DefaultBracket::SquareOpen(_) | DefaultBracket::AngleOpen(_)
        )
    }

    fn complete(&self) -> Option<Self> {
        match self {
            DefaultBracket::SquareOpen(n) => Some(Self::SquareClose(*n)),
            DefaultBracket::AngleOpen(n) => Some(Self::AngleClose(*n)),
            _ => None,
        }
    }

    fn kind(&self) -> BracketKind {
        match self {
            DefaultBracket::SquareOpen(_) => 0,
            DefaultBracket::SquareClose(_) => 0,
            DefaultBracket::AngleOpen(_) => 1,
            DefaultBracket::AngleClose(_) => 1,
        }
    }

    fn index(&self) -> BracketIndex {
        match self {
            DefaultBracket::SquareOpen(n) => *n,
            DefaultBracket::SquareClose(n) => *n,
            DefaultBracket::AngleOpen(n) => *n,
            DefaultBracket::AngleClose(n) => *n,
        }
    }
}
pub enum DefaultBracketParseError {
    InvalidBracket(char),
    InvalidIndex(String),
    EmptyString,
}
impl FromStr for DefaultBracket {
    type Err = DefaultBracketParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let kind = s
            .chars()
            .next()
            .ok_or(DefaultBracketParseError::EmptyString)?;
        let index = s[kind.len_utf8()..]
            .parse::<usize>()
            .map_err(|e| DefaultBracketParseError::InvalidIndex(e.to_string()))?;

        match kind {
            '[' => Ok(Self::SquareOpen(index)),
            ']' => Ok(Self::SquareClose(index)),
            '<' => Ok(Self::AngleOpen(index)),
            '>' => Ok(Self::AngleClose(index)),
            _ => Err(DefaultBracketParseError::InvalidBracket(kind)),
        }
    }
}
impl<'a> BracketSet<'a, DefaultBracket> for HashSet<DefaultBracket> {
    type BracketsIter = std::collections::hash_set::Iter<'a, DefaultBracket>;

    fn brackets(&'a self) -> Self::BracketsIter {
        self.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultToken(pub Option<char>);
impl Token for DefaultToken {
    fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    fn new_empty() -> Self {
        Self(None)
    }
}
pub enum DefaultTokenParseError {
    EmptyString,
    InvalidCharCount,
}
impl FromStr for DefaultToken {
    type Err = DefaultTokenParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.chars();
        let first = it.next().ok_or(DefaultTokenParseError::EmptyString)?;
        if first == '(' {
            if it.next().map_or(false, |t| t == ')') && it.next().is_none() {
                Ok(DefaultToken(None))
            } else {
                Err(DefaultTokenParseError::InvalidCharCount)
            }
        } else if it.next().is_some() {
            Err(DefaultTokenParseError::InvalidCharCount)
        } else {
            Ok(DefaultToken(Some(first)))
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefaultEdge<'a, T = DefaultToken, B = DefaultBracket, BS = HashSet<DefaultBracket>> {
    from: NodeIndex,
    to: TargetNode,
    token: T,
    brackets: BS,
    _p: PhantomData<(B, &'a i32)>,
}

impl<'a, T, B, BS> DefaultEdge<'a, T, B, BS> {
    pub fn new(from: NodeIndex, token: T, brackets: BS, to: TargetNode) -> Self {
        Self {
            from,
            to,
            token,
            brackets,
            _p: PhantomData,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefaultLGraph<
    'a,
    T = Option<char>,
    B = DefaultBracket,
    L = String,
    BS = HashSet<DefaultBracket>,
> {
    edges: Vec<DefaultEdge<'a, T, B, BS>>,
    node_names: Vec<L>,
}

impl<'a, T, B, L, BS> DefaultLGraph<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    pub fn new(edges: Vec<DefaultEdge<'a, T, B, BS>>, node_names: Vec<L>) -> Self {
        Self { edges, node_names }
    }
}

impl<'a, T, B, L, BS> LGraph<'a, T, B, L> for DefaultLGraph<'a, T, B, L, BS>
where
    T: Token + 'a,
    L: Label + 'a,
    B: Bracket + 'a,
    BS: BracketSet<'a, B> + 'a,
{
    type EdgeFromToIter = DefaultEdgeFromToIter<'a, T, B, L, BS>;
    type EdgeFromIter = DefaultEdgeFromIter<'a, T, B, L, BS>;
    type EdgeToIter = DefaultEdgeToIter<'a, T, B, L, BS>;
    type EdgeIter = DefaultEdgeIter<'a, T, B, L, BS>;
    type NodeIter = DefaultNodeIter<'a, T, B, L, BS>;
    type NodeWithLabelIter<'b, Q> = DefaultNodeWithLabelIter<'a,'b,T,B,Q,L,BS>
    where
        Q: ?Sized + 'b,
        L: Borrow<Q> + PartialEq<Q>,
        'b: 'a,
        Self: 'a;
    type BracketSet = BS;

    fn edges_from_to(&'a self, from: NodeIndex, to: TargetNode) -> Self::EdgeFromToIter {
        DefaultEdgeFromToIter {
            graph: self,
            from,
            to,
            i: 0,
        }
    }
    fn edges_from(&'a self, from: NodeIndex) -> Self::EdgeFromIter {
        DefaultEdgeFromIter {
            graph: self,
            from,
            i: 0,
        }
    }
    fn edges_to(&'a self, to: TargetNode) -> Self::EdgeToIter {
        DefaultEdgeToIter {
            graph: self,
            to,
            i: 0,
        }
    }
    fn edges(&'a self) -> Self::EdgeIter {
        DefaultEdgeIter { graph: self, i: 0 }
    }

    fn edge_start(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.edges.get(edge).map(|e| e.from)
    }
    fn edge_dest(&self, edge: EdgeIndex) -> Option<TargetNode> {
        self.edges.get(edge).map(|e| e.to.clone())
    }
    fn edge_token(&self, edge: EdgeIndex) -> Option<&T> {
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
        DefaultNodeIter { graph: self, i: 0 }
    }
    fn nodes_with_label<'b, Q>(&'a self, label: &'b Q) -> Self::NodeWithLabelIter<'b, Q>
    where
        Q: ?Sized,
        'b: 'a,
        L: std::borrow::Borrow<Q> + PartialEq<Q>,
    {
        DefaultNodeWithLabelIter {
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

pub struct DefaultEdgeFromToIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    from: NodeIndex,
    to: TargetNode,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for DefaultEdgeFromToIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
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

pub struct DefaultEdgeFromIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    from: NodeIndex,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for DefaultEdgeFromIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
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

pub struct DefaultEdgeToIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    to: TargetNode,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for DefaultEdgeToIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
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

pub struct DefaultEdgeIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for DefaultEdgeIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
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

pub struct DefaultNodeIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    i: usize,
}
impl<'a, T, B, L, BS> Iterator for DefaultNodeIter<'a, T, B, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
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

pub struct DefaultNodeWithLabelIter<'a, 'b, T, B, Q, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label + Borrow<Q> + PartialEq<Q>,
    Q: ?Sized + 'b,
    BS: BracketSet<'a, B>,
{
    graph: &'a DefaultLGraph<'a, T, B, L, BS>,
    label: &'b Q,
    i: usize,
}
impl<'a, 'b, T, B, Q, L, BS> Iterator for DefaultNodeWithLabelIter<'a, 'b, T, B, Q, L, BS>
where
    T: Token,
    B: Bracket + 'a,
    L: Label + Borrow<Q> + PartialEq<Q>,
    Q: ?Sized + 'b,
    BS: BracketSet<'a, B>,
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
    fn default_lgraph() {
        let g = DefaultLGraph::new(
            vec![
                DefaultEdge::new(
                    0,
                    DefaultToken(None),
                    HashSet::from([DefaultBracket::AngleOpen(0)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(1, DefaultToken(Some('a')), HashSet::new(), TargetNode::End),
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
            Some(&HashSet::from([DefaultBracket::AngleOpen(0)]))
        );
        assert_eq!(g.edge_brackets(1), Some(&HashSet::new()));
        assert_eq!(g.edge_token(0), Some(&DefaultToken(None)));
        assert_eq!(g.edge_token(1), Some(&DefaultToken(Some('a'))));

        assert_eq!(g.nodes().collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(g.nodes_with_label("A").collect::<Vec<_>>(), vec![0, 1]);
        assert_eq!(g.nodes_with_label("B").collect::<Vec<_>>(), vec![]);
    }
}
