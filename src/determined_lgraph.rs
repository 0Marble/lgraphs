use std::{collections::HashMap, marker::PhantomData};

use crate::lgraph::lgraph_trait::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeterminedLGraphError<T, B> {
    NoWayToContinue {
        from_node: NodeIndex,
        string_left: Vec<T>,
    },
    CouldNotFinish {
        string_left: Vec<T>,
        brackets_left: HashMap<BracketKind, Vec<B>>,
        last_node: NodeIndex,
    },
}

#[derive(Debug, Clone)]
struct BracketStack<B: Bracket>(HashMap<BracketKind, Vec<B>>);

impl<B: Bracket> BracketStack<B> {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn try_accept<'a, BS: BracketSet<'a, B>>(&mut self, set: &'a BS) -> bool
    where
        B: 'a,
    {
        let all_accepted = set.brackets().all(|b| {
            let top = self.0.entry(b.kind()).or_default().last();
            b.is_opening() || top.map_or(false, |top| top.is_closed_by(b))
        });

        if !all_accepted {
            return false;
        }

        set.brackets().for_each(|b| {
            if b.is_opening() {
                self.0.entry(b.kind()).or_default().push(b.clone());
            } else {
                self.0.entry(b.kind()).or_default().pop();
            }
        });
        true
    }

    fn is_empty(&self) -> bool {
        self.0.iter().all(|(_, s)| s.is_empty())
    }
}

#[derive(Debug, Clone)]
pub struct DeterminedLGraph<'a, G, T, B, L, BS>
where
    G: LGraph<'a, T, B, L>,
    Self: 'a,
    T: Token,
    L: Label,
    B: Bracket + 'a,
    BS: BracketSet<'a, B>,
{
    graph: G,
    _p: PhantomData<(&'a T, B, L, BS)>,
}

impl<'a, G, T, B, L, BS> DeterminedLGraph<'a, G, T, B, L, BS>
where
    G: LGraph<'a, T, B, L, BracketSet = BS>,
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    pub fn new_unchecked(graph: G) -> Self {
        Self {
            graph,
            _p: PhantomData,
        }
    }

    pub fn traverse(&'a self, string: &[T]) -> Result<TraversalPath, DeterminedLGraphError<T, B>> {
        let mut cur_node = 0;
        let mut bracket_stack = BracketStack::new();
        let mut consumed_tokens_count = 0;
        let mut path = Vec::new();

        'token_iter: for (i, token) in string
            .iter()
            .cloned()
            .chain((0..).map(|_| T::new_empty()))
            .enumerate()
        {
            consumed_tokens_count = i;
            loop {
                let taken_edge = self
                    .edges_from(cur_node)
                    .filter_map(|e| self.edge(e))
                    .filter(|e| e.token == &token)
                    .chain(
                        self.edges_from(cur_node)
                            .filter_map(|e| self.edge(e))
                            .filter(|e| e.token.is_empty()),
                    )
                    .find(|e| bracket_stack.try_accept(e.brackets))
                    .ok_or_else(|| DeterminedLGraphError::NoWayToContinue {
                        from_node: cur_node,
                        string_left: string[consumed_tokens_count..].to_vec(),
                    })?;

                path.push(taken_edge.index);
                match taken_edge.to {
                    TargetNode::End => {
                        break 'token_iter;
                    }
                    TargetNode::Node(next) => cur_node = next,
                }

                if taken_edge.token == &token {
                    break;
                }
            }
            consumed_tokens_count += 1;
        }

        if !bracket_stack.is_empty() || consumed_tokens_count < string.len() {
            return Err(DeterminedLGraphError::CouldNotFinish {
                string_left: string[consumed_tokens_count..].to_vec(),
                brackets_left: bracket_stack.0,
                last_node: cur_node,
            });
        }

        Ok(TraversalPath { path })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TraversalPath {
    path: Vec<EdgeIndex>,
}

impl TraversalPath {
    pub fn new(path: Vec<EdgeIndex>) -> Self {
        Self { path }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::lgraph::{defaults::DefaultToken, *};

    #[test]
    fn traverse_single_bracket() {
        let g = DeterminedLGraph::new_unchecked(DefaultLGraph::new(
            vec![
                DefaultEdge::new(
                    0,
                    DefaultToken(Some('a')),
                    HashSet::from([DefaultBracket::SquareOpen(0)]),
                    TargetNode::Node(0),
                ),
                DefaultEdge::new(
                    0,
                    DefaultToken(Some('b')),
                    HashSet::new(),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken(Some('c')),
                    HashSet::from([DefaultBracket::SquareClose(0)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(1, DefaultToken(None), HashSet::new(), TargetNode::End),
            ],
            vec!["A".to_string(), "B".to_string()],
        ));

        assert_eq!(
            g.traverse(
                "b".chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![1, 3]))
        );
        assert_eq!(
            g.traverse(
                "abc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![0, 1, 2, 3]))
        );
        assert_eq!(
            g.traverse(
                "aabcc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![0, 0, 1, 2, 2, 3]))
        );
        assert_eq!(
            g.traverse(
                "aaaaaaaaabccccccccc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3
            ]))
        );
        assert!(g
            .traverse(
                "aabc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err());
        assert!(g
            .traverse(
                "bc".chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err());
    }

    #[test]
    fn traverse_multi_bracket() {
        let g = DeterminedLGraph::new_unchecked(DefaultLGraph::new(
            vec![
                DefaultEdge::new(
                    0,
                    DefaultToken(Some('a')),
                    HashSet::from([DefaultBracket::SquareOpen(0)]),
                    TargetNode::Node(0),
                ),
                DefaultEdge::new(
                    0,
                    DefaultToken(Some('b')),
                    HashSet::from([DefaultBracket::SquareClose(0), DefaultBracket::AngleOpen(0)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken(Some('b')),
                    HashSet::from([DefaultBracket::SquareClose(0), DefaultBracket::AngleOpen(0)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken(Some('c')),
                    HashSet::from([DefaultBracket::AngleClose(0)]),
                    TargetNode::Node(2),
                ),
                DefaultEdge::new(
                    2,
                    DefaultToken(Some('c')),
                    HashSet::from([DefaultBracket::AngleClose(0)]),
                    TargetNode::Node(2),
                ),
                DefaultEdge::new(2, DefaultToken(None), HashSet::new(), TargetNode::End),
            ],
            vec!["1".to_string(), "2".to_string(), "3".to_string()],
        ));

        assert_eq!(
            g.traverse(
                "abc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![0, 1, 3, 5]))
        );

        assert_eq!(
            g.traverse(
                "aabbcc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![0, 0, 1, 2, 3, 4, 5]))
        );
        assert_eq!(
            g.traverse(
                "aaaaabbbbbccccc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok(TraversalPath::new(vec![
                0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 3, 4, 4, 4, 4, 5
            ]))
        );
        assert!(g
            .traverse(
                "aabc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err());
        assert!(g
            .traverse(
                "abbc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err());
        assert!(g
            .traverse(
                "abcc"
                    .chars()
                    .map(|c| DefaultToken(Some(c)))
                    .collect::<Vec<_>>()
                    .as_slice()
            )
            .is_err());
    }

    #[test]
    fn traverse_evil_empty() {
        let g = DeterminedLGraph::new_unchecked(DefaultLGraph::new(
            vec![
                DefaultEdge::new(
                    0,
                    DefaultToken::new_empty(),
                    HashSet::from([DefaultBracket::SquareOpen(0)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken::new_empty(),
                    HashSet::from([DefaultBracket::SquareClose(0), DefaultBracket::AngleOpen(1)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken::new_empty(),
                    HashSet::from([DefaultBracket::AngleClose(1), DefaultBracket::SquareOpen(1)]),
                    TargetNode::Node(1),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken::new_empty(),
                    HashSet::from([DefaultBracket::SquareClose(1)]),
                    TargetNode::Node(2),
                ),
                DefaultEdge::new(
                    1,
                    DefaultToken::new_empty(),
                    HashSet::from([DefaultBracket::AngleClose(0)]),
                    TargetNode::Node(3),
                ),
                DefaultEdge::new(
                    2,
                    DefaultToken::new_empty(),
                    HashSet::from([]),
                    TargetNode::Node(3),
                ),
                DefaultEdge::new(
                    3,
                    DefaultToken::new_empty(),
                    HashSet::from([]),
                    TargetNode::End,
                ),
            ],
            vec![
                "0".to_string(),
                "1".to_string(),
                "2".to_string(),
                "3".to_string(),
            ],
        ));

        assert_eq!(
            g.traverse(&[]),
            Ok(TraversalPath::new(vec![0, 1, 2, 3, 5, 6]))
        );
    }
}

impl<'a, G, T, B, L, BS> LGraph<'a, T, B, L> for DeterminedLGraph<'a, G, T, B, L, BS>
where
    G: LGraph<'a, T, B, L, BracketSet = BS>,
    T: Token,
    B: Bracket + 'a,
    L: Label,
    BS: BracketSet<'a, B>,
{
    type EdgeFromToIter = G::EdgeFromToIter;
    type EdgeFromIter = G::EdgeFromIter;
    type EdgeToIter = G::EdgeToIter;
    type EdgeIter = G::EdgeIter;
    type NodeIter = G::NodeIter;
    type NodeWithLabelIter<'b, Q> = G::NodeWithLabelIter<'b,Q>
    where
        Q: ?Sized + 'b,
        L: std::borrow::Borrow<Q> + PartialEq<Q>,
        'b: 'a,
        Self: 'a;
    type BracketSet = BS;

    fn edges_from_to(&'a self, from: NodeIndex, to: TargetNode) -> Self::EdgeFromToIter {
        self.graph.edges_from_to(from, to)
    }
    fn edges_from(&'a self, node: NodeIndex) -> Self::EdgeFromIter {
        self.graph.edges_from(node)
    }
    fn edges_to(&'a self, node: TargetNode) -> Self::EdgeToIter {
        self.graph.edges_to(node)
    }
    fn edges(&'a self) -> Self::EdgeIter {
        self.graph.edges()
    }
    fn edge_start(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.edge_start(edge)
    }
    fn edge_dest(&self, edge: EdgeIndex) -> Option<TargetNode> {
        self.graph.edge_dest(edge)
    }
    fn edge_token(&self, edge: EdgeIndex) -> Option<&T> {
        self.graph.edge_token(edge)
    }
    fn edge_brackets(&self, edge: EdgeIndex) -> Option<&Self::BracketSet> {
        self.graph.edge_brackets(edge)
    }
    fn edge(&'a self, edge_index: EdgeIndex) -> Option<EdgeRef<T, B, Self::BracketSet>> {
        self.graph.edge(edge_index)
    }
    fn nodes(&'a self) -> Self::NodeIter {
        self.graph.nodes()
    }
    fn nodes_with_label<'b, Q>(&'a self, label: &'b Q) -> Self::NodeWithLabelIter<'b, Q>
    where
        Q: ?Sized,
        'b: 'a,
        L: std::borrow::Borrow<Q> + PartialEq<Q>,
    {
        self.graph.nodes_with_label(label)
    }
    fn node_label(&self, node: NodeIndex) -> Option<&L> {
        self.graph.node_label(node)
    }
    fn node(&self, node: NodeIndex) -> Option<NodeRef<L>> {
        self.graph.node(node)
    }
}
