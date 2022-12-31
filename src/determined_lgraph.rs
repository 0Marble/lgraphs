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
            .map(Some)
            .chain((0..).map(|_| None))
            .enumerate()
        {
            consumed_tokens_count = i;
            loop {
                let taken_edge = self
                    .edges_from(cur_node)
                    .filter_map(|e| self.edge(e))
                    .filter(|e| e.token.as_ref() == token)
                    .chain(
                        self.edges_from(cur_node)
                            .filter_map(|e| self.edge(e))
                            .filter(|e| e.token.is_none()),
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

                if taken_edge.token.as_ref() == token {
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

    use std::str::FromStr;

    use super::DeterminedLGraph;
    use crate::{determined_lgraph::TraversalPath, lgraph::defaults::*};

    #[test]
    fn traverse_complicated() {
        let s = "
            0 () {[0}    (1)
            1 () {]0,<1} (1)
            1 () {>1,[1} (1)
            1 () {>0}    (3)
            1 () {]1,<0} (2)
            2 () {}      (3)
            3 () {>0}    ()
        ";

        let g: LGraph<char, Bracket, String> = LGraph::from_str(s).unwrap();
        let g = DeterminedLGraph::new_unchecked(g);
        dbg!(&g);
        let t = g.traverse("".chars().collect::<Vec<_>>().as_slice());
        assert_eq!(t, Ok(TraversalPath::new(vec![0, 1, 2, 4, 5, 6])));
    }

    #[test]
    fn traverse_single() {
        let s = "
            0 (a) {[0} (0)
            0 (b) {}   (1)
            1 (a) {]0} (1)
            1 ()  {}   ()
        ";
        let g: LGraph<char, Bracket, String> = LGraph::from_str(s).unwrap();
        let g = DeterminedLGraph::new_unchecked(g);
        assert_eq!(
            g.traverse("aba".chars().collect::<Vec<_>>().as_slice()),
            Ok(TraversalPath::new(vec![0, 1, 2, 3]))
        );
        assert_eq!(
            g.traverse("aaabaaa".chars().collect::<Vec<_>>().as_slice()),
            Ok(TraversalPath::new(vec![0, 0, 0, 1, 2, 2, 2, 3]))
        );
        assert!(g
            .traverse("aaabaa".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("a".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("".chars().collect::<Vec<_>>().as_slice())
            .is_err());
    }

    #[test]
    fn traverse_multi() {
        let s = "
            1 (a) {[0}    (1)
            1 (b) {]0,<0} (2)
            2 (b) {<0,]0} (2)
            2 (c) {>0}    (3)
            3 (c) {>0}    (3)
            3 ()  {}      ()
        ";
        let g: LGraph<char, Bracket, String> = LGraph::from_str(s).unwrap();
        let g = DeterminedLGraph::new_unchecked(g);
        assert_eq!(
            g.traverse("abc".chars().collect::<Vec<_>>().as_slice()),
            Ok(TraversalPath::new(vec![0, 1, 3, 5]))
        );
        assert_eq!(
            g.traverse("aaabbbccc".chars().collect::<Vec<_>>().as_slice()),
            Ok(TraversalPath::new(vec![0, 0, 0, 1, 2, 2, 3, 4, 4, 5]))
        );
        assert!(g
            .traverse("aaabbbcc".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("aaabbccc".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("aabbbccc".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("a".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("".chars().collect::<Vec<_>>().as_slice())
            .is_err());
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
    fn edge_token(&self, edge: EdgeIndex) -> Option<&Option<T>> {
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
