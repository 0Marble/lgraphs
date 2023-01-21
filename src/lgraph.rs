use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
    ops::Index,
};

use crate::graph::{EdgeIndex, EdgeRef, Graph, NodeIndex, NodeRef};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Bracket<K> {
    kind: K,
    index: usize,
    is_opening: bool,
}

impl<K> Bracket<K> {
    pub fn new(kind: K, index: usize, is_opening: bool) -> Self {
        Self {
            kind,
            index,
            is_opening,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BracketSet<K> {
    set: HashSet<Bracket<K>>,
}

impl<K> BracketSet<K> {
    pub fn new(brackets: impl IntoIterator<Item = Bracket<K>>) -> Self
    where
        K: Eq + Hash,
    {
        let mut set = HashSet::new();
        for new_bracket in brackets {
            if !set.iter().any(|b: &Bracket<K>| b.kind == new_bracket.kind) {
                set.insert(new_bracket);
            }
        }

        Self { set }
    }
}
impl<K> IntoIterator for BracketSet<K> {
    type Item = Bracket<K>;

    type IntoIter = std::collections::hash_set::IntoIter<Bracket<K>>;

    fn into_iter(self) -> Self::IntoIter {
        self.set.into_iter()
    }
}
impl<'a, K> IntoIterator for &'a BracketSet<K> {
    type Item = &'a Bracket<K>;

    type IntoIter = std::collections::hash_set::Iter<'a, Bracket<K>>;

    fn into_iter(self) -> Self::IntoIter {
        self.set.iter()
    }
}

#[derive(Clone, Debug)]
pub struct BracketStack<K> {
    stacks: HashMap<K, VecDeque<usize>>,
}

impl<K> Hash for BracketStack<K>
where
    K: Eq + Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in self.stacks.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}
impl<K> Default for BracketStack<K> {
    fn default() -> Self {
        Self {
            stacks: HashMap::new(),
        }
    }
}
impl<K> PartialEq for BracketStack<K>
where
    K: PartialEq + Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.stacks == other.stacks
    }
}
impl<K> Eq for BracketStack<K> where K: PartialEq + Eq + Hash {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BracketStackError<K> {
    EmptyOnKind(K),
    Expected(Bracket<K>),
}

impl<K> BracketStack<K> {
    pub fn max_depth(&self) -> usize {
        self.stacks
            .values()
            .map(|s| s.len())
            .max()
            .unwrap_or_default()
    }

    pub fn can_accept(&self, set: &BracketSet<K>) -> Result<(), BracketStackError<K>>
    where
        K: Eq + Hash + Clone,
    {
        for bracket in set {
            if let Some(stack) = self.stacks.get(&bracket.kind) {
                if !bracket.is_opening {
                    match stack.front() {
                        Some(i) if *i != bracket.index => {
                            return Err(BracketStackError::Expected(Bracket::new(
                                bracket.kind.clone(),
                                *i,
                                false,
                            )))
                        }
                        None => return Err(BracketStackError::EmptyOnKind(bracket.kind.clone())),
                        _ => (),
                    }
                }
            }
        }

        Ok(())
    }

    fn accept(&mut self, set: BracketSet<K>)
    where
        K: Eq + Hash + Clone,
    {
        for bracket in set.into_iter() {
            let stack = self.stacks.entry(bracket.kind).or_default();
            if bracket.is_opening {
                stack.push_front(bracket.index)
            } else {
                stack.pop_front();
            }
        }
    }

    pub fn try_accept(&mut self, set: BracketSet<K>) -> Result<(), BracketStackError<K>>
    where
        K: Eq + Hash + Clone,
    {
        self.can_accept(&set)?;
        self.accept(set);

        Ok(())
    }

    fn completed(&self) -> bool {
        self.stacks.iter().all(|(_, stack)| stack.is_empty())
    }

    fn accept_and_copy(&self, set: BracketSet<K>) -> Option<Self>
    where
        K: Eq + Hash + Clone,
    {
        self.can_accept(&set).ok()?;
        let mut new_stack = self.clone();
        new_stack.accept(set);
        Some(new_stack)
    }
}

#[derive(Debug, Clone)]
pub struct LGraph<N, I, K> {
    graph: Graph<N, (Option<I>, BracketSet<K>)>,
    start_node: NodeIndex,
    end_nodes: HashSet<NodeIndex>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LGraphError<K> {
    StackError(BracketStackError<K>),
    NoWayToContinue,
    UnbalancedBrackets,
    InvalidEdge(EdgeIndex),
}
impl<K> From<BracketStackError<K>> for LGraphError<K> {
    fn from(e: BracketStackError<K>) -> Self {
        Self::StackError(e)
    }
}

#[derive(Debug)]
pub struct PathRef<'a, N, I, K> {
    graph: &'a LGraph<N, I, K>,
    edges: Vec<EdgeIndex>,
}

impl<'a, N, I, K> PathRef<'a, N, I, K> {
    pub fn new(graph: &'a LGraph<N, I, K>, edges: impl IntoIterator<Item = EdgeIndex>) -> Self {
        Self {
            edges: edges.into_iter().collect(),
            graph,
        }
    }

    pub fn empty(&self) -> bool {
        self.edges.is_empty()
    }

    pub fn edges<'b>(&'b self) -> Edges<'b, 'a, N, I, K> {
        Edges { path: self, cur: 0 }
    }

    fn push(mut self, edge: EdgeIndex) -> PathRef<'a, N, I, K> {
        self.edges.push(edge);
        self
    }
}

pub struct Edges<'a, 'b, N, I, K> {
    path: &'a PathRef<'b, N, I, K>,
    cur: usize,
}
impl<'a, 'b, N, I, K> Iterator for Edges<'a, 'b, N, I, K> {
    type Item = EdgeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        self.path.edges.get(self.cur).map(|e| {
            self.cur += 1;
            *e
        })
    }
}

impl<'a, N, I, K> Index<usize> for PathRef<'a, N, I, K> {
    type Output = EdgeIndex;

    fn index(&self, index: usize) -> &Self::Output {
        &self.edges[index]
    }
}

impl<'a, N, I, K> Clone for PathRef<'a, N, I, K> {
    fn clone(&self) -> Self {
        Self {
            graph: self.graph,
            edges: self.edges.clone(),
        }
    }
}

impl<'a, N, I, K> Hash for PathRef<'a, N, I, K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.edges.hash(state);
    }
}
impl<'a, N, I, K> PartialEq for PathRef<'a, N, I, K> {
    fn eq(&self, other: &Self) -> bool {
        self.edges == other.edges
    }
}
impl<'a, N, I, K> Eq for PathRef<'a, N, I, K> {}

impl<N, I, K> LGraph<N, I, K> {
    pub fn start_node(&self) -> NodeIndex {
        self.start_node
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.end_nodes.iter().cloned()
    }
    pub fn item(&self, edge: EdgeIndex) -> Option<&(Option<I>, BracketSet<K>)> {
        self.graph.item(edge)
    }
    pub fn node(&self, node: NodeIndex) -> Option<&N> {
        self.graph.node(node)
    }
    pub fn target(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.target(edge)
    }
    pub fn source(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.source(edge)
    }
    pub fn edge_ref(&self, edge: EdgeIndex) -> Option<EdgeRef<'_, N, (Option<I>, BracketSet<K>)>> {
        self.graph.edge_ref(edge)
    }
    pub fn node_ref(&self, node: NodeIndex) -> Option<NodeRef<'_, N>> {
        self.graph.node_ref(node)
    }
    pub fn nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.graph.nodes()
    }
    pub fn edges(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges()
    }
    pub fn edges_from(&self, node: NodeIndex) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges_from(node)
    }
    pub fn is_end_node(&self, node: NodeIndex) -> bool {
        self.end_nodes.contains(&node)
    }
    pub fn is_start_node(&self, node: NodeIndex) -> bool {
        self.start_node == node
    }

    pub fn new_unchecked(
        graph: Graph<N, (Option<I>, BracketSet<K>)>,
        start: N,
        end: impl IntoIterator<Item = N>,
    ) -> Self
    where
        N: Eq,
    {
        Self {
            start_node: graph
                .nodes()
                .flat_map(|n| graph.node_ref(n))
                .find(|n| n.contents().eq(&start))
                .map(|n| n.index())
                .unwrap(),
            end_nodes: end
                .into_iter()
                .map(|node| {
                    graph
                        .nodes()
                        .flat_map(|n| graph.node_ref(n))
                        .find(|n| n.contents().eq(&node))
                        .map(|n| n.index())
                        .unwrap()
                })
                .collect(),
            graph,
        }
    }

    pub fn is_in_core(
        &self,
        path: &PathRef<N, I, K>,
        w: usize,
        d: usize,
    ) -> Result<bool, LGraphError<K>>
    where
        K: Clone + Eq + Hash,
    {
        if path.empty() {
            return Ok(true);
        }

        let mut cur_stack: BracketStack<K> = BracketStack::default();
        let mut node_cycles = HashMap::from([((self.source(path[0]), cur_stack.clone()), 0)]);

        for edge in path.edges() {
            cur_stack.try_accept(
                self.item(edge)
                    .ok_or(LGraphError::InvalidEdge(edge))?
                    .1
                    .clone(),
            )?;
            let cur_depth = cur_stack.max_depth();
            if cur_depth > d + 1 {
                return Ok(false);
            };

            let cur_node = self.target(edge);
            let node_cycles_count = node_cycles
                .entry((cur_node, cur_stack.clone()))
                .and_modify(|node_cycles_count| *node_cycles_count += 1)
                .or_default();
            if *node_cycles_count > w {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub fn core(&self, w: usize, d: usize) -> Vec<PathRef<'_, N, I, K>>
    where
        K: Eq + Hash + Clone + Debug,
    {
        let mut state_stack = VecDeque::from([(
            PathRef::new(self, []),
            self.start_node(),
            BracketStack::<K>::default(),
        )]);
        let mut results = vec![];

        loop {
            let (path, node, brackets) = match state_stack.pop_front() {
                Some(t) => t,
                None => break,
            };
            if self.is_end_node(node) && brackets.completed() {
                results.push(path.clone());
            }

            for edge in self.edges_from(node) {
                if let Some(new_brackets) =
                    brackets.accept_and_copy(self.item(edge).unwrap().1.clone())
                {
                    let new_path = path.clone().push(edge);

                    if self.is_in_core(&new_path, w, d).unwrap() {
                        state_stack.push_front((new_path, self.target(edge).unwrap(), new_brackets))
                    }
                }
            }
        }

        results
    }

    fn direct_has(&self, e: EdgeIndex, item: Option<&I>) -> bool
    where
        I: Eq,
    {
        if let Some(edge) = self.edge_ref(e) {
            if edge.item().0.as_ref() == item {
                return true;
            }
        }

        false
    }

    pub fn traverse_iter<IT>(
        &self,
        items: impl IntoIterator<Item = I, IntoIter = IT>,
    ) -> LgraphTraverse<'_, N, K, I, IT>
    where
        IT: Iterator<Item = I>,
    {
        let mut it = items.into_iter();
        let first_item = it.next();
        LgraphTraverse {
            graph: self,
            cur_node: self.start_node(),
            item_iter: it,
            bracket_stack: BracketStack::default(),
            cur_item: first_item,
            finished: false,
        }
    }

    pub fn traverse(
        &self,
        items: impl IntoIterator<Item = I>,
    ) -> Result<PathRef<N, I, K>, LGraphError<K>>
    where
        I: Eq,
        K: Eq + Hash + Clone,
    {
        let path = self.traverse_iter(items).collect::<Result<Vec<_>, _>>()?;
        Ok(PathRef::new(self, path))
    }
}

pub struct LgraphTraverse<'a, L, K, I, IT> {
    graph: &'a LGraph<L, I, K>,
    cur_node: NodeIndex,
    item_iter: IT,
    bracket_stack: BracketStack<K>,
    cur_item: Option<I>,
    finished: bool,
}
impl<'a, L, K, I, IT> Iterator for LgraphTraverse<'a, L, K, I, IT>
where
    IT: Iterator<Item = I>,
    I: Eq,
    K: Eq + Hash + Clone,
{
    type Item = Result<EdgeIndex, LGraphError<K>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let mut chosen_edge = None;

        for edge in self
            .graph
            .graph
            .edges_from(self.cur_node)
            .flat_map(|e| self.graph.graph.edge_ref(e))
        {
            if self.graph.direct_has(edge.index(), self.cur_item.as_ref()) {
                match self.bracket_stack.try_accept(edge.item().1.clone()) {
                    Ok(_) => {
                        chosen_edge = Some(edge);
                    }
                    Err(e) => {
                        self.finished = true;
                        return Some(Err(e.into()));
                    }
                }
            }
        }

        let chosen_edge = if let Some(e) = chosen_edge {
            e
        } else {
            self.finished = true;
            return Some(Err(LGraphError::NoWayToContinue));
        };
        if chosen_edge.item().0.is_some() {
            self.cur_item = self.item_iter.next();
        }
        self.cur_node = chosen_edge.target_index();
        if self.graph.is_end_node(self.cur_node) && self.cur_item.is_none() {
            self.finished = true;
            if !self.bracket_stack.completed() {
                return Some(Err(LGraphError::UnbalancedBrackets));
            }
        }

        Some(Ok(chosen_edge.index()))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn example() -> LGraph<char, char, char> {
        let g = Graph::from_builder()
            .add_named_edge((
                '1',
                (Some('a'), BracketSet::new([Bracket::new('[', 1, true)])),
                '1',
            ))
            .add_named_edge((
                '1',
                (Some('d'), BracketSet::new([Bracket::new('[', 2, true)])),
                '2',
            ))
            .add_named_edge((
                '2',
                (Some('b'), BracketSet::new([Bracket::new('[', 2, false)])),
                '2',
            ))
            .add_named_edge((
                '2',
                (Some('c'), BracketSet::new([Bracket::new('[', 3, true)])),
                '2',
            ))
            .add_named_edge((
                '2',
                (Some('d'), BracketSet::new([Bracket::new('[', 3, false)])),
                '3',
            ))
            .add_named_edge((
                '3',
                (Some('a'), BracketSet::new([Bracket::new('[', 1, false)])),
                '3',
            ))
            .build();

        LGraph::new_unchecked(g, '1', ['3'])
    }

    #[test]
    fn in_core() {
        let g = example();
        let p0 = PathRef::new(
            &g,
            [0, 1, 2, 3, 4, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        assert!(g.is_in_core(&p0, 1, 1).expect("Error"));
        assert!(g.is_in_core(&p0, 1, 2).expect("Error"));

        let p1 = PathRef::new(&g, [1, 2, 3, 4].into_iter().flat_map(|i| g.edges().nth(i)));
        assert!(g.is_in_core(&p1, 1, 1).expect("Error"));
        assert!(g.is_in_core(&p1, 1, 2).expect("Error"));

        let p2 = PathRef::new(
            &g,
            [0, 0, 1, 2, 3, 4, 5, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        assert!(!g.is_in_core(&p2, 1, 1).expect("Error"));
        assert!(g.is_in_core(&p2, 1, 2).expect("Error"));
    }

    #[test]
    fn core() {
        let g = example();
        let core11 = HashSet::from_iter(g.core(1, 1).into_iter());
        let core12 = HashSet::from_iter(g.core(1, 2).into_iter());

        let p0 = PathRef::new(
            &g,
            [0, 1, 2, 3, 4, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        let p1 = PathRef::new(&g, [1, 2, 3, 4].into_iter().flat_map(|i| g.edges().nth(i)));
        let p2 = PathRef::new(
            &g,
            [0, 0, 1, 2, 3, 4, 5, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        let actual_core11 = HashSet::from([p0.clone(), p1.clone()]);
        let actual_core12 = HashSet::from([p0, p1, p2]);

        assert_eq!(core11, actual_core11);
        assert_eq!(core12, actual_core12);
    }

    #[test]
    fn traverse() {
        let g = example();
        let s = "aadbcdaa";
        let p = PathRef::new(
            &g,
            [0, 0, 1, 2, 3, 4, 5, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );

        assert_eq!(g.traverse(s.chars()), Ok(p));
        assert!(g.traverse("dbcd".chars()).is_ok());
        assert!(g.traverse("adbcda".chars()).is_ok());
        assert!(g.traverse("aadbcda".chars()).is_err());
    }
}
