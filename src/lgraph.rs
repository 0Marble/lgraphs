use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
    ops::Index,
};

use crate::{
    graph::{
        EdgeIndex, EdgeRef, Graph, GraphBuilder, MangledBuilder, MangledNode, NodeIndex, NodeRef,
        PathRef, ReachableFrom,
    },
    state_machine,
};

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
    stacks: HashMap<K, Vec<usize>>,
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
                    match stack.last() {
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
                stack.push(bracket.index)
            } else {
                stack.pop();
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

    pub fn empty(&self) -> bool {
        self.stacks.iter().all(|(_, stack)| stack.is_empty())
    }

    pub fn remove_suffix(&self, b: &BracketStack<K>) -> Option<BracketStack<K>>
    where
        K: Eq + Hash + Clone,
    {
        let mut new_stacks = HashMap::new();
        for (kind, stack) in &self.stacks {
            let suffix_stack = b.stacks.get(kind)?;
            let prefix = stack.strip_suffix(suffix_stack.as_slice())?;
            new_stacks.insert(kind, prefix);
        }

        Some(BracketStack {
            stacks: new_stacks
                .into_iter()
                .map(|(kind, stack)| (kind.clone(), stack.to_vec()))
                .collect(),
        })
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
    NoNodesGiven,
}
impl<K> From<BracketStackError<K>> for LGraphError<K> {
    fn from(e: BracketStackError<K>) -> Self {
        Self::StackError(e)
    }
}

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
    pub fn node_with(&self, node: &N) -> Option<NodeIndex>
    where
        N: Eq,
    {
        self.graph.node_with(node)
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
    pub fn reachable_from(&self, node: NodeIndex) -> ReachableFrom<N, (Option<I>, BracketSet<K>)> {
        self.graph.reachable_from(node)
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
        path: &PathRef<N, (Option<I>, BracketSet<K>)>,
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

    pub fn core(&self, w: usize, d: usize) -> Vec<PathRef<'_, N, (Option<I>, BracketSet<K>)>>
    where
        K: Eq + Hash + Clone + Debug,
    {
        let mut state_stack = VecDeque::from([(
            PathRef::new(&self.graph, []),
            self.start_node(),
            BracketStack::<K>::default(),
        )]);
        let mut results = vec![];

        loop {
            let (path, node, brackets) = match state_stack.pop_front() {
                Some(t) => t,
                None => break,
            };
            if self.is_end_node(node) && brackets.empty() {
                results.push(path.clone());
            }

            for edge in self.edges_from(node) {
                if brackets
                    .can_accept(&self.item(edge).unwrap().1)
                    .map_or(false, |_| true)
                {
                    let mut new_brackets = brackets.clone();
                    new_brackets.accept(self.item(edge).unwrap().1.clone());
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
    ) -> Result<PathRef<N, (Option<I>, BracketSet<K>)>, LGraphError<K>>
    where
        I: Eq,
        K: Eq + Hash + Clone,
    {
        let path = self.traverse_iter(items).collect::<Result<Vec<_>, _>>()?;
        Ok(PathRef::new(&self.graph, path))
    }

    fn graph_from_paths<'a>(
        paths: impl IntoIterator<Item = PathRef<'a, N, (Option<I>, BracketSet<K>)>>,
    ) -> Result<LGraph<(NodeIndex, BracketStack<K>), &'a I, K>, LGraphError<K>>
    where
        N: 'a + Eq,
        I: 'a + Clone,
        K: 'a + Eq + Hash + Clone,
    {
        let mut builder = Graph::from_builder();
        let mut start_node = None;
        let mut end_nodes = HashSet::new();
        for path in paths.into_iter() {
            if path.empty() {
                continue;
            }

            let first_edge = path
                .graph()
                .edge_ref(path[0])
                .ok_or(LGraphError::InvalidEdge(path[0]))?;
            let mut stack = BracketStack::default();
            let mut prev_node = (first_edge.source_index(), stack.clone());
            start_node = Some(prev_node.clone());

            for edge in path.edges().flat_map(|e| path.graph().edge_ref(e)) {
                stack.try_accept(edge.item().1.clone())?;
                let next_node = (edge.target_index(), stack.clone());
                let item = edge.item().0.as_ref();
                let brackets = edge.item().1.clone();
                builder = builder.add_named_edge((prev_node, (item, brackets), next_node.clone()));
                prev_node = next_node.clone();
            }
            end_nodes.insert(prev_node);
        }

        let start_node = if let Some(n) = start_node {
            n
        } else {
            return Err(LGraphError::NoNodesGiven);
        };

        Ok(LGraph::new_unchecked(
            builder.build(),
            start_node,
            end_nodes,
        ))
    }

    pub fn normal_form(
        &self,
    ) -> Result<LGraph<MangledNode<(NodeIndex, BracketStack<K>)>, &I, K>, LGraphError<K>>
    where
        N: Eq,
        I: Clone,
        K: Eq + Hash + Clone + Debug,
    {
        let core11 = Self::graph_from_paths(self.core(1, 1))?;
        let core12 = Self::graph_from_paths(self.core(1, 2))?;
        let mut builder = MangledBuilder::new(core11.clone().graph.to_builder());

        for node in core12.nodes().flat_map(|n| core12.node_ref(n)) {
            let (q, ab) = node.contents();
            for (path, next_node) in core12
                .reachable_from(node.index())
                .flat_map(|(path, n)| Some((path, core12.node_ref(n)?)))
            {
                let (next, b) = next_node.contents();
                if next != q {
                    continue;
                }

                let a = if let Some(a) = ab.remove_suffix(b) {
                    a
                } else {
                    continue;
                };

                let combined_node = (*next, a);
                if core11.node_with(&combined_node).is_none() {
                    continue;
                }
                builder =
                    builder.add_path(combined_node.clone(), path.items().cloned(), combined_node);
            }
        }

        Ok(LGraph::new_unchecked(
            builder.build(),
            MangledNode::Node(core11.node_ref(self.start_node).unwrap().contents().clone()),
            core11
                .end_nodes()
                .flat_map(|n| core11.node_ref(n))
                .chain(core12.end_nodes().flat_map(|n| core11.node_ref(n)))
                .map(|n| n.contents().clone())
                .map(MangledNode::Node),
        ))
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
            if !self.bracket_stack.empty() {
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
            &g.graph,
            [0, 1, 2, 3, 4, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        assert!(g.is_in_core(&p0, 1, 1).expect("Error"));
        assert!(g.is_in_core(&p0, 1, 2).expect("Error"));

        let p1 = PathRef::new(
            &g.graph,
            [1, 2, 3, 4].into_iter().flat_map(|i| g.edges().nth(i)),
        );
        assert!(g.is_in_core(&p1, 1, 1).expect("Error"));
        assert!(g.is_in_core(&p1, 1, 2).expect("Error"));

        let p2 = PathRef::new(
            &g.graph,
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
            &g.graph,
            [0, 1, 2, 3, 4, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );
        let p1 = PathRef::new(
            &g.graph,
            [1, 2, 3, 4].into_iter().flat_map(|i| g.edges().nth(i)),
        );
        let p2 = PathRef::new(
            &g.graph,
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
            &g.graph,
            [0, 0, 1, 2, 3, 4, 5, 5]
                .into_iter()
                .flat_map(|i| g.edges().nth(i)),
        );

        assert_eq!(g.traverse(s.chars()), Ok(p));
        assert!(g.traverse("dbcd".chars()).is_ok());
        assert!(g.traverse("adbcda".chars()).is_ok());
        assert!(g.traverse("aadbcda".chars()).is_err());
    }

    #[test]
    fn regular() {
        let g = example();
        let normal = g.normal_form().unwrap();
        dbg!(normal);
        assert!(false);
    }
}
