use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
    iter::once,
    marker::PhantomData,
};

use super::{
    default::DefaultBuilder,
    graph_trait::{Builder, Graph},
    iters::AllPairs,
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

    pub fn is_open(&self) -> bool {
        matches!(self.open, BracketType::Open)
    }
}

#[derive(Debug, Default, Hash, Clone, PartialEq, Eq)]
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

    pub fn can_accept(&self, bracket: Bracket) -> bool {
        if bracket.is_open() {
            return true;
        } else if let Some(top) = self.stack.last() {
            if top == &bracket.index {
                return true;
            }
        }

        false
    }

    pub fn accept_clone(&self, bracket: Bracket) -> Option<Self> {
        if !self.can_accept(bracket) {
            return None;
        }

        let mut stack = self.stack.clone();
        if bracket.is_open() {
            stack.push(bracket.index);
        } else {
            stack.pop();
        }
        Some(Self { stack })
    }

    pub fn accept_mut(&mut self, bracket: Bracket) -> bool {
        if !self.can_accept(bracket) {
            return false;
        }

        if bracket.is_open() {
            self.stack.push(bracket.index);
        } else {
            self.stack.pop();
        }
        true
    }

    pub fn has_prefix(&self, prefix: &Self) -> bool {
        self.stack.strip_prefix(prefix.stack.as_slice()).is_some()
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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

#[derive(Debug)]
pub struct Memory<'a, N> {
    node: NodeRef<'a, N>,
    stack: BracketStack,
}

impl<'a, N> Hash for Memory<'a, N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
        self.stack.hash(state);
    }
}

impl<'a, N> Eq for Memory<'a, N> {}

impl<'a, N> PartialEq for Memory<'a, N> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && self.stack == other.stack
    }
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
        self.node.contents()
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

    fn path_to_memories<'a, 'b>(
        &'a self,
        path: &'b Path<'a, N, Item<E>>,
    ) -> impl Iterator<Item = Memory<'a, N>> + 'b
    where
        'a: 'b,
    {
        path.nodes()
            .zip(once(BracketStack::default()).chain(path.edges().scan(
                BracketStack::default(),
                |stack, e| {
                    stack.accept_clone(e.contents().bracket()).map(|new_stack| {
                        *stack = new_stack.clone();
                        new_stack
                    })
                },
            )))
            .map(|(node, stack)| Memory { node, stack })
    }

    fn is_in_core<'a>(&'a self, path: &Path<'a, N, Item<E>>, w: usize, d: usize) -> bool {
        let mut visited: HashMap<_, usize> = HashMap::new();

        for mem in self.path_to_memories(path) {
            if mem.stack.len() > d + 1 {
                return false;
            }

            let count = visited
                .entry(mem)
                .and_modify(|count| *count += 1)
                .or_default();
            if *count > w {
                return false;
            }
        }

        true
    }

    pub fn stack_core(
        &self,
        w: usize,
        d: usize,
    ) -> impl Iterator<Item = Path<'_, N, Item<E>>> + '_ {
        let state_stack = vec![(Path::default(), self.start_node(), BracketStack::default())];
        StackCore {
            graph: self,
            state: state_stack,
            w,
            d,
        }
    }

    fn graph_from_paths<'a, B>(
        &'a self,
        paths: impl Iterator<Item = Path<'a, N, Item<E>>>,
        builder: &mut B,
    ) -> LGraph<Memory<'a, N>, &'a E, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Item<&'a E>>,
    {
        builder.clear();
        let mut end_nodes = vec![];
        for path in paths {
            let mem: Vec<_> = self.path_to_memories(&path).collect();
            if mem.is_empty() {
                continue;
            }

            for (i, edge) in path.edges().enumerate() {
                builder.add_edge(mem[i].clone(), edge.contents().as_ref(), mem[i + 1].clone());
            }

            end_nodes.push(mem.last().cloned().unwrap());
        }

        LGraph::new(builder.build(
            Memory {
                node: self.start_node(),
                stack: BracketStack::default(),
            },
            end_nodes,
        ))
    }

    pub fn stack_core_graph<'a, B>(
        &'a self,
        w: usize,
        d: usize,
        builder: &mut B,
    ) -> LGraph<Memory<'a, N>, &'a E, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Item<&'a E>>,
    {
        let paths = self.stack_core(w, d);
        self.graph_from_paths(paths, builder)
    }

    pub fn delta_stack_core_graph<'a, B>(
        &'a self,
        w: usize,
        d: usize,
        builder: &mut B,
    ) -> LGraph<Memory<'a, N>, &'a E, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Item<&'a E>>,
    {
        let prev_paths: HashSet<_> = self.stack_core(w, d - 1).collect();
        let paths = self.stack_core(w, d).filter(|p| !prev_paths.contains(p));
        self.graph_from_paths(paths, builder)
    }

    pub fn normal_form<'a, B>(
        &'a self,
        builder: &mut B,
    ) -> LGraph<Memory<'a, N>, &'a E, B::TargetGraph>
    where
        B: Builder<Memory<'a, N>, Item<&'a E>>,
        N: Eq + Debug,
        E: Eq + Debug,
    {
        let core;
        let dcore: Vec<_>;

        let mut d = 1;
        loop {
            let c_paths: HashSet<_> = self.stack_core(1, d).collect();

            let dc = self
                .stack_core(1, d + 1)
                .filter(|p| !c_paths.contains(p))
                .collect();
            let c = self.graph_from_paths(c_paths.into_iter(), &mut DefaultBuilder::default());

            if self
                .nodes()
                .all(|node| c.nodes().any(|n| n.contents().node() == node.contents()))
            {
                core = c;
                dcore = dc;
                break;
            }
            d += 1;
        }

        builder.clear();
        for edge in core.edges() {
            builder.add_edge(
                edge.source().contents().clone(),
                edge.contents().clone(),
                edge.target().contents().clone(),
            );
        }

        for node in core.nodes() {
            builder.add_node(node.contents().clone());
        }

        for path in dcore {
            let mems: Vec<_> = self.path_to_memories(&path).collect();

            let has_node = |node: &N, stack: &BracketStack| {
                core.nodes()
                    .any(|n| n.contents().node() == node && n.contents().stack() == stack)
            };

            let mut left_pairs = vec![];
            let mut right_pairs = vec![];

            for (i, a) in mems.iter().enumerate() {
                if !has_node(a.node(), a.stack()) {
                    continue;
                }
                for (j, other) in mems.iter().enumerate().skip(i) {
                    if has_node(other.node(), other.stack()) {
                        continue;
                    }

                    if other.node() == a.node() && other.stack().has_prefix(a.stack()) {
                        left_pairs.push((j, a.node(), a.stack().clone(), other.stack().clone()));
                    }
                }
            }

            for (i, b) in mems.iter().enumerate() {
                if has_node(b.node(), b.stack()) {
                    continue;
                }
                for other in mems.iter().skip(i) {
                    if !has_node(other.node(), other.stack()) {
                        continue;
                    }

                    if other.node() == b.node() && b.stack().has_prefix(other.stack()) {
                        right_pairs.push((i, b.node(), b.stack().clone(), other.stack().clone()));
                    }
                }
            }

            let mut pairs = vec![];
            for ((j, a, as1, as1s2), (i, b, bs1s2, bs1)) in AllPairs::new(left_pairs, right_pairs) {
                if as1 == bs1 && as1s2 == bs1s2 && j < i {
                    pairs.push((a, b, as1, as1s2))
                }
            }

            for (a, b, s1, s1s2) in pairs {
                let from = mems
                    .iter()
                    .enumerate()
                    .find(|(_, mem)| mem.node() == a && mem.stack() == &s1)
                    .map(|(i, _)| i)
                    .unwrap();
                let to = mems
                    .iter()
                    .enumerate()
                    .find(|(_, mem)| mem.node() == a && mem.stack() == &s1s2)
                    .map(|(i, _)| i)
                    .unwrap();

                for (i, edge) in path.edges().enumerate() {
                    if i >= from && i + 1 < to {
                        builder.add_edge(
                            mems[i].clone(),
                            edge.contents().as_ref(),
                            mems[i + 1].clone(),
                        );
                    } else if i + 1 == to {
                        builder.add_edge(
                            mems[i].clone(),
                            edge.contents().as_ref(),
                            mems[from].clone(),
                        );
                    }
                }

                let from = mems
                    .iter()
                    .enumerate()
                    .find(|(_, mem)| mem.node() == b && mem.stack() == &s1s2)
                    .map(|(i, _)| i)
                    .unwrap();
                let to = mems
                    .iter()
                    .enumerate()
                    .find(|(_, mem)| mem.node() == b && mem.stack() == &s1)
                    .map(|(i, _)| i)
                    .unwrap();
                for (i, edge) in path.edges().enumerate() {
                    if i > from && i < to {
                        builder.add_edge(
                            mems[i].clone(),
                            edge.contents().as_ref(),
                            mems[i + 1].clone(),
                        );
                    } else if i == from {
                        builder.add_edge(
                            mems[to].clone(),
                            edge.contents().as_ref(),
                            mems[i + 1].clone(),
                        );
                    }
                }
            }
        }

        LGraph::new(builder.build(
            core.start_node().contents().clone(),
            core.end_nodes().map(|n| n.contents().clone()),
        ))
    }
}

pub struct StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
{
    graph: &'a LGraph<N, E, G>,
    state: Vec<(Path<'a, N, Item<E>>, NodeRef<'a, N>, BracketStack)>,
    w: usize,
    d: usize,
}

impl<'a, N, E, G> Iterator for StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
{
    type Item = Path<'a, N, Item<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((path, node, brackets)) = self.state.pop() {
            let mut res = None;

            if self.graph.is_end_node(node) && brackets.is_empty() {
                res = Some(path.clone());
            }

            for edge in self.graph.edges_from(node) {
                if let Some(new_brackets) = brackets.accept_clone(edge.contents().bracket()) {
                    let new_path = path.clone().push(edge);

                    if self.graph.is_in_core(&new_path, self.w, self.d) {
                        self.state.push((new_path, edge.target(), new_brackets))
                    }
                }
            }

            if let Some(res) = res {
                return Some(res);
            }
        }

        None
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

#[cfg(test)]
mod tests {
    use crate::graphs::{
        default::{DefaultBuilder, DefaultGraph},
        graph_trait::{Builder, Graph},
        refs::Path,
        state_machine::StateMachine,
    };

    use super::{Bracket, BracketType, Item, LGraph};

    fn edge(
        from: i32,
        item: Option<char>,
        index: usize,
        open: bool,
        to: i32,
    ) -> (i32, Item<char>, i32) {
        (
            from,
            Item::new(
                item,
                Bracket::new(
                    index,
                    if open {
                        BracketType::Open
                    } else {
                        BracketType::Close
                    },
                ),
            ),
            to,
        )
    }

    fn example_1() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 2),
            edge(2, Some('b'), 3, true, 3),
            edge(3, None, 3, false, 4),
            edge(4, None, 2, false, 4),
            edge(4, None, 1, false, 5),
            edge(2, Some('c'), 3, true, 6),
            edge(6, None, 3, false, 7),
            edge(7, None, 2, false, 8),
            edge(8, None, 2, false, 9),
            edge(9, None, 2, false, 10),
            edge(10, None, 1, false, 5),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [5]))
    }

    fn example_2() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 3),
            edge(3, None, 3, true, 4),
            edge(4, None, 3, false, 2),
            edge(2, None, 2, false, 5),
            edge(5, None, 2, false, 6),
            edge(6, None, 2, false, 7),
            edge(7, None, 2, false, 7),
            edge(7, None, 2, false, 8),
            edge(8, None, 2, false, 9),
            edge(9, None, 2, false, 10),
            edge(10, None, 2, false, 11),
            edge(11, None, 3, true, 12),
            edge(12, None, 3, false, 10),
            edge(10, None, 1, false, 13),
        ];

        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [13]))
    }

    fn example_3() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, Some('a'), 0, true, 1),
            edge(1, Some('b'), 0, false, 2),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [2]))
    }

    fn example_4() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 2),
            edge(2, Some('b'), 3, true, 20),
            edge(20, None, 3, false, 3),
            edge(3, None, 2, false, 3),
            edge(3, Some('c'), 3, true, 30),
            edge(30, None, 3, false, 4),
            edge(4, Some('a'), 2, true, 4),
            edge(4, Some('d'), 3, true, 40),
            edge(40, None, 3, false, 5),
            edge(5, None, 2, false, 5),
            edge(5, None, 1, false, 6),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [6]))
    }

    fn example_5() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 3),
            edge(3, Some('b'), 3, true, 4),
            edge(4, None, 3, false, 3),
            edge(3, Some('c'), 4, true, 5),
            edge(5, None, 3, true, 6),
            edge(6, None, 3, false, 2),
            edge(2, Some('d'), 3, true, 7),
            edge(7, None, 3, false, 8),
            edge(8, None, 2, false, 8),
            edge(8, None, 4, false, 8),
            edge(8, None, 1, false, 9),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [9]))
    }

    fn example_6() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, Some('a'), 1, true, 2),
            edge(2, Some('b'), 2, true, 3),
            edge(3, None, 2, false, 4),
            edge(4, None, 1, false, 5),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [5]))
    }

    fn example_7() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 2),
            edge(2, None, 2, false, 3),
            edge(3, None, 2, false, 4),
            edge(4, None, 1, false, 5),
            edge(5, Some('b'), 2, true, 5),
            edge(5, Some('c'), 3, true, 6),
            edge(6, None, 3, false, 7),
            edge(7, None, 2, false, 7),
        ];

        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [7]))
    }

    fn example_8() -> LGraph<i32, char, impl Graph<i32, Item<char>>> {
        let edges = [
            edge(1, Some('a'), 1, true, 1),
            edge(1, Some('b'), 2, true, 2),
            edge(2, None, 2, false, 3),
            edge(3, None, 1, false, 3),
        ];

        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [3]))
    }

    fn test_graph(g: LGraph<i32, char, impl Graph<i32, Item<char>>>) -> usize {
        let normal = g.normal_form(&mut DefaultBuilder::default());
        let img = normal.regular_image(&mut DefaultBuilder::default());
        let no_nones = img.remove_nones(&mut DefaultBuilder::default());
        let determined = no_nones.determine(&mut DefaultBuilder::default());
        let minimized = determined.minimize(&mut DefaultBuilder::default());

        minimized.node_count()
    }

    #[test]
    fn regularize_1() {
        assert_eq!(test_graph(example_1()), 6)
    }
    #[test]
    fn regularize_2() {
        assert_eq!(test_graph(example_2()), 7)
    }
    #[test]
    fn regularize_3() {
        assert_eq!(test_graph(example_3()), 3)
    }
    #[test]
    fn regularize_4() {
        assert_eq!(test_graph(example_4()), 4)
    }
    #[test]
    fn regularize_5() {
        assert_eq!(test_graph(example_5()), 5)
    }
    #[test]
    fn regularize_6() {
        assert_eq!(test_graph(example_6()), 3)
    }
    #[test]
    fn regularize_7() {
        assert_eq!(test_graph(example_7()), 4)
    }
    #[test]
    fn regularize_8() {
        assert_eq!(test_graph(example_8()), 2)
    }

    #[test]
    fn t8() {
        let g = example_5();
        let nodes: Vec<_> = [1, 2, 3, 4, 3, 5, 6, 2, 7, 8, 8, 8, 9]
            .iter()
            .flat_map(|n| g.node_with_contents(n))
            .collect();
        let path: Path<i32, Item<char>> = Path::new(
            nodes
                .windows(2)
                .flat_map(|w| {
                    let a = w[0];
                    let b = w[1];
                    g.edges_from_to(a, b)
                })
                .collect::<Vec<_>>(),
        );

        assert!(g.is_in_core(&path, 1, 3))
    }
}
