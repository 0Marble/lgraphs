use std::{collections::HashMap, fmt::Debug, hash::Hash};

use crate::graph::lgraph::LGraphLetter;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Bracket {
    index: usize,
    open: bool,
}

impl Bracket {
    pub fn new(index: usize, open: bool) -> Self {
        Self { index, open }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn is_open(&self) -> bool {
        self.open
    }
}

pub trait Node: Debug + Clone + PartialEq + Eq + Hash {}
impl Node for usize {}
impl Node for char {}
impl Node for String {}

pub trait Letter: Debug + Clone + PartialEq + Eq + Hash {}
impl Letter for usize {}
impl Letter for char {}
impl Letter for String {}
impl Letter for Option<char> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Edge<N, L>
where
    N: Node,
    L: Letter,
{
    source: N,
    target: N,
    letter: L,
}

impl<N, L> Edge<N, LGraphLetter<L>>
where
    N: Node,
    L: Letter,
{
    pub fn bracket(&self) -> Option<&Bracket> {
        self.letter().bracket()
    }
    pub fn item(&self) -> Option<&L> {
        self.letter().letter()
    }
}

impl<N, L> Edge<N, L>
where
    N: Node,
    L: Letter,
{
    pub fn new(source: N, target: N, letter: L) -> Self {
        Self {
            source,
            target,
            letter,
        }
    }

    pub fn beg(&self) -> &N {
        &self.source
    }

    pub fn end(&self) -> &N {
        &self.target
    }

    pub fn letter(&self) -> &L {
        &self.letter
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<N, L>
where
    N: Node,
    L: Letter,
{
    inner: hidden::Path<N, L>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BracketStack {
    state: Vec<usize>,
}

impl Hash for BracketStack {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.state.hash(state);
    }
}

impl BracketStack {
    pub fn new() -> Self {
        Self { state: vec![] }
    }

    pub fn accept(&mut self, bracket: Bracket) {
        if bracket.is_open() {
            self.state.push(bracket.index());
        } else if Some(bracket.index()) == self.state.last().cloned() {
            self.state.pop();
        }
    }

    pub fn can_accept(&self, bracket: &Bracket) -> bool {
        if bracket.is_open() {
            true
        } else {
            Some(bracket.index()) == self.state.last().cloned()
        }
    }

    pub fn clear(&mut self) {
        self.state.clear()
    }

    pub fn state(&self) -> &[usize] {
        &self.state
    }

    pub fn is_empty(&self) -> bool {
        self.state.is_empty()
    }

    pub fn len(&self) -> usize {
        self.state.len()
    }
}

impl Default for BracketStack {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Memory<N: Node> {
    node: N,
    brackets: BracketStack,
}

impl<N: Node> Memory<N> {
    pub fn new(node: N, brackets: BracketStack) -> Self {
        Self { node, brackets }
    }

    pub fn node(&self) -> &N {
        &self.node
    }

    pub fn brackets(&self) -> &BracketStack {
        &self.brackets
    }
}

impl<N: Node> Node for Memory<N> {}

impl<N, L> Path<N, LGraphLetter<L>>
where
    N: Node,
    L: Letter,
{
    pub fn mem(&self) -> Path<Memory<N>, LGraphLetter<L>> {
        let mut stack = BracketStack::default();
        let mut res = Path::new(Memory::new(self.beg().clone(), stack.clone()));

        for edge in self.edges() {
            let start = Memory::new(edge.beg().clone(), stack.clone());
            if let Some(bracket) = edge.bracket() {
                stack.accept(bracket.clone());
            }
            let end = Memory::new(edge.end().clone(), stack.clone());
            res.add_edge(Edge::new(start, end, edge.letter.clone()));
        }

        res
    }

    pub fn depth(&self) -> usize {
        let mut stack = BracketStack::new();
        let mut max = 0;
        for edge in self.edges() {
            if let Some(bracket) = edge.bracket() {
                stack.accept(bracket.clone());
                max = usize::max(max, stack.len());
            }
        }

        max
    }

    pub fn get_w(&self) -> usize {
        let mem = self.mem();
        let mut mem_repeats: HashMap<_, Vec<_>> = HashMap::new();

        for (i, node) in mem.nodes().enumerate() {
            mem_repeats.entry(node).or_default().push(i);
        }

        mem_repeats
            .values()
            .filter(|r| r.len() > 1)
            .map(|r| r.len() - 1)
            .max()
            .unwrap_or_default()
    }

    pub fn get_d(&self) -> usize {
        let mem = self.mem();
        let mut stack_repeats: HashMap<_, Vec<_>> = HashMap::new();
        let mut node_repeats: HashMap<_, Vec<_>> = HashMap::new();

        for (i, node) in mem.nodes().enumerate() {
            stack_repeats.entry(node.brackets()).or_default().push(i);
            node_repeats.entry(node.node()).or_default().push(i);
        }

        let mut max_d = 0;
        for mid_stack_repeats in stack_repeats.values() {
            for i in 0..mid_stack_repeats.len() {
                let mid_begin_node_index = mid_stack_repeats[i];
                let mid_begin_node = self.nth_node(mid_begin_node_index).unwrap();

                let left_node_repeats = node_repeats.get(mid_begin_node).unwrap();
                if left_node_repeats.len() == 1 {
                    continue;
                }

                #[allow(clippy::needless_range_loop)]
                for j in i + 1..mid_stack_repeats.len() {
                    let mid_end_node_index = mid_stack_repeats[j];
                    let mid_end_node = self.nth_node(mid_end_node_index).unwrap();
                    if mid_begin_node == mid_end_node {
                        continue;
                    }
                    if !self.is_subpath_balanced(mid_begin_node_index, mid_end_node_index) {
                        continue;
                    }

                    let right_node_repeats = node_repeats.get(mid_end_node).unwrap();
                    if right_node_repeats.len() == 1 {
                        continue;
                    }

                    let mut cur_d = 0;
                    for k in left_node_repeats.iter().rev() {
                        let k = *k;
                        if k >= mid_begin_node_index {
                            continue;
                        }
                        if self.is_subpath_balanced(k, mid_begin_node_index) {
                            continue;
                        }

                        let kth_mem = mem.nth_node(k).unwrap();

                        for m in right_node_repeats {
                            let m = *m;
                            if m <= mid_end_node_index {
                                continue;
                            }
                            if self.is_subpath_balanced(mid_end_node_index, m) {
                                continue;
                            }

                            let mth_mem = mem.nth_node(m).unwrap();

                            if mth_mem.brackets() == kth_mem.brackets()
                                && self.is_subpath_balanced(k, m)
                            {
                                cur_d += 1;
                                break;
                            }
                        }
                    }

                    max_d = usize::max(max_d, cur_d);
                }
            }
        }

        max_d
    }

    pub fn iota(&self) -> Vec<Bracket> {
        let hidden::Path::NonEmpty(edges) = &self.inner else {
            return  vec![];
        };

        let mut stack = vec![];
        for edge in edges {
            if let Some(bracket) = edge.bracket() {
                stack.push(bracket.clone())
            }
        }

        stack
    }

    pub fn is_balanced(&self) -> bool {
        let mut stack = BracketStack::default();
        for edge in self.edges() {
            if let Some(bracket) = edge.bracket() {
                if !stack.can_accept(bracket) {
                    return false;
                }
                stack.accept(bracket.clone());
            }
        }

        stack.is_empty()
    }

    pub fn is_subpath_balanced(&self, beg_node_index: usize, end_node_index: usize) -> bool {
        if beg_node_index > end_node_index {
            return false;
        }

        let mut stack = BracketStack::default();
        for i in beg_node_index..end_node_index {
            let Some(edge) = self.nth_edge(i) else {return false;};
            if let Some(bracket) = edge.bracket() {
                if !stack.can_accept(bracket) {
                    return false;
                }
                stack.accept(bracket.clone());
            }
        }

        stack.is_empty()
    }

    pub fn paired_loops(&self) -> impl Iterator<Item = ((usize, usize), (usize, usize))> + '_ {
        let n = self.len_in_nodes();
        (0..n)
            .flat_map(move |t21| {
                (t21 + 1..n)
                    .filter(move |t22| self.nth_node(t21) == self.nth_node(*t22))
                    .flat_map(move |t22| {
                        (t22 + 1..n).flat_map(move |t41| {
                            (t41 + 1..n)
                                .filter(move |t42| self.nth_node(t41) == self.nth_node(*t42))
                                .map(move |t42| ((t21, t22), (t41, t42)))
                        })
                    })
            })
            .filter(|(left, right)| self.is_nest(*left, *right))
    }

    pub fn is_nest(&self, left: (usize, usize), right: (usize, usize)) -> bool {
        if left.0 == left.1 || right.0 == right.1 {
            return false;
        }

        let mut stack = BracketStack::default();
        for i in left.0..left.1 {
            let Some(edge) = self.nth_edge(i) else {return false;};
            if let Some(bracket) = edge.bracket() {
                if !stack.can_accept(bracket) {
                    return false;
                }
                stack.accept(bracket.clone());
            }
        }
        if stack.is_empty() {
            return false;
        } // (0-1) is not neutral

        let stack_at_left_1 = stack.clone();
        let mut mid_stack = BracketStack::default();
        for i in left.1..right.0 {
            let Some(edge) = self.nth_edge(i) else {return false;};
            if let Some(bracket) = edge.bracket() {
                if !stack.can_accept(bracket) || !mid_stack.can_accept(bracket) {
                    return false;
                }
                stack.accept(bracket.clone());
                mid_stack.accept(bracket.clone())
            }
        }
        if stack != stack_at_left_1 {
            return false;
        } // (1-2) is neutral, but (0-1-2) isnt

        for i in right.0..right.1 {
            let Some(edge) = self.nth_edge(i) else {return false;};
            if let Some(bracket) = edge.bracket() {
                if !stack.can_accept(bracket) {
                    return false;
                }
                stack.accept(bracket.clone());
            }
        }
        if !stack.is_empty() {
            return false;
        } // (0-1-2-3) is neutral

        true
    }

    pub fn is_simple_paired_loops(&self, left: (usize, usize), right: (usize, usize)) -> bool {
        for t21 in left.0 + 1..left.1 {
            if self.nth_node(t21) != self.nth_node(left.1) {
                continue;
            }

            for t42 in right.0 + 1..right.1 {
                if self.nth_node(right.0) != self.nth_node(t42) {
                    continue;
                }

                if self.is_nest((t21, left.1), (right.0, t42))
                    && self.is_nest((left.0, t21), (t42, right.1))
                {
                    return false;
                }
            }
        }
        true
    }

    #[inline]
    pub fn simple_paired_loops(
        &self,
    ) -> impl Iterator<Item = ((usize, usize), (usize, usize))> + '_ {
        self.paired_loops()
            .filter(|(l, r)| self.is_simple_paired_loops(*l, *r))
    }
}

impl Path<usize, LGraphLetter<char>> {}

impl<N, L> Path<N, L>
where
    N: Node,
    L: Letter,
{
    pub fn new(node: N) -> Self {
        Self {
            inner: hidden::Path::Empty(node),
        }
    }

    pub fn add_edge(&mut self, edge: Edge<N, L>) {
        match &mut self.inner {
            hidden::Path::Empty(_) => self.inner = hidden::Path::NonEmpty(vec![edge]),
            hidden::Path::NonEmpty(edges) => {
                edges.push(edge);
            }
        }
    }

    pub fn beg(&self) -> &N {
        match &self.inner {
            hidden::Path::Empty(node) => node,
            hidden::Path::NonEmpty(edges) => edges.first().unwrap().beg(),
        }
    }

    pub fn end(&self) -> &N {
        match &self.inner {
            hidden::Path::Empty(node) => node,
            hidden::Path::NonEmpty(edges) => edges.last().unwrap().end(),
        }
    }

    pub fn len_in_edges(&self) -> usize {
        match &self.inner {
            hidden::Path::Empty(_) => 0,
            hidden::Path::NonEmpty(e) => e.len(),
        }
    }

    pub fn len_in_nodes(&self) -> usize {
        match &self.inner {
            hidden::Path::Empty(_) => 1,
            hidden::Path::NonEmpty(e) => e.len() + 1,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len_in_edges() == 0
    }

    pub fn nth_node(&self, n: usize) -> Option<&N> {
        match &self.inner {
            hidden::Path::Empty(node) if n == 0 => Some(node),
            hidden::Path::NonEmpty(edges) if edges.len() + 1 > n => {
                if n == 0 {
                    Some(edges[0].beg())
                } else {
                    Some(edges[n - 1].end())
                }
            }
            _ => None,
        }
    }

    pub fn nth_edge(&self, n: usize) -> Option<&Edge<N, L>> {
        match &self.inner {
            hidden::Path::NonEmpty(edges) => edges.get(n),
            _ => None,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = &N> + '_ {
        (0..self.len_in_edges() + 1).map(|i| self.nth_node(i).unwrap())
    }

    pub fn edges(&self) -> impl Iterator<Item = &Edge<N, L>> + '_ {
        (0..self.len_in_edges()).map(|i| self.nth_edge(i).unwrap())
    }

    pub fn subpath(&self, beg_node_index: usize, end_node_index: usize) -> Option<Self> {
        if beg_node_index > end_node_index {
            return None;
        }
        let mut t = Self::new(self.nth_node(beg_node_index)?.clone());

        for i in beg_node_index..end_node_index {
            t.add_edge(self.nth_edge(i)?.clone());
        }

        Some(t)
    }
    pub fn loopify_on_first(&self) -> Option<Self> {
        if self.is_empty() {
            return None;
        }

        let mut t = self.clone();
        match &mut t.inner {
            hidden::Path::Empty(_) => unreachable!(),
            hidden::Path::NonEmpty(edges) => {
                let first = edges.first().unwrap().clone();
                let last = edges.last_mut().unwrap();
                last.target = first.source;
            }
        }

        Some(t)
    }
    pub fn loopify_on_last(&self) -> Option<Self> {
        if self.is_empty() {
            return None;
        }

        let mut t = self.clone();
        match &mut t.inner {
            hidden::Path::Empty(_) => unreachable!(),
            hidden::Path::NonEmpty(edges) => {
                let last = edges.last().unwrap().clone();
                let first = edges.first_mut().unwrap();
                first.source = last.target;
            }
        }

        Some(t)
    }

    pub fn concat(&self, next: &Self) -> Self {
        let mut t = Path::new(self.beg().clone());
        for edge in self.edges().chain(next.edges()) {
            t.add_edge(edge.clone());
        }

        t
    }
}

mod hidden {
    use super::{Edge, Letter, Node};

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Path<N, L>
    where
        N: Node,
        L: Letter,
    {
        Empty(N),
        NonEmpty(Vec<Edge<N, L>>),
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, str::FromStr};

    use super::*;

    #[test]
    fn paired_loops() {
        for s in [
            "0-[0->0->1-]0->1",
            "0->1",
            "0->0",
            "0->1-[0->2->1->3-]0->3",
            "0->1-[0->2->1->1->3-]0->3",
            "0->1-[0->2->1-[0->1->3-]0->3-]0->4->3",
            "0-[0->0->0->0->1-]0->1->2-[0->2-[0->2->3-]0->3->3-]0->4->3",
        ] {
            let t = Path::from_str(s).unwrap();
            let loops: HashSet<_> = HashSet::from_iter(t.paired_loops());
            println!("t: {}", t);

            for ((t21, t22), (t41, t42)) in &loops {
                println!("\tt1: {}", t.subpath(0, *t21).unwrap());
                println!("\tt2: {}", t.subpath(*t21, *t22).unwrap());
                println!("\tt3: {}", t.subpath(*t22, *t41).unwrap());
                println!("\tt4: {}", t.subpath(*t41, *t42).unwrap());
                println!("\tt5: {}", t.subpath(*t42, t.len_in_nodes() - 1).unwrap());
                println!();
            }

            for t21 in 0..t.len_in_nodes() {
                for t22 in t21 + 1..t.len_in_nodes() {
                    for t41 in t22 + 1..t.len_in_nodes() {
                        for t42 in t41 + 1..t.len_in_nodes() {
                            let left = (t21, t22);
                            let right = (t41, t42);
                            if left.0 != left.1
                                && right.0 != right.1
                                && !t.is_subpath_balanced(left.0, left.1)
                                && !t.is_subpath_balanced(right.0, right.1)
                                && t.is_subpath_balanced(left.0, right.1)
                                && t.is_subpath_balanced(left.1, right.1)
                                && t.nth_node(left.0) == t.nth_node(left.1)
                                && t.nth_node(right.0) == t.nth_node(right.1)
                            {
                                assert!(loops.contains(&(left, right)));
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn simple_paired_loops() {
        #[allow(clippy::type_complexity)]
        fn split<N, L>(
            t: &Path<N, L>,
            left: (usize, usize),
            right: (usize, usize),
        ) -> (Path<N, L>, Path<N, L>, Path<N, L>, Path<N, L>, Path<N, L>)
        where
            N: Node,
            L: Letter,
        {
            (
                t.subpath(0, left.0).unwrap(),
                t.subpath(left.0, left.1).unwrap(),
                t.subpath(left.1, right.0).unwrap(),
                t.subpath(right.0, right.1).unwrap(),
                t.subpath(right.1, t.len_in_nodes() - 1).unwrap(),
            )
        }
        fn get_loops(
            t: &Path<usize, LGraphLetter<char>>,
        ) -> impl Iterator<
            Item = (
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
            ),
        > + '_ {
            t.paired_loops()
                .filter(|(l, r)| t.is_simple_paired_loops(*l, *r))
                .map(move |(l, r)| split(t, l, r))
        }
        #[allow(clippy::type_complexity)]
        fn print_paths(
            t: &(
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
                Path<usize, LGraphLetter<char>>,
            ),
        ) {
            let (t1, t2, t3, t4, t5) = t;

            println!("t1: {}", t1);
            println!("t2: {}", t2);
            println!("t3: {}", t3);
            println!("t4: {}", t4);
            println!("t5: {}\n", t5);
        }

        let t = Path::from_str("0-[0->0->1-]0->1").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        let actual_loops =
            HashSet::from_iter([split(&t, (0, 1), (2, 3))].into_iter().inspect(print_paths));
        assert_eq!(loops, actual_loops);

        let t = Path::from_str("0->1").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        let actual_loops = HashSet::from_iter([].into_iter().inspect(print_paths));
        assert_eq!(loops, actual_loops);

        let t = Path::from_str("0->1").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        let actual_loops = HashSet::from_iter([].into_iter().inspect(print_paths));
        assert_eq!(loops, actual_loops);

        let t = Path::from_str("1-[0->1-[0->1->2-]0->2-]0->2").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        let actual_loops = HashSet::from_iter(
            [split(&t, (0, 1), (4, 5)), split(&t, (1, 2), (3, 4))]
                .into_iter()
                .inspect(print_paths),
        );
        assert_eq!(loops, actual_loops);

        let t = Path::from_str("0-[0->0->1-[0->1->2-]0->2-]0->2").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        let actual_loops = HashSet::from_iter(
            [split(&t, (0, 1), (5, 6)), split(&t, (2, 3), (4, 5))]
                .into_iter()
                .inspect(print_paths),
        );
        assert_eq!(loops, actual_loops);

        // 0     1  2  3  4     5  6     7     8  9    10 11    12 13
        // 0-[0->0->0->0->1-]0->1->2-[0->2-[0->2->3-]0->3->3-]0->4->3
        let t =
            Path::from_str("0-[0->0->0->0->1-]0->1->2-[0->2-[0->2->3-]0->3->3-]0->4->3").unwrap();
        let loops: HashSet<_> = get_loops(&t).inspect(print_paths).collect();
        println!("!!!!!!!");
        let actual_loops = HashSet::from_iter(
            [
                split(&t, (0, 1), (4, 5)),
                split(&t, (0, 2), (4, 5)),
                split(&t, (0, 3), (4, 5)),
                split(&t, (6, 7), (11, 13)),
                split(&t, (6, 7), (10, 13)),
                split(&t, (7, 8), (9, 10)),
                split(&t, (7, 8), (9, 11)),
            ]
            .into_iter()
            .inspect(print_paths),
        );
        assert_eq!(loops, actual_loops);
    }

    #[test]
    fn subpath() {
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(0, 2).unwrap(),
            Path::from_str("1->2->3").unwrap()
        );
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(0, 1).unwrap(),
            Path::from_str("1->2").unwrap()
        );
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(0, 0).unwrap(),
            Path::from_str("1").unwrap()
        );
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(1, 2).unwrap(),
            Path::from_str("2->3").unwrap()
        );
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(2, 2).unwrap(),
            Path::from_str("3").unwrap()
        );
        assert_eq!(
            Path::from_str("1->2->3").unwrap().subpath(1, 1).unwrap(),
            Path::from_str("2").unwrap()
        );
    }

    #[test]
    fn parse() {
        let mut t = Path::new(0);
        t.add_edge(Edge::new(0, 1, LGraphLetter::default()));
        assert_eq!(t, Path::from_str("0->1").unwrap());

        t = Path::new(0);
        assert_eq!(t, Path::from_str("0").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(0, 1, LGraphLetter::default()));
        t.add_edge(Edge::new(1, 2, LGraphLetter::default()));
        assert_eq!(t, Path::from_str("0->1->2").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(0, 1, LGraphLetter::new(Some('a'), None)));
        assert_eq!(t, Path::from_str("0-a->1").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(
            0,
            1,
            LGraphLetter::new(None, Some(Bracket::new(0, true))),
        ));
        assert_eq!(t, Path::from_str("0-[0->1").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(
            0,
            1,
            LGraphLetter::new(None, Some(Bracket::new(0, false))),
        ));
        assert_eq!(t, Path::from_str("0-]0->1").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(
            0,
            1,
            LGraphLetter::new(Some('a'), Some(Bracket::new(0, false))),
        ));
        assert_eq!(t, Path::from_str("0-a]0->1").unwrap());

        t = Path::new(0);
        t.add_edge(Edge::new(
            0,
            1,
            LGraphLetter::new(Some('a'), Some(Bracket::new(0, true))),
        ));
        t.add_edge(Edge::new(
            1,
            2,
            LGraphLetter::new(Some('b'), Some(Bracket::new(1, false))),
        ));
        assert_eq!(t, Path::from_str("0-a[0->1-b]1->2").unwrap());

        t = Path::new(10);
        t.add_edge(Edge::new(
            10,
            20,
            LGraphLetter::new(Some('a'), Some(Bracket::new(3124, true))),
        ));
        assert_eq!(t, Path::from_str("10-a[3124->20").unwrap());
    }

    #[test]
    fn get_w() {
        let mut t = Path::from_str("0-[0->1-]0->0").unwrap();
        assert_eq!(t.get_w(), 1);

        t = Path::from_str("0").unwrap();
        assert_eq!(t.get_w(), 0);

        t = Path::from_str("0->0").unwrap();
        assert_eq!(t.get_w(), 1);

        t = Path::from_str("0->0->0").unwrap();
        assert_eq!(t.get_w(), 2);

        t = Path::from_str("0-[0->1-[0->0").unwrap();
        assert_eq!(t.get_w(), 0);

        t = Path::from_str("0->1->2-[0->3->4-]0->5->0").unwrap();
        assert_eq!(t.get_w(), 1);
    }

    #[test]
    fn get_d() {
        assert_eq!(Path::from_str("0-[0->0->1-]0->1").unwrap().get_d(), 1);
        assert_eq!(Path::from_str("0->1").unwrap().get_d(), 0);
        assert_eq!(Path::from_str("0->0").unwrap().get_d(), 0);
        assert_eq!(Path::from_str("0->1-[0->2->1->3-]0->3").unwrap().get_d(), 1);
        assert_eq!(
            Path::from_str("0->1-[0->2->1->1->3-]0->3").unwrap().get_d(),
            1
        );
        assert_eq!(
            Path::from_str("0-[0->0->1-[0->1->2-]0->2-]0->2")
                .unwrap()
                .get_d(),
            1
        );
        assert_eq!(
            Path::from_str("1-[0->1-[0->1->2-]0->2-]0->2")
                .unwrap()
                .get_d(),
            2
        );
        assert_eq!(
            Path::from_str("0->1-[0->2->1-[0->1->3-]0->3-]0->4->3")
                .unwrap()
                .get_d(),
            2
        );
        assert_eq!(
            Path::from_str("0-[0->0->0->0->1-]0->1->2-[0->2-[0->2->3-]0->3->3-]0->4->3")
                .unwrap()
                .get_d(),
            2
        );
    }
}
