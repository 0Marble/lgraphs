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

    pub fn can_accept(&self, bracket: Bracket) -> bool {
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
                if !stack.can_accept(bracket.clone()) {
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
                if !stack.can_accept(bracket.clone()) {
                    return false;
                }
                stack.accept(bracket.clone());
            }
        }

        stack.is_empty()
    }

    #[allow(clippy::type_complexity)]
    pub fn paired_loops(
        &self,
    ) -> Vec<(
        (usize, usize),
        (usize, usize),
        (usize, usize),
        (usize, usize),
        (usize, usize),
    )> {
        let mem = self.mem();
        let mut stack_repeats: HashMap<_, Vec<_>> = HashMap::new();
        let mut node_repeats: HashMap<_, Vec<_>> = HashMap::new();

        for (i, node) in mem.nodes().enumerate() {
            stack_repeats.entry(node.brackets()).or_default().push(i);
            node_repeats.entry(node.node()).or_default().push(i);
        }

        let mut res = vec![];
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
                                res.push((
                                    (0, k),
                                    (k, mid_begin_node_index),
                                    (mid_begin_node_index, mid_end_node_index),
                                    (mid_end_node_index, m),
                                    (m, self.len_in_nodes() - 1),
                                ));
                                break;
                            }
                        }
                    }
                }
            }
        }

        res
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
    use std::str::FromStr;

    use super::*;

    #[test]
    fn paired_loops() {
        let t =
            Path::from_str("0-[0->0->0->0->1-]0->1->2-[0->2-[0->2->3-]0->3->3-]0->4->3").unwrap();
        // (0)-[0->(0)->0->0->(1)-]0->(1) -> (2)-[0->(2)-[0->(2)->(3)-]0->(3)->3-]0->4->(3)
        let loops = t.paired_loops();
        for ((t11, t12), (t21, t22), (t31, t32), (t41, t42), (t51, t52)) in loops {
            println!("t1: {}", t.subpath(t11, t12).unwrap());
            println!("t2: {}", t.subpath(t21, t22).unwrap());
            println!("t3: {}", t.subpath(t31, t32).unwrap());
            println!("t4: {}", t.subpath(t41, t42).unwrap());
            println!("t5: {}", t.subpath(t51, t52).unwrap());
            println!()
        }
        panic!();
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
        assert_eq!(t, Path::from_str("0-a-]0->1").unwrap());

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
        assert_eq!(t, Path::from_str("0-a-[0->1-b-]1->2").unwrap());

        t = Path::new(10);
        t.add_edge(Edge::new(
            10,
            20,
            LGraphLetter::new(Some('a'), Some(Bracket::new(3124, true))),
        ));
        assert_eq!(t, Path::from_str("10-a-[3124->20").unwrap());
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
