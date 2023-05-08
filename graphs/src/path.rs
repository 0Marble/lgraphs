use std::{collections::HashMap, fmt::Debug, hash::Hash, str::FromStr};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Edge<N>
where
    N: Node,
{
    source: N,
    target: N,
    contents: (Option<usize>, Option<Bracket>),
}

impl<N> Edge<N>
where
    N: Node,
{
    pub fn new(source: N, target: N, contents: (Option<usize>, Option<Bracket>)) -> Self {
        Self {
            source,
            target,
            contents,
        }
    }

    pub fn beg(&self) -> &N {
        &self.source
    }

    pub fn end(&self) -> &N {
        &self.target
    }

    pub fn omega(&self) -> Option<&usize> {
        self.contents.0.as_ref()
    }

    pub fn iota(&self) -> Option<&Bracket> {
        self.contents.1.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path<N>
where
    N: Node,
{
    inner: hidden::Path<N>,
}

// format: path = letter | letter ('-' digit )? ('-' '[' | ']' digit)? '->' path
impl FromStr for Path<char> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum State {
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
        }

        fn make_error(s: &str, i: usize, msg: &str) -> Result<(), String> {
            Err(format!(
                "Error parsing path:\n\"{}\"\n {}^: {}",
                s,
                (0..i).map(|_| '~').collect::<String>(),
                msg
            ))
        }

        let mut path: Option<Path<char>> = None;
        let mut prev_node = ' ';
        let mut item = None;
        let mut bracket = None;

        let mut state = State::A;
        for (i, c) in s.char_indices() {
            if c.is_whitespace() {
                continue;
            }

            match state {
                State::A => {
                    if c.is_alphabetic() {
                        if let Some(path) = path.as_mut() {
                            path.add_edge(Edge::new(prev_node, c, (item.take(), bracket.take())));
                        } else {
                            path = Some(Path::new(c));
                        }

                        prev_node = c;
                        state = State::B;
                    } else {
                        make_error(s, i, "Expected a letter")?;
                    }
                }
                State::B => {
                    if c == '-' {
                        state = State::C;
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::C => match c {
                    '[' => {
                        bracket = Some(Bracket::new(0, true));
                        state = State::E;
                    }
                    ']' => {
                        bracket = Some(Bracket::new(0, false));
                        state = State::E;
                    }
                    c if c.is_numeric() => {
                        item = Some(c.to_digit(10).unwrap() as usize);
                        state = State::D;
                    }
                    '>' => state = State::A,
                    _ => make_error(s, i, "Expected a digit, '>', '[' or ']'")?,
                },
                State::D => {
                    if c == '-' {
                        state = State::F
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::E => {
                    if c.is_numeric() {
                        bracket.as_mut().unwrap().index = c.to_digit(10).unwrap() as usize;
                        state = State::G;
                    } else {
                        make_error(s, i, "Expected a digit")?;
                    }
                }
                State::F => match c {
                    '[' => {
                        bracket = Some(Bracket::new(0, true));
                        state = State::E;
                    }
                    ']' => {
                        bracket = Some(Bracket::new(0, false));
                        state = State::E;
                    }
                    '>' => state = State::A,
                    _ => make_error(s, i, "Expected a '>', '[' or ']'")?,
                },
                State::G => {
                    if c == '-' {
                        state = State::H
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::H => {
                    if c == '>' {
                        state = State::A
                    } else {
                        make_error(s, i, "Expected a '>'")?;
                    }
                }
            }
        }

        let i = s.len();
        match state {
            State::A => make_error(s, i, "Expected a letter")?,
            State::B => {}
            State::C => make_error(s, i, "Expected a digit, '>', '[' or ']'")?,
            State::D => make_error(s, i, "Expected a '-'")?,
            State::E => make_error(s, i, "Expected a digit")?,
            State::F => make_error(s, i, "Expected a '>', '[' or ']'")?,
            State::G => make_error(s, i, "Expected a '-'")?,
            State::H => make_error(s, i, "Expected a '>'")?,
        }

        Ok(path.unwrap())
    }
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

    pub fn push(&mut self, bracket: Bracket) -> bool {
        if bracket.is_open() {
            self.state.push(bracket.index());
            true
        } else if Some(bracket.index()) == self.state.last().cloned() {
            self.state.pop();
            true
        } else {
            false
        }
    }

    pub fn clear(&mut self) {
        self.state.clear()
    }

    pub fn state(&self) -> &[usize] {
        &self.state
    }
}

impl Default for BracketStack {
    fn default() -> Self {
        Self::new()
    }
}

impl Node for usize {}
impl Node for char {}

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

impl<N> Path<N>
where
    N: Node,
{
    pub fn mem(&self) -> Path<Memory<N>> {
        let mut stack = BracketStack::default();
        let mut res = Path::new(Memory::new(self.beg().clone(), stack.clone()));

        for edge in self.edges() {
            let start = Memory::new(edge.beg().clone(), stack.clone());
            if let Some(bracket) = edge.iota() {
                stack.push(bracket.clone());
            }
            let end = Memory::new(edge.end().clone(), stack.clone());
            res.add_edge(Edge::new(start, end, edge.contents.clone()));
        }

        res
    }

    pub fn neutral_loops(&self) -> Vec<(Path<N>, Path<N>, Path<N>)> {
        let mut res = vec![];
        let mem = self.mem();
        let mut mem_repeats: HashMap<_, Vec<_>> = HashMap::new();

        for (i, node) in mem.nodes().enumerate() {
            mem_repeats.entry(node).or_default().push(i);
        }

        for repeats in mem_repeats.values().filter(|r| r.len() > 1) {
            for i in 0..repeats.len() {
                let loop_start_node_index = repeats[i];
                #[allow(clippy::needless_range_loop)]
                for j in i + 1..repeats.len() {
                    let loop_end_node_index = repeats[j];
                    // loop_start_node_index = 2, loop_end_node_index = 5, len = 7
                    // t1 = [0]0-1,[1]1-2
                    // t2 = [2]2-3,[3]3-4,[4]4-5,
                    // t3 = [5]5-6,[6]6-7

                    // loop_start_index = 0, loop_end_index = 3, len = 5
                    // t1 = 0
                    // t2 = [0]0-1,[1]1-2,[2]2-3,
                    // t3 = [3]3-4,[4]4-5,

                    // loop_start_node_index = 2, loop_end_node_index = 5, len = 5
                    // t1 = [0]0-1,[1]1-2
                    // t2 = [2]2-3,[3]3-4,[4]4-5,
                    // t3 = 5

                    let mut t1 = Path::new(self.beg().clone());
                    for i in 0..loop_start_node_index {
                        t1.add_edge(self.nth_edge(i).unwrap().clone());
                    }

                    let mut t2 = Path::new(self.nth_node(loop_start_node_index).unwrap().clone());
                    for i in loop_start_node_index..loop_end_node_index {
                        t2.add_edge(self.nth_edge(i).unwrap().clone());
                    }

                    let mut t3 = Path::new(self.nth_node(loop_end_node_index).unwrap().clone());
                    for i in loop_end_node_index..self.len_in_edges() {
                        t3.add_edge(self.nth_edge(i).unwrap().clone());
                    }

                    res.push((t1, t2, t3))
                }
            }
        }

        res
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

                    let right_node_repeats = node_repeats.get(mid_end_node).unwrap();
                    if right_node_repeats.len() == 1 {
                        continue;
                    }

                    let mut cur_d = 0;

                    let mut prev_k = None;
                    let mut prev_m = None;
                    for k in left_node_repeats.iter().rev() {
                        let k = *k;
                        if k >= mid_begin_node_index {
                            continue;
                        }

                        let kth_mem = mem.nth_node(k).unwrap();
                        if let Some(prev_k) = prev_k.as_mut() {
                            let prev_kth_mem = mem.nth_node(*prev_k).unwrap();
                            if prev_kth_mem.brackets() == kth_mem.brackets() {
                                *prev_k = k;
                                continue;
                            }
                        }
                        prev_k = Some(k);

                        for m in right_node_repeats {
                            let m = *m;
                            if m <= mid_end_node_index {
                                continue;
                            }

                            let mth_mem = mem.nth_node(m).unwrap();
                            if let Some(prev_m) = prev_m.as_mut() {
                                let prev_mth_mem = mem.nth_node(*prev_m).unwrap();

                                if prev_mth_mem.brackets() == mth_mem.brackets() {
                                    *prev_m = m;
                                    continue;
                                }
                            }
                            prev_m = Some(m);

                            if mth_mem.brackets() == kth_mem.brackets() {
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
}

impl<N> Path<N>
where
    N: Node,
{
    pub fn new(node: N) -> Self {
        Self {
            inner: hidden::Path::Empty(node),
        }
    }

    pub fn add_edge(&mut self, edge: Edge<N>) {
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

    pub fn nth_edge(&self, n: usize) -> Option<&Edge<N>> {
        match &self.inner {
            hidden::Path::NonEmpty(edges) => edges.get(n),
            _ => None,
        }
    }

    pub fn nodes(&self) -> impl Iterator<Item = &N> + '_ {
        (0..self.len_in_edges() + 1).map(|i| self.nth_node(i).unwrap())
    }

    pub fn edges(&self) -> impl Iterator<Item = &Edge<N>> + '_ {
        (0..self.len_in_edges()).map(|i| self.nth_edge(i).unwrap())
    }

    pub fn iota(&self) -> Vec<Bracket> {
        let hidden::Path::NonEmpty(edges) = &self.inner else {
            return  vec![];
        };

        let mut stack = vec![];
        for edge in edges {
            if let Some(bracket) = edge.iota() {
                stack.push(bracket.clone())
            }
        }

        stack
    }
}

mod hidden {
    use super::{Edge, Node};

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Path<N>
    where
        N: Node,
    {
        Empty(N),
        NonEmpty(Vec<Edge<N>>),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        let mut t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (None, None)));
        assert_eq!(t, Path::from_str("a->b").unwrap());

        t = Path::new('a');
        assert_eq!(t, Path::from_str("a").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (None, None)));
        t.add_edge(Edge::new('b', 'c', (None, None)));
        assert_eq!(t, Path::from_str("a->b->c").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (Some(1), None)));
        assert_eq!(t, Path::from_str("a-1->b").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (None, Some(Bracket::new(0, true)))));
        assert_eq!(t, Path::from_str("a-[0->b").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (None, Some(Bracket::new(0, false)))));
        assert_eq!(t, Path::from_str("a-]0->b").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (Some(1), Some(Bracket::new(0, false)))));
        assert_eq!(t, Path::from_str("a-1-]0->b").unwrap());

        t = Path::new('a');
        t.add_edge(Edge::new('a', 'b', (Some(1), Some(Bracket::new(0, true)))));
        t.add_edge(Edge::new('b', 'c', (Some(2), Some(Bracket::new(1, false)))));
        assert_eq!(t, Path::from_str("a-1-[0->b-2-]1->c").unwrap());
    }

    #[test]
    fn get_w() {
        let mut t = Path::from_str("a-[0->b-]0->a").unwrap();
        assert_eq!(t.get_w(), 1);

        t = Path::from_str("a").unwrap();
        assert_eq!(t.get_w(), 0);

        t = Path::from_str("a->a").unwrap();
        assert_eq!(t.get_w(), 1);

        t = Path::from_str("a->a->a").unwrap();
        assert_eq!(t.get_w(), 2);

        t = Path::from_str("a-[0->b-[0->a").unwrap();
        assert_eq!(t.get_w(), 0);

        t = Path::from_str("a->b->c-[0->d->e-]0->c->a").unwrap();
        assert_eq!(t.get_w(), 1);
    }

    #[test]
    fn get_d() {
        assert_eq!(Path::from_str("a-[0->a->b-]0->b").unwrap().get_d(), 1);
        assert_eq!(Path::from_str("a->b").unwrap().get_d(), 0);
        assert_eq!(Path::from_str("a->a").unwrap().get_d(), 0);
        assert_eq!(Path::from_str("a->b-[0->c->b->d-]0->d").unwrap().get_d(), 1);
        assert_eq!(
            Path::from_str("a->b-[0->c->b->b->d-]0->d").unwrap().get_d(),
            1
        );
        assert_eq!(
            Path::from_str("a->b-[0->c->b-[0->b->d-]0->d-]0->e->d")
                .unwrap()
                .get_d(),
            2
        );
        assert_eq!(
            Path::from_str("a-[0->a->a->a->b-]0->b->c-[0->c-[0->c->e-]0->e->e-]0->f->e")
                .unwrap()
                .get_d(),
            2
        );
    }
}
