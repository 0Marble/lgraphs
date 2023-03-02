use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    iter::once,
    marker::PhantomData,
};

use super::{
    graph_trait::{Builder, Graph},
    iters::AllPairs,
    refs::{EdgeRef, NodeRef, Path},
    state_machine::StateMachine,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bracket {
    index: usize,
    is_open: bool,
}

impl Bracket {
    pub fn new(index: usize, is_open: bool) -> Self {
        Self { index, is_open }
    }

    pub fn is_open(&self) -> bool {
        self.is_open
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Default, Hash, Clone, PartialEq, Eq)]
pub struct BracketStack {
    stack: Vec<usize>,
}

impl BracketStack {
    pub fn brackets(&self) -> impl Iterator<Item = Bracket> + '_ {
        self.stack.iter().cloned().map(|i| Bracket::new(i, true))
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
        self.stack.len() > prefix.stack.len()
            && self.stack.strip_prefix(prefix.stack.as_slice()).is_some()
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
        match self.bracket.is_open() {
            true => write!(f, "[{}", self.bracket.index),
            false => write!(f, "]{}", self.bracket.index),
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

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Memory<N> {
    node: N,
    stack: BracketStack,
}

impl<N> Memory<N> {
    pub fn new(node: N, stack: BracketStack) -> Self {
        Self { node, stack }
    }
}

impl<'a, N> Memory<&'a N> {
    pub fn deref(&self) -> Memory<N>
    where
        N: Clone,
    {
        Memory {
            node: self.node.clone(),
            stack: self.stack.clone(),
        }
    }
}

impl<'a, N> Memory<NodeRef<'a, N>> {
    pub fn deref(&self) -> Memory<N>
    where
        N: Clone,
    {
        Memory {
            node: self.node.contents().clone(),
            stack: self.stack.clone(),
        }
    }
}

impl<N> Display for Memory<N>
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

impl<N> Memory<N> {
    pub fn node(&self) -> &N {
        &self.node
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

#[derive(Debug)]
pub struct Nest<'a, 'b, N, E> {
    pub left: (usize, usize),
    pub right: (usize, usize),
    pub path: &'b Path<'a, N, E>,
}

impl<'a, 'b, N, E> Clone for Nest<'a, 'b, N, E> {
    fn clone(&self) -> Self {
        Self {
            left: self.left,
            right: self.right,
            path: self.path,
        }
    }
}

impl<'a, 'b, N, E> Hash for Nest<'a, 'b, N, E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.left.hash(state);
        self.right.hash(state);
        self.path.hash(state);
    }
}

impl<'a, 'b, N, E> Eq for Nest<'a, 'b, N, E> {}

impl<'a, 'b, N, E> PartialEq for Nest<'a, 'b, N, E> {
    fn eq(&self, other: &Self) -> bool {
        self.left == other.left && self.right == other.right && self.path == other.path
    }
}

impl<'a, 'b, N, E> Nest<'a, 'b, N, E>
where
    N: Debug,
    E: Debug,
{
    pub fn contains(&self, other: &Self) -> bool {
        if self.path != other.path {
            return false;
        }

        let (a11, a21) = self.left;
        let (a12, a22) = other.left;
        let (b11, b21) = self.right;
        let (b12, b22) = other.right;

        // fully inside
        // left the same, right inside
        // right the same, left inside

        (a21 >= a12 && b22 <= b11)
            || (a11 == a12 && a22 == a21 && b22 <= b11)
            || (a12 >= a21 && b22 == b21 && b12 == b11)
    }

    pub fn minimize(&self) -> (usize, Self) {
        let (a1, a2) = self.left;
        let (b1, b2) = self.right;
        let left = &self.path[a1..a2];
        let right = &self.path[b1..b2];
        let mid = &self.path[a2..b1];

        let mut left_min = None;
        'find_left_min: for candidate in (1..left.len())
            .filter(|end| left[0] == left[*end])
            .map(|end| &left[0..end])
        {
            let mut remaining = left;
            loop {
                if let Some(next) = remaining.strip_prefix(candidate) {
                    remaining = next;
                    if remaining.is_empty() {
                        left_min = Some(candidate);
                        break 'find_left_min;
                    }
                } else {
                    continue 'find_left_min;
                }
            }
        }
        let left_min = left_min.unwrap_or(left);

        let end = right.len() - 1;
        let mut right_min = None;
        'find_right_min: for candidate in (0..=end)
            .rev()
            .filter(|start| right[*start] == right[end])
            .map(|start| &right[start..=end])
        {
            let mut remaining = right;
            loop {
                if let Some(next) = remaining.strip_suffix(candidate) {
                    remaining = next;
                    if remaining.is_empty() {
                        right_min = Some(candidate);
                        break 'find_right_min;
                    }
                } else {
                    continue 'find_right_min;
                }
            }
        }
        let right_min = right_min.unwrap_or(right);

        let mut left_count = 0;
        let mut remaining = mid;
        while let Some(next) = remaining.strip_prefix(left_min) {
            left_count += 1;
            remaining = next;
        }
        while let Some(next) = remaining.strip_suffix(right_min) {
            remaining = next;
        }
        let mid_min = remaining;
        left_count += left.len() / left_min.len();

        (
            left_count,
            Self {
                left: (
                    a1 + (left_count - 1) * left_min.len(),
                    a1 + left_count * left_min.len(),
                ),
                right: (
                    a1 + left_count * left_min.len() + mid_min.len(),
                    a1 + left_count * left_min.len() + mid_min.len() + right_min.len(),
                ),
                path: self.path,
            },
        )
    }
}

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Debug,
    E: Eq + Clone + Debug,
{
    pub fn new_unchecked(graph: G) -> Self {
        Self {
            graph,
            _p: PhantomData,
        }
    }

    fn path_to_memories<'a, 'b>(
        &'a self,
        path: &'b Path<'a, N, Item<E>>,
    ) -> impl Iterator<Item = Memory<NodeRef<'a, N>>> + 'b
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

    pub fn depth_set(&self, w: usize, d: usize) -> impl Iterator<Item = Path<'_, N, Item<E>>> + '_
    where
        N: Hash,
    {
        DepthSet::new(self, w, d)
    }

    pub fn is_in_core_w_checked(&self, path: &Path<N, Item<E>>, d: usize) -> bool {
        let loops: Vec<_> = path.loops().collect();

        let mut nests = vec![];
        for ((a1, a2), (b1, b2)) in
            AllPairs::new(loops.iter(), loops.iter()).filter(|((_, a2), (b1, _))| a2 < b1)
        {
            let full = Path::new(path[*a1..*b2].to_vec());
            let mid = Path::new(path[*a2..*b1].to_vec());

            if self.path_balance(&full).map_or(false, |s| s.is_empty())
                && self.path_balance(&mid).map_or(false, |s| s.is_empty())
            {
                nests.push(Nest {
                    left: (*a1, *a2),
                    right: (*b1, *b2),
                    path,
                })
            }
        }

        for (_, outer_nest) in nests.iter().enumerate() {
            let mut depth = 1;
            for inner_nest in nests.iter() {
                if outer_nest.contains(inner_nest) {
                    depth += 1;
                }
            }
            if depth > d {
                return false;
            }
        }
        true
    }

    pub fn core(
        &self,
        w: usize,
        d: usize,
        d_helper: Option<usize>,
    ) -> impl Iterator<Item = Path<N, Item<E>>> + '_
    where
        N: Hash,
        E: Display,
        N: Display,
    {
        self.depth_set(
            w,
            d_helper.unwrap_or((d + 1) * self.node_count() * self.node_count()),
        )
        .filter(move |p| self.is_in_core_w_checked(p, d))
        .map(|p| {
            println!("{}", p.print());
            p
        })
    }

    pub fn graph_from_paths_unchecked<'a, B>(
        &'a self,
        paths: impl Iterator<Item = Path<'a, N, Item<E>>>,
        builder: &mut B,
    ) -> LGraph<Memory<N>, E, B::TargetGraph>
    where
        B: Builder<Memory<N>, Item<E>>,
    {
        builder.clear();
        let mut end_nodes = vec![];
        for path in paths {
            let mem: Vec<_> = self.path_to_memories(&path).collect();
            if mem.is_empty() {
                continue;
            }

            for (i, edge) in path.edges().enumerate() {
                builder.add_edge(mem[i].deref(), edge.contents().clone(), mem[i + 1].deref());
            }

            end_nodes.push(mem.last().cloned().unwrap().deref());
        }

        LGraph::new_unchecked(builder.build(
            Memory {
                node: self.start_node().contents().clone(),
                stack: BracketStack::default(),
            },
            end_nodes,
        ))
    }

    pub fn path_balance<'a>(&'a self, path: &Path<'a, N, Item<E>>) -> Option<BracketStack> {
        path.edges()
            .fold(Some(BracketStack::default()), |stack, e| {
                stack.and_then(|s| s.accept_clone(e.contents().bracket()))
            })
    }

    pub fn path_nests<'a, 'b>(
        &'a self,
        path: &'b Path<'a, N, Item<E>>,
    ) -> HashMap<Nest<'a, 'b, N, Item<E>>, usize> {
        let loops: Vec<_> = path.loops().collect();

        let mut nests = vec![];
        for ((a1, a2), (b1, b2)) in
            AllPairs::new(loops.iter(), loops.iter()).filter(|((_, a2), (b1, _))| a2 < b1)
        {
            let full = Path::new(path[*a1..*b2].to_vec());
            let mid = Path::new(path[*a2..*b1].to_vec());

            if self.path_balance(&full).map_or(false, |s| s.is_empty())
                && self.path_balance(&mid).map_or(false, |s| s.is_empty())
            {
                nests.push(Nest {
                    left: (*a1, *a2),
                    right: (*b1, *b2),
                    path,
                })
            }
        }

        let mut min_nests: HashMap<_, _> = HashMap::new();
        for nest in nests {
            let (count, min) = nest.minimize();
            let cur_max_instance = min_nests.entry(min).or_insert((count, nest.clone()));
            if cur_max_instance.0 < count {
                *cur_max_instance = (count, nest);
            }
        }

        min_nests
            .into_iter()
            .map(|(nest, (count, _))| (nest, count))
            .collect()
    }

    pub fn normal_form<B>(&self, _: &mut B) -> LGraph<Mangled<Memory<N>, usize>, E, B::TargetGraph>
    where
        B: Builder<Mangled<Memory<N>, usize>, Item<E>>,
        N: Hash,
        N: Debug,
        E: Debug,
    {
        todo!()
    }

    pub fn regular_image<B>(&self, builder: &mut B) -> StateMachine<N, Option<E>, B::TargetGraph>
    where
        B: Builder<N, Option<E>>,
    {
        builder.clear();

        for edge in self.edges() {
            builder.add_edge(
                edge.source().contents().clone(),
                edge.contents().item().clone(),
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

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Mangled<N, A> {
    Node(N),
    Mangled(A),
}

impl<N, A> Display for Mangled<N, A>
where
    N: Display,
    A: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mangled::Node(node) => write!(f, "{}", node),
            Mangled::Mangled(alt) => write!(f, "\'{}\'", alt),
        }
    }
}

pub struct DepthSet<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
{
    graph: &'a LGraph<N, E, G>,
    #[allow(clippy::type_complexity)]
    state: Vec<(
        Path<'a, N, Item<E>>,
        NodeRef<'a, N>,
        BracketStack,
        HashMap<Memory<NodeRef<'a, N>>, usize>,
    )>,
    w: usize,
    d: usize,
}

impl<'a, N, E, G> DepthSet<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Debug,
    E: Debug,
{
    pub fn new(graph: &'a LGraph<N, E, G>, w: usize, d: usize) -> Self {
        Self {
            state: vec![(
                Path::default(),
                graph.start_node(),
                BracketStack::default(),
                HashMap::new(),
            )],
            graph,
            w,
            d,
        }
    }
}

impl<'a, N, E, G> Iterator for DepthSet<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Hash + Debug,
    E: Eq + Clone + Debug,
{
    type Item = Path<'a, N, Item<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((path, node, brackets, visit_stats)) = self.state.pop() {
            let mut res = None;

            if self.graph.is_end_node(node) && brackets.is_empty() && !path.is_empty() {
                res = Some(path.clone());
            }

            'next_search: for edge in self.graph.edges_from(node) {
                if let Some(new_brackets) = brackets.accept_clone(edge.contents().bracket()) {
                    if new_brackets.len() > self.d {
                        continue 'next_search;
                    }
                    let mut new_visit_stats = visit_stats.clone();
                    let mem = Memory {
                        node: edge.target(),
                        stack: new_brackets.clone(),
                    };
                    let count = new_visit_stats
                        .entry(mem)
                        .and_modify(|count| *count += 1)
                        .or_default();
                    if *count > self.w {
                        continue 'next_search;
                    }

                    let new_path = path.clone().push(edge);

                    self.state
                        .push((new_path, edge.target(), new_brackets, new_visit_stats))
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
