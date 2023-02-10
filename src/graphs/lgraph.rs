use std::{
    collections::HashMap,
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

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Memory<N> {
    node: N,
    stack: BracketStack,
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
struct Nest<'a, 'b, N, E> {
    left: (usize, usize),
    right: (usize, usize),
    path: &'b Path<'a, N, E>,
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

    fn core0(&self) -> (Vec<Path<N, Item<E>>>, usize)
    where
        N: Hash,
    {
        for d in 0.. {
            let paths: Vec<_> = StackCore::new(self, 1, d).collect();

            if self.nodes().all(|node| {
                paths
                    .iter()
                    .any(|(path, _, _)| path.nodes().any(|n| n == node))
            }) {
                return (
                    StackCore::new(self, 1, d + 1).map(|(p, _, _)| p).collect(),
                    d + 1,
                );
            }
        }

        unreachable!()
    }

    fn core1(&self, core0_paths: &[Path<N, Item<E>>], d: usize) -> Vec<Path<N, Item<E>>>
    where
        N: Hash,
    {
        let longest_path_len = core0_paths
            .iter()
            .map(|p| p.len())
            .max()
            .unwrap_or_default();

        for stack_depth in d.. {
            dbg!(stack_depth);
            let core1_paths: Vec<_> = StackCore::new(self, 1, stack_depth).collect();

            if core1_paths
                .iter()
                .map(|(p, _, _)| p)
                .filter_map(|p| {
                    let nests = self.path_nests(p);
                    if nests.values().any(|count| *count > longest_path_len) || nests.is_empty() {
                        None
                    } else {
                        Some(nests)
                    }
                })
                .all(|nests| nests.into_values().all(|count| count == longest_path_len))
            {
                return core1_paths.into_iter().map(|(p, _, _)| p).collect();
            }

            // if core1_paths
            //     .iter()
            //     .filter(|(_, _, d)| stack_depth == *d)
            //     .map(|(p, _, _)| p)
            //     .all(|p| {
            //         let nests = self.path_nests(p);
            //         nests.values().all(|count| *count > longest_path_len)
            //     })
            // {
            //     return core1_paths.into_iter().map(|(p, _, _)| p).collect();
            // }
        }

        todo!()
    }

    fn meaningful_nests<'a, 'b>(
        &'a self,
        core0_paths: &[Path<'a, N, Item<E>>],
        core1_paths: &'b [Path<'a, N, Item<E>>],
    ) -> Vec<Nest<'a, 'b, N, Item<E>>> {
        let core0 =
            self.graph_from_paths(core0_paths.iter().cloned(), &mut DefaultBuilder::default());

        let mut nests = vec![];
        for path in core1_paths {
            let mem: Vec<_> = self.path_to_memories(path).collect();
            for (nest, _) in self.path_nests(path) {
                let left_end = &mem[nest.left.1];
                let right_start = &mem[nest.right.0];
                if core0.node_with_contents(&left_end.deref()).is_none()
                    && core0.node_with_contents(&right_start.deref()).is_none()
                {
                    nests.push(nest)
                }
            }
        }

        nests
    }

    // pub fn core(&self, d: usize) -> Vec<Path<N, Item<E>>>
    // where
    //     N: Hash,
    // {
    //     todo!()
    // }

    fn graph_from_paths<'a, B>(
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

    fn path_balance<'a>(&'a self, path: &Path<'a, N, Item<E>>) -> Option<BracketStack> {
        path.edges()
            .fold(Some(BracketStack::default()), |stack, e| {
                stack.and_then(|s| s.accept_clone(e.contents().bracket()))
            })
    }

    fn path_nests<'a, 'b>(
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

    fn add_nest<'a, 'b, B>(
        &'a self,
        nest: Nest<'a, 'b, N, Item<E>>,
        builder: &mut B,
        mangled_count: &mut usize,
    ) where
        B: Builder<Mangled<Memory<N>, usize>, Item<E>>,
        N: Hash,
        N: Debug,
        E: Debug,
    {
        let (_, nest) = nest.minimize();

        let path = nest.path;
        let (a1, a2) = nest.left;
        let (b1, b2) = nest.right;

        let mem: Vec<_> = self.path_to_memories(path).collect();

        let mut translation: HashMap<&Memory<NodeRef<N>>, Mangled<Memory<N>, usize>> =
            HashMap::from([
                (&mem[a1], Mangled::Node(mem[a2].deref())),
                (&mem[a2], Mangled::Node(mem[a2].deref())),
            ]);

        for (i, edge) in path
            .edges()
            .enumerate()
            .filter(|(i, _)| *i >= a1 && *i < a2)
        {
            let src = translation
                .entry(&mem[i])
                .or_insert_with(|| {
                    *mangled_count += 1;
                    Mangled::Mangled(*mangled_count - 1)
                })
                .clone();
            let dst = translation
                .entry(&mem[i + 1])
                .or_insert_with(|| {
                    *mangled_count += 1;
                    Mangled::Mangled(*mangled_count - 1)
                })
                .clone();
            builder.add_edge(src, edge.contents().clone(), dst);
        }

        let mut translation: HashMap<&Memory<NodeRef<N>>, Mangled<Memory<N>, usize>> =
            HashMap::from([
                (&mem[b1], Mangled::Node(mem[b1].deref())),
                (&mem[b2], Mangled::Node(mem[b1].deref())),
            ]);

        for (i, edge) in path
            .edges()
            .enumerate()
            .filter(|(i, _)| *i >= b1 && *i < b2)
        {
            let src = translation
                .entry(&mem[i])
                .or_insert_with(|| {
                    *mangled_count += 1;
                    Mangled::Mangled(*mangled_count - 1)
                })
                .clone();
            let dst = translation
                .entry(&mem[i + 1])
                .or_insert_with(|| {
                    *mangled_count += 1;
                    Mangled::Mangled(*mangled_count - 1)
                })
                .clone();
            builder.add_edge(src, edge.contents().clone(), dst);
        }
    }

    pub fn normal_form<B>(
        &self,
        builder: &mut B,
    ) -> LGraph<Mangled<Memory<N>, usize>, E, B::TargetGraph>
    where
        B: Builder<Mangled<Memory<N>, usize>, Item<E>>,
        N: Hash,
        N: Display + Debug,
        E: Display + Debug,
    {
        let (core0_paths, d) = self.core0();
        let core1_paths = self.core1(&core0_paths, d);
        let nests = self.meaningful_nests(&core0_paths, &core1_paths);

        let core1 = self.graph_from_paths(
            core1_paths.clone().into_iter(),
            &mut DefaultBuilder::default(),
        );

        builder.clear();
        for edge in core1.edges() {
            builder.add_edge(
                Mangled::Node(edge.source().contents().clone()),
                edge.contents().clone(),
                Mangled::Node(edge.target().contents().clone()),
            );
        }
        for node in core1.nodes() {
            builder.add_node(Mangled::Node(node.contents().clone()));
        }

        let mut mangled_count = 0;
        for nest in nests {
            self.add_nest(nest, builder, &mut mangled_count);
        }

        LGraph::new_unchecked(
            builder.build(
                Mangled::Node(core1.start_node().contents().clone()),
                core1
                    .end_nodes()
                    .map(|n| n.contents().clone())
                    .map(Mangled::Node),
            ),
        )
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

pub struct StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
{
    graph: &'a LGraph<N, E, G>,
    #[allow(clippy::type_complexity)]
    state: Vec<(Path<'a, N, Item<E>>, NodeRef<'a, N>, BracketStack)>,
    w: usize,
    d: usize,
}

impl<'a, N, E, G> StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Debug,
    E: Debug,
{
    pub fn new(graph: &'a LGraph<N, E, G>, w: usize, d: usize) -> Self {
        Self {
            state: vec![(Path::default(), graph.start_node(), BracketStack::default())],
            graph,
            w,
            d,
        }
    }

    fn is_in_core(&self, path: &Path<'a, N, Item<E>>, w: usize, d: usize) -> bool
    where
        N: Hash + Eq + Clone,
        E: Eq + Clone,
    {
        let mut visited: HashMap<_, usize> = HashMap::new();

        for mem in self.graph.path_to_memories(path) {
            if mem.stack.len() > d {
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

    fn get_wd(&self, path: &Path<'a, N, Item<E>>) -> (usize, usize)
    where
        N: Hash + Eq + Clone,
        E: Eq + Clone,
    {
        let mut visited: HashMap<_, usize> = HashMap::new();

        let mut max_d = 0;
        let mut max_w = 0;
        for mem in self.graph.path_to_memories(path) {
            if mem.stack.len() > max_d {
                max_d = mem.stack.len();
            }

            let count = visited
                .entry(mem)
                .and_modify(|count| *count += 1)
                .or_default();
            if *count > max_w {
                max_w = *count;
            }
        }

        (max_w, max_d)
    }
}

impl<'a, N, E, G> Iterator for StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Hash + Debug,
    E: Eq + Clone + Debug,
{
    type Item = (Path<'a, N, Item<E>>, usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((path, node, brackets)) = self.state.pop() {
            let mut res = None;

            if self.graph.is_end_node(node) && brackets.is_empty() && !path.is_empty() {
                res = Some(path.clone());
            }

            for edge in self.graph.edges_from(node) {
                if let Some(new_brackets) = brackets.accept_clone(edge.contents().bracket()) {
                    let new_path = path.clone().push(edge);

                    if self.is_in_core(&new_path, self.w, self.d) {
                        self.state.push((new_path, edge.target(), new_brackets))
                    }
                }
            }

            if let Some(res) = res {
                let (w, d) = self.get_wd(&res);
                return Some((res, w, d));
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
