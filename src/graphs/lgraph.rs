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

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone,
    E: Eq + Clone,
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

    fn is_in_core<'a>(&'a self, path: &Path<'a, N, Item<E>>, w: usize, d: usize) -> bool
    where
        N: Hash,
    {
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

    pub fn stack_core(&self, w: usize, d: usize) -> impl Iterator<Item = Path<'_, N, Item<E>>> + '_
    where
        N: Hash,
    {
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

        LGraph::new(builder.build(
            Memory {
                node: self.start_node().contents().clone(),
                stack: BracketStack::default(),
            },
            end_nodes,
        ))
    }

    pub fn stack_core_graph<B>(
        &self,
        w: usize,
        d: usize,
        builder: &mut B,
    ) -> LGraph<Memory<N>, E, B::TargetGraph>
    where
        B: Builder<Memory<N>, Item<E>>,
        N: Hash,
    {
        let paths = self.stack_core(w, d);
        self.graph_from_paths(paths, builder)
    }

    pub fn delta_stack_core_graph<B>(
        &self,
        w: usize,
        d: usize,
        builder: &mut B,
    ) -> LGraph<Memory<N>, E, B::TargetGraph>
    where
        B: Builder<Memory<N>, Item<E>>,
        N: Hash,
    {
        let prev_paths: HashSet<_> = self.stack_core(w, d - 1).collect();
        let paths = self.stack_core(w, d).filter(|p| !prev_paths.contains(p));
        self.graph_from_paths(paths, builder)
    }

    fn path_balance<'a>(&'a self, path: &Path<'a, N, Item<E>>) -> Option<BracketStack> {
        path.edges()
            .fold(Some(BracketStack::default()), |stack, e| {
                stack.and_then(|s| s.accept_clone(e.contents().bracket()))
            })
    }

    fn path_loop_nests<'a>(
        &'a self,
        path: &Path<'a, N, Item<E>>,
    ) -> Vec<(usize, usize, usize, usize)> {
        let mut loops = vec![];
        for (i, a) in path.nodes().enumerate() {
            for (j, b) in path.nodes().enumerate().skip(i + 1) {
                if b == a {
                    loops.push((i, j));
                }
            }
        }

        let mut nests = vec![];
        for ((a1, a2), (b1, b2)) in
            AllPairs::new(loops.iter(), loops.iter()).filter(|((_, a2), (b1, _))| a2 < b1)
        {
            let full = Path::new(path[*a1..*b2].to_vec());
            let mid = Path::new(path[*a2..*b1].to_vec());

            if self.path_balance(&full).map_or(false, |s| s.is_empty())
                && self.path_balance(&mid).map_or(false, |s| s.is_empty())
            {
                nests.push((*a1, *a2, *b1, *b2))
            }
        }
        nests
    }

    fn core0<'a>(&'a self) -> (usize, Vec<Path<'a, N, Item<E>>>)
    where
        N: Hash,
    {
        for d in 0.. {
            let core: Vec<_> = self.stack_core(1, d).collect();
            if self
                .nodes()
                .all(|node| core.iter().any(|path| path.nodes().any(|n| n == node)))
            {
                return (d, self.stack_core(1, d + 1).collect());
            }
        }

        unreachable!()
    }

    fn core1<'a, 'b>(
        &'a self,
        core0: impl Iterator<Item = Path<'a, N, Item<E>>>,
        d: usize,
    ) -> (
        Vec<Path<'a, N, Item<E>>>,
        Vec<(Path<N, Item<E>>, usize, usize, usize, usize)>,
    )
    where
        'a: 'b,
        N: Hash,
        N: Debug,
    {
        let core0_paths: HashSet<_> = core0.collect();
        let core0 = self.graph_from_paths(
            core0_paths.clone().into_iter(),
            &mut DefaultBuilder::default(),
        );
        let core0_nests: Vec<_> = core0_paths
            .iter()
            .flat_map(|p| {
                self.path_loop_nests(p)
                    .into_iter()
                    .map(|(a1, a2, b1, b2)| (p.clone(), a1, a2, b1, b2))
            })
            .collect();

        let mut core0_unique_nests = HashSet::new();
        for (path, a1, a2, b1, b2) in core0_nests {
            core0_unique_nests.insert((
                Path::new(path[a1..a2].to_vec()),
                Path::new(path[a2..b1].to_vec()),
                Path::new(path[b1..b2].to_vec()),
            ));
        }
        let print_nest = |nest: &(
            Path<'a, N, Item<E>>,
            Path<'a, N, Item<E>>,
            Path<'a, N, Item<E>>,
        )| {
            use std::fmt::Write;
            let mut s = String::new();
            let (left, mid, right) = nest;
            write!(
                s,
                "{:?} - ",
                left.nodes().map(|n| n.contents()).collect::<Vec<_>>()
            )
            .unwrap();
            write!(
                s,
                "{:?}",
                mid.nodes().map(|n| n.contents()).collect::<Vec<_>>()
            )
            .unwrap();
            write!(
                s,
                " - {:?}",
                right.nodes().map(|n| n.contents()).collect::<Vec<_>>()
            )
            .unwrap();
            s
        };

        #[cfg(test)]
        {
            println!("core0 nests:");
            for nest in &core0_unique_nests {
                println!("{}", print_nest(nest));
            }
        }

        'core1_d_loop: for d in d + 1.. {
            let core1_paths: Vec<_> = self.stack_core(1, d).collect();
            let dcore_paths = core1_paths.iter().filter(|p| !core0_paths.contains(p));
            let dcore_nests: Vec<_> = dcore_paths
                .flat_map(|p| {
                    self.path_loop_nests(p)
                        .into_iter()
                        .map(|(a1, a2, b1, b2)| (p.clone(), a1, a2, b1, b2))
                })
                .collect();

            if dcore_nests.is_empty() && !core0_unique_nests.is_empty() {
                continue;
            }

            let mut dcore_unique_nests = HashSet::new();
            for (path, a1, a2, b1, b2) in &dcore_nests {
                dcore_unique_nests.insert((
                    Path::new(path[*a1..*a2].to_vec()),
                    Path::new(path[*a2..*b1].to_vec()),
                    Path::new(path[*b1..*b2].to_vec()),
                ));
            }

            #[cfg(test)]
            {
                println!("d = {}, dcore nests:", d);
                for nest in &dcore_unique_nests {
                    println!("{}", print_nest(nest));
                }
            }

            for nest in &core0_unique_nests {
                if !dcore_unique_nests.contains(nest) {
                    println!("d={d} - Could not find core0 nest {}", print_nest(nest));
                    continue 'core1_d_loop;
                }
            }

            let mut dcore_nests_per_path: HashMap<_, HashSet<(usize, usize, usize, usize)>> =
                HashMap::new();
            for (path, a1, a2, b1, b2) in dcore_nests.clone().into_iter() {
                let path_nodes: Vec<_> = path.nodes().collect();
                let left = path_nodes[a1];
                let right = path_nodes[a2];

                let nests = dcore_nests_per_path
                    .entry((path.clone(), left, right))
                    .or_default();
                nests.insert((a1, a2, b1, b2));
            }

            let mut good_nests = Vec::new();
            for old_nest in &core0_unique_nests {
                let Some((path,a1,a2,b1,b2)) = dcore_nests.iter().find(|(path, a1, a2, b1, b2)| {
                    Path::new(path[*a1..*a2].to_vec()) == old_nest.0
                        && Path::new(path[*a2..*b1].to_vec()) == old_nest.1
                        && Path::new(path[*b1..*b2].to_vec()) == old_nest.2
                }) else {
                    continue 'core1_d_loop;
                };
                let mem: Vec<_> = self.path_to_memories(path).collect();

                if core0.node_with_contents(&mem[*a2].deref()).is_none()
                    && core0.node_with_contents(&mem[*b1].deref()).is_none()
                {
                    good_nests.push((path.clone(), *a1, *a2, *b1, *b2));
                }
            }

            return (core1_paths, good_nests);
        }

        unreachable!()
    }

    pub fn add_mangled_edges<'a, B>(
        &'a self,
        path: &Path<'a, N, Item<E>>,
        a1: usize,
        a2: usize,
        b1: usize,
        b2: usize,
        builder: &mut B,
        mangled_count: &mut usize,
    ) where
        B: Builder<Mangled<Memory<N>, usize>, Item<E>>,
        N: Hash,
        N: Display,
        E: Display,
    {
        let mem: Vec<_> = self.path_to_memories(path).collect();
        println!("{}", path);

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
        let (d, core0_paths) = self.core0();
        let (core1_paths, good_nests) = self.core1(core0_paths.clone().into_iter(), d);
        let core1 = self.graph_from_paths(core1_paths.into_iter(), &mut DefaultBuilder::default());

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
        for (path, a1, a2, b1, b2) in &good_nests {
            self.add_mangled_edges(path, *a1, *a2, *b1, *b2, builder, &mut mangled_count);
        }

        LGraph::new(
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
    state: Vec<(Path<'a, N, Item<E>>, NodeRef<'a, N>, BracketStack)>,
    w: usize,
    d: usize,
}

impl<'a, N, E, G> Iterator for StackCore<'a, N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Hash,
    E: Eq + Clone,
{
    type Item = Path<'a, N, Item<E>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((path, node, brackets)) = self.state.pop() {
            let mut res = None;

            if self.graph.is_end_node(node) && brackets.is_empty() && !path.is_empty() {
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
        // todo!("Broken");

        let edges = [
            edge(1, Some('a'), 1, true, 2),
            edge(2, Some('b'), 0, true, 2),
            edge(2, None, 0, false, 1),
            edge(1, Some('d'), 0, true, 3),
            edge(3, None, 0, false, 4),
            edge(4, None, 1, false, 4),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [4]))
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
            edge(1, Some('a'), 1, true, 2),
            edge(2, Some('b'), 0, true, 3),
            edge(3, None, 0, false, 2),
            edge(2, Some('c'), 2, true, 1),
            edge(1, Some('d'), 0, true, 4),
            edge(4, None, 0, false, 5),
            edge(5, None, 1, false, 5),
            edge(5, None, 2, false, 5),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [5]))
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

    fn example_9() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
        let edges = [
            edge(1, Some('a'), 1, true, 1),
            edge(1, Some('b'), 0, true, 2),
            edge(2, Some('c'), 2, true, 3),
            edge(3, None, 2, true, 4),
            edge(4, None, 2, true, 2),
            edge(2, Some('d'), 2, true, 5),
            edge(5, None, 2, false, 5),
            edge(5, None, 0, false, 6),
            edge(6, None, 1, false, 6),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        LGraph::new(builder.build(1, [6]))
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
        assert_eq!(test_graph(example_2()), 3)
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
    fn regularize_5() {
        assert_eq!(test_graph(example_5()), 5)
    }
    #[test]
    fn regularize_9() {
        assert_eq!(test_graph(example_9()), 4)
    }
}
