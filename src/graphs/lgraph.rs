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

    fn path_nests<'a>(
        &'a self,
        path: &Path<'a, N, Item<E>>,
    ) -> HashMap<(NodeRef<'a, N>, NodeRef<'a, N>), HashSet<(BracketStack, BracketStack)>> {
        let mems: Vec<_> = self.path_to_memories(path).collect();

        let mut left_pairs = vec![];
        let mut right_pairs = vec![];

        for (i, a) in mems.iter().enumerate() {
            for (j, other) in mems.iter().enumerate().skip(i + 1) {
                if other.node() == a.node() {
                    if other.stack().has_prefix(a.stack()) {
                        left_pairs.push((j, *a.node(), a.stack().clone(), other.stack().clone()));
                    }
                    break;
                }
            }
        }

        for (i, b) in mems.iter().enumerate() {
            for other in mems.iter().skip(i + 1) {
                if other.node() == b.node() {
                    if b.stack().has_prefix(other.stack()) {
                        right_pairs.push((i, *b.node(), b.stack().clone(), other.stack().clone()));
                    }
                    break;
                }
            }
        }

        let mut pairs: HashMap<(NodeRef<N>, NodeRef<N>), HashSet<(BracketStack, BracketStack)>> =
            HashMap::new();
        for ((j, a, as1, as1s2), (i, b, bs1s2, bs1)) in AllPairs::new(left_pairs, right_pairs) {
            if as1 == bs1 && as1s2 == bs1s2 && j < i {
                let cur_nest = pairs.entry((a, b)).or_default();
                cur_nest.insert((as1, as1s2));
            }
        }

        pairs
    }

    fn all_nests<'a, 'b>(
        &'a self,
        paths: impl Iterator<Item = &'b Path<'a, N, Item<E>>>,
    ) -> HashMap<
        (NodeRef<N>, NodeRef<N>),
        HashMap<(BracketStack, BracketStack), Vec<&'b Path<'a, N, Item<E>>>>,
    >
    where
        'a: 'b,
    {
        paths.map(|path| (self.path_nests(path), path)).fold(
            HashMap::new(),
            |mut all_nests, (path_nests, path)| {
                for (pair, instances) in path_nests {
                    let cur_nest = all_nests.entry(pair).or_default();
                    for (s1, s1s2) in instances {
                        let paths_with_nest = cur_nest.entry((s1, s1s2)).or_default();
                        paths_with_nest.push(path);
                    }
                }
                all_nests
            },
        )
    }

    fn prep_cores<'a>(
        &'a self,
    ) -> (
        Vec<Path<'a, N, Item<E>>>,
        Vec<Path<'a, N, Item<E>>>,
        Vec<Path<'a, N, Item<E>>>,
    )
    where
        N: Hash + Debug,
        E: Debug,
    {
        let mut d = 0;
        loop {
            d += 1;
            let core0_paths = self.stack_core(1, d).collect::<HashSet<_>>();
            if !self.nodes().all(|node| {
                core0_paths
                    .iter()
                    .any(|path| path.nodes().any(|n| n == node))
            }) {
                continue;
            }

            let core0_nests = self.all_nests(core0_paths.iter());
            let core0 = self.graph_from_paths(
                core0_paths.clone().into_iter(),
                &mut DefaultBuilder::default(),
            );

            println!("core0 d={d}");
            for ((a, b), instances) in &core0_nests {
                for (s1, s1s2) in instances.keys() {
                    println!(
                        "Nest {:?}-{:?}, {:?}-{:?}",
                        a.contents(),
                        b.contents(),
                        s1,
                        s1s2
                    );
                }
            }

            loop {
                d += 1;
                let core1_paths = self.stack_core(1, d).collect::<Vec<_>>();
                let dcore_paths = core1_paths
                    .clone()
                    .into_iter()
                    .filter(|path| !core0_paths.contains(path))
                    .collect::<Vec<_>>();
                let dcore_nests = self.all_nests(dcore_paths.iter());

                println!("core1 d={d}");
                for ((a, b), instances) in &dcore_nests {
                    for (s1, s1s2) in instances.keys() {
                        println!(
                            "Nest {:?}-{:?}, {:?}-{:?}",
                            a.contents(),
                            b.contents(),
                            s1,
                            s1s2
                        );
                    }
                }

                if d > 20 {
                    panic!()
                }

                let mut all_increased = true;
                for (nest, instances) in &dcore_nests {
                    let core0_instances: HashSet<_> = core0_nests
                        .get(nest)
                        .map(|i| i.keys().collect())
                        .unwrap_or_default();
                    let dcore_instances: HashSet<_> = instances.keys().collect();
                    let gained = dcore_instances
                        .difference(&core0_instances)
                        .collect::<Vec<_>>();
                    if gained.is_empty() {
                        all_increased = false;
                        break;
                    }

                    for (_, s1s2) in gained {
                        if core0
                            .node_with_contents(&Memory {
                                node: nest.0.contents().clone(),
                                stack: s1s2.clone(),
                            })
                            .is_some()
                        {
                            println!("Node {:?}{:?} in core0", nest.0.contents(), s1s2);
                            all_increased = false;
                            break;
                        }
                        if core0
                            .node_with_contents(&Memory {
                                node: nest.1.contents().clone(),
                                stack: s1s2.clone(),
                            })
                            .is_some()
                        {
                            println!("Node {:?}{:?} in core0", nest.1.contents(), s1s2);
                            all_increased = false;
                            break;
                        }
                    }
                }

                if !all_increased {
                    continue;
                }

                return (core0_paths.into_iter().collect(), core1_paths, dcore_paths);
            }
        }
    }

    fn nest_loops<'a>(
        &'a self,
        path: &Path<'a, N, Item<E>>,
        a: NodeRef<'a, N>,
        b: NodeRef<'a, N>,
        s1: &BracketStack,
        s1s2: &BracketStack,
        mangled_count: &mut usize,
    ) -> Vec<(Mangled<Memory<N>>, Item<E>, Mangled<Memory<N>>)> {
        let mut transformed = vec![];

        let mems: Vec<_> = self.path_to_memories(path).collect();

        let mut p0 = vec![];
        let mut p1 = vec![];
        let mut mangled_p1 = vec![];
        let mut p2 = vec![];
        let mut p3 = vec![];
        let mut mangled_p3 = vec![];
        let mut p4 = vec![];
        enum Stage {
            BeforeLeft,
            InLeft,
            Middle,
            InRight,
            AfterRight,
        }

        let mut stage = Stage::BeforeLeft;
        // let mut mangled_names = (*mangled_count..).flat_map(|m| once(m).chain(once(m)));

        let mut mangled_from = *mangled_count;
        let mut mangled_to = *mangled_count;
        for (from, edge, to) in path
            .edges()
            .enumerate()
            .map(|(i, e)| (&mems[i], e, &mems[i + 1]))
        {
            match stage {
                Stage::BeforeLeft => {
                    p0.push((
                        Mangled::Node(from.deref()),
                        edge.contents().clone(),
                        Mangled::Node(to.deref()),
                    ));
                    if to.stack() == s1 && to.node() == &a {
                        stage = Stage::InLeft;
                    }
                }
                Stage::InLeft => {
                    p1.push((
                        Mangled::Node(from.deref()),
                        edge.contents().clone(),
                        Mangled::Node(to.deref()),
                    ));
                    if to.stack() == s1s2 && to.node() == &a {
                        if !(from.stack() == s1 && to.node() == &a) {
                            mangled_p1.push((
                                Mangled::Mangled(mangled_from),
                                edge.contents().clone(),
                                Mangled::Node(Memory {
                                    node: a.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                            ))
                        } else {
                            mangled_p1.push((
                                Mangled::Node(Memory {
                                    node: a.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                                edge.contents().clone(),
                                Mangled::Node(Memory {
                                    node: a.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                            ))
                        }

                        stage = Stage::Middle;
                        continue;
                    }

                    if from.stack() == s1 && from.node() == &a {
                        mangled_p1.push((
                            Mangled::Node(Memory {
                                node: a.contents().clone(),
                                stack: s1s2.clone(),
                            }),
                            edge.contents().clone(),
                            Mangled::Mangled(mangled_to),
                        ));
                        mangled_from = mangled_to;
                        mangled_to += 1;
                    } else {
                        mangled_p1.push((
                            Mangled::Mangled(mangled_from),
                            edge.contents().clone(),
                            Mangled::Mangled(mangled_to),
                        ));
                        mangled_from = mangled_to;
                        mangled_to += 1;
                    }
                }
                Stage::Middle => {
                    p2.push((
                        Mangled::Node(from.deref()),
                        edge.contents().clone(),
                        Mangled::Node(to.deref()),
                    ));
                    if to.stack() == s1s2 && to.node() == &b {
                        stage = Stage::InRight;
                    }
                }
                Stage::InRight => {
                    p3.push((
                        Mangled::Node(from.deref()),
                        edge.contents().clone(),
                        Mangled::Node(to.deref()),
                    ));
                    if to.stack() == s1 && to.node() == &b {
                        if !(from.stack() == s1s2 && to.node() == &b) {
                            mangled_p3.push((
                                Mangled::Mangled(mangled_from),
                                edge.contents().clone(),
                                Mangled::Node(Memory {
                                    node: b.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                            ))
                        } else {
                            mangled_p3.push((
                                Mangled::Node(Memory {
                                    node: b.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                                edge.contents().clone(),
                                Mangled::Node(Memory {
                                    node: b.contents().clone(),
                                    stack: s1s2.clone(),
                                }),
                            ))
                        }

                        stage = Stage::AfterRight;
                        continue;
                    }

                    if from.stack() == s1s2 && from.node() == &b {
                        mangled_p3.push((
                            Mangled::Node(Memory {
                                node: b.contents().clone(),
                                stack: s1s2.clone(),
                            }),
                            edge.contents().clone(),
                            Mangled::Mangled(mangled_to),
                        ));
                        mangled_from = mangled_to;
                        mangled_to += 1;
                    } else {
                        mangled_p3.push((
                            Mangled::Mangled(mangled_from),
                            edge.contents().clone(),
                            Mangled::Mangled(mangled_to),
                        ));
                        mangled_from = mangled_to;
                        mangled_to += 1;
                    }
                }
                Stage::AfterRight => {
                    p4.push((
                        Mangled::Node(from.deref()),
                        edge.contents().clone(),
                        Mangled::Node(to.deref()),
                    ));
                }
            }
        }

        *mangled_count = mangled_to;
        // p0 -> (a, s1) -> p1 -> (a, s1s2) -> p2 -> (b, s1s2) -> p3 -> (b, s1) -> p4
        // =>
        // p0 -> (a, s1) -> p1 -> (a, s1s2) -> mangle(p1) -> (a, s1s2) ->
        //    -> p2 -> (b, s1s2) -> mangle(p3) -> (b, s1s2) -> p3 -> (b, s1) -> p4
        transformed.append(&mut p0);
        transformed.append(&mut p1);
        transformed.append(&mut mangled_p1);
        transformed.append(&mut p2);
        transformed.append(&mut mangled_p3);
        transformed.append(&mut p3);
        transformed.append(&mut p4);

        transformed
    }

    pub fn normal_form<B>(&self, builder: &mut B) -> LGraph<Mangled<Memory<N>>, E, B::TargetGraph>
    where
        B: Builder<Mangled<Memory<N>>, Item<E>>,
        N: Hash,
        N: Debug,
        E: Debug,
    {
        let (core0_paths, core1_paths, dcore_paths) = self.prep_cores();
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

        let dcore_nests = self.all_nests(dcore_paths.iter());
        let core0_nests = self.all_nests(core0_paths.iter());

        let mut mangled_count = 0;
        for (nest, dcore_instances) in &dcore_nests {
            let core0_instances = core0_nests.get(nest);
            let gained_nests = dcore_instances
                .iter()
                .filter(|(stacks, _)| core0_instances.map_or(true, |c| !c.contains_key(stacks)));

            let (a, b) = nest;
            for ((s1, s1s2), paths) in gained_nests {
                for path in paths {
                    let new_edges = self.nest_loops(path, *a, *b, s1, s1s2, &mut mangled_count);

                    for (from, edge, to) in new_edges {
                        builder.add_edge(from, edge, to);
                    }
                }
            }
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
pub enum Mangled<N> {
    Node(N),
    Mangled(usize),
}

impl<N> Display for Mangled<N>
where
    N: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mangled::Node(node) => write!(f, "{}", node),
            Mangled::Mangled(i) => write!(f, "\'{}\'", i),
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
        refs::Path,
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
        // let edges = [
        //     edge(1, None, 1, true, 2),
        //     edge(2, Some('a'), 2, true, 3),
        //     edge(3, None, 3, true, 4),
        //     edge(4, None, 3, false, 2),
        //     edge(2, None, 2, false, 5),
        //     edge(5, None, 2, false, 6),
        //     edge(6, None, 2, false, 7),
        //     edge(7, None, 2, false, 7),
        //     edge(7, None, 2, false, 8),
        //     edge(8, None, 2, false, 9),
        //     edge(9, None, 2, false, 10),
        //     edge(10, None, 2, false, 11),
        //     edge(11, None, 3, true, 12),
        //     edge(12, None, 3, false, 10),
        //     edge(10, None, 1, false, 13),
        // ];
        let edges = [
            edge(1, None, 1, true, 2),
            edge(2, Some('a'), 2, true, 3),
            edge(3, None, 3, true, 4),
            edge(4, None, 3, false, 2),
            edge(2, None, 2, false, 7),
            // edge(5, None, 2, false, 6),
            // edge(6, None, 2, false, 7),
            edge(7, None, 2, false, 7),
            edge(7, None, 2, false, 10),
            // edge(8, None, 2, false, 9),
            // edge(9, None, 2, false, 10),
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
