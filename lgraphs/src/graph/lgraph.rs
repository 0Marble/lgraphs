use std::marker::PhantomData;

use crate::path::{Bracket, BracketStack, Letter, Memory, Node, Path};

use super::{Graph, ModifyableGraph};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct LGraphLetter<L>
where
    L: Letter,
{
    letter: Option<L>,
    bracket: Option<Bracket>,
}

impl<L> LGraphLetter<L>
where
    L: Letter,
{
    pub fn new(letter: Option<L>, bracket: Option<Bracket>) -> Self {
        Self { letter, bracket }
    }

    pub fn letter(&self) -> Option<&L> {
        self.letter.as_ref()
    }

    pub fn bracket(&self) -> Option<&Bracket> {
        self.bracket.as_ref()
    }
}
impl<L> Letter for LGraphLetter<L> where L: Letter {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    graph: G,
    _p: PhantomData<(N, L)>,
}

impl<G, N, L> Graph<N, LGraphLetter<L>> for LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    fn nodes(&self) -> Box<dyn Iterator<Item = &N> + '_> {
        self.graph.nodes()
    }

    fn edges(&self) -> Box<dyn Iterator<Item = &crate::path::Edge<N, LGraphLetter<L>>> + '_> {
        self.graph.edges()
    }

    fn start_node(&self) -> &N {
        self.graph.start_node()
    }

    fn end_nodes(&self) -> Box<dyn Iterator<Item = &N> + '_> {
        self.graph.end_nodes()
    }
}

impl<G, N, L> ModifyableGraph<N, LGraphLetter<L>> for LGraph<G, N, L>
where
    G: ModifyableGraph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    fn new_empty(start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self {
        LGraph::new(G::new_empty(start_node, end_nodes))
    }

    fn add_edge(&mut self, edge: crate::path::Edge<N, LGraphLetter<L>>) {
        self.graph.add_edge(edge)
    }
}

impl<G, N, L> LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    pub fn new(graph: G) -> Self {
        Self {
            graph,
            _p: PhantomData,
        }
    }

    pub fn graph(&self) -> &G {
        &self.graph
    }

    pub fn core<'a>(
        &'a self,
        w: usize,
        d: usize,
    ) -> impl Iterator<Item = Path<N, LGraphLetter<L>>> + 'a
    where
        Self: Sized,
        L: 'a,
        N: 'a,
    {
        CoreIter::new(self, w, d)
    }

    pub fn core_from<'a>(
        &'a self,
        start_path: Path<N, LGraphLetter<L>>,
        w: usize,
        d: usize,
    ) -> impl Iterator<Item = Path<N, LGraphLetter<L>>> + 'a
    where
        Self: Sized,
        L: 'a,
        N: 'a,
    {
        let stack = BracketStack::from_brackets(start_path.iota()).unwrap();
        CoreIter {
            graph: self,
            state: vec![(start_path, stack)],
            w,
            d,
        }
    }

    pub fn normal_form<G0>(&self) -> LGraph<G0, Memory<N>, L>
    where
        G0: ModifyableGraph<Memory<N>, LGraphLetter<L>>,
    {
        let (core0, core1): (Vec<_>, Vec<_>) = self.core(1, 2).partition(|p| p.get_d() <= 1);
        let mut g0 = G0::from_paths(core0.into_iter().map(|t| t.mem())).unwrap();

        for t in core1 {
            let mem = t.mem();
            for (left, right) in t
                .paired_loops()
                .filter(|(l, r)| t.is_simple_paired_loops(*l, *r))
            {
                let t1 = t.subpath(0, left.0).unwrap();
                let t3 = t.subpath(left.1, right.0).unwrap();
                let t5 = t.subpath(right.1, t.len_in_nodes() - 1).unwrap();

                if t1.concat(&t3).concat(&t5).get_d() > 1 {
                    continue;
                }

                let t2 = mem.subpath(left.0, left.1).unwrap();
                if !g0.has_node(t2.beg()) || g0.has_node(t2.end()) {
                    continue;
                }
                let t2_mem_loop = t2.loopify_on_first().unwrap();

                for edge in t2_mem_loop.edges() {
                    if !g0.has_edge(edge) {
                        g0.add_edge(edge.clone());
                    }
                }

                let t4 = mem.subpath(right.0, right.1).unwrap();
                if g0.has_node(t4.beg()) || !g0.has_node(t4.end()) {
                    continue;
                }
                let t4_mem_loop = t4.loopify_on_last().unwrap();
                if !g0.has_node(t4_mem_loop.end()) {
                    continue;
                }
                for edge in t4_mem_loop.edges() {
                    if !g0.has_edge(edge) {
                        g0.add_edge(edge.clone());
                    }
                }
            }
        }
        LGraph::new(g0)
    }
}

struct CoreIter<'a, G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
    L: 'a,
{
    graph: &'a LGraph<G, N, L>,
    state: Vec<(Path<N, LGraphLetter<L>>, BracketStack)>,
    w: usize,
    d: usize,
}

impl<'a, G, N, L> CoreIter<'a, G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
    L: 'a,
{
    fn new(graph: &'a LGraph<G, N, L>, w: usize, d: usize) -> Self {
        Self {
            graph,
            state: vec![(
                Path::new(graph.start_node().clone()),
                BracketStack::default(),
            )],
            w,
            d,
        }
    }
}

impl<'a, G, N, L> Iterator for CoreIter<'a, G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    type Item = Path<N, LGraphLetter<L>>;

    fn next(&mut self) -> Option<Self::Item> {
        let max_depth = (self.d + 1) * self.graph.node_count() * self.graph.node_count();
        while let Some((path, brackets)) = self.state.pop() {
            let mut res = None;
            let node = path.end();

            if self.graph.is_end_node(node) && brackets.is_empty() && path.get_d() <= self.d {
                res = Some(path.clone());
            }

            for edge in self.graph.edges_from(node) {
                if edge.bracket().map_or(true, |b| brackets.can_accept(b)) {
                    let mut new_path = path.clone();
                    new_path.add_edge(edge.clone());
                    if new_path.depth() <= max_depth && new_path.get_w() <= self.w
                    // && new_path.get_d() <= self.d
                    {
                        let mut new_brackets = brackets.clone();
                        if let Some(b) = edge.bracket() {
                            new_brackets.accept(b.clone())
                        }
                        self.state.push((new_path, new_brackets));
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

#[cfg(test)]
mod tests {
    use std::{assert_eq, collections::HashSet, println, str::FromStr};

    use crate::graph::default_graph::DefaultGraph;

    use super::*;

    #[test]
    fn core() {
        let g = "-> 1; 1-a[1->1; 1-d[2->2; 2-b]2->2; 2-c[3->2; 2-d]3->3; 3-a]1->3; 3->;";
        let t1 = "1-d[2->2-b]2->2-c[3->2-d]3->3";
        let t2 = "1-a[1->1-d[2->2-b]2->2-c[3->2-d]3->3-a]1->3";
        let t3 = "1-a[1->1-a[1->1-d[2->2-b]2->2-c[3->2-d]3->3-a]1->3-a]1->3";

        let core00 = HashSet::from([Path::from_str(t1).unwrap()]);
        let core11 = HashSet::from([Path::from_str(t1).unwrap(), Path::from_str(t2).unwrap()]);
        let core12 = HashSet::from([
            Path::from_str(t1).unwrap(),
            Path::from_str(t2).unwrap(),
            Path::from_str(t3).unwrap(),
        ]);
        let g: LGraph<DefaultGraph<_, _>, _, _> = LGraph::from_str(g).unwrap();
        assert_eq!(
            g.core(0, 0)
                .inspect(|t| println!("core00: {}", t))
                .collect::<HashSet<_>>(),
            core00
        );
        assert_eq!(
            g.core(1, 1)
                .inspect(|t| println!("core11: {}", t))
                .collect::<HashSet<_>>(),
            core11
        );
        assert_eq!(
            g.core(1, 2)
                .inspect(|t| println!("core12: {}", t))
                .collect::<HashSet<_>>(),
            core12
        );
    }

    #[test]
    fn normal_form() {
        let g = "->1; 1-a[1->1; 1-d[2->2; 2-b]2->2; 2-c[3->2; 2-d]3->3; 3-a]1->3; 3->;";
        let g: LGraph<DefaultGraph<usize, _>, _, _> = LGraph::from_str(g).unwrap();
        let g1 = g.normal_form::<DefaultGraph<_, _>>();
        println!("{}", g1);
        assert_eq!(g1, LGraph::from_str("->1{};3{}->;1{}-d[2->2{2};2{1, 2}-b]2->2{1};3{1}-a]1->3{};2{1, 3}-d]3->3{1};2{3}-d]3->3{};1{}-a[1->1{1};1{1}-d[2->2{1, 2};2{}-c[3->2{3};2{1}-c[3->2{1, 3};2{2}-b]2->2{};1{}-a[1->1{};3{}-a]1->3{};1{1}-a[1->1{1};3{1}-a]1->3{1};").unwrap());

        let g = "->1; 6->; 1-[1->2; 2-a[2->2; 2-b->3; 3-a]2->3; 3-b->4; 4-]2->4; 3-a]1->5; 5-a->5; 5-b->6; 4-]1->6;";
        let g: LGraph<DefaultGraph<usize, _>, _, _> = LGraph::from_str(g).unwrap();
        let g1 = g.normal_form::<DefaultGraph<_, _>>();
        assert_eq!(g1, LGraph::from_str("->1{};6{}->;5{}-a->5{};3{1}-a]1->5{};3{1, 2, 2}-a]2->3{1, 2};1{}-[1->2{1};4{1}-]1->6{};2{1}-a[2->2{1, 2};5{}-b->6{};3{1, 2}-b->4{1, 2};2{1}-b->3{1};3{1, 2}-a]2->3{1};2{1, 2}-a[2->2{1, 2, 2};3{1}-b->4{1};2{1, 2, 2}-b->3{1, 2, 2};2{1, 2}-b->3{1, 2};4{1, 2}-]2->4{1};2{1}-a[2->2{1};4{1}-]2->4{1};2{1, 2}-a[2->2{1, 2};4{1, 2}-]2->4{1, 2};3{1}-a]2->3{1};3{1, 2}-a]2->3{1, 2};2{1, 2, 2}-a[2->2{1, 2, 2};3{1, 2, 2}-a]2->3{1, 2, 2};").unwrap());
    }
}
