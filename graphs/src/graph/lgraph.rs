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

    pub fn normal_form<G0>(&self, d0: usize) -> LGraph<G0, Memory<N>, L>
    where
        G0: ModifyableGraph<Memory<N>, LGraphLetter<L>>,
    {
        let (core0, core1): (Vec<_>, Vec<_>) = self.core(1, d0 + 1).partition(|p| p.get_d() <= d0);
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

                if t1.concat(&t3).concat(&t5).get_d() > d0 {
                    continue;
                }

                let t2_mem_loop = mem
                    .subpath(left.0, left.1)
                    .unwrap()
                    .loopify_on_first()
                    .unwrap();
                if !g0.has_node(t2_mem_loop.end()) {
                    continue;
                }

                for edge in t2_mem_loop.edges() {
                    if !g0.has_edge(edge) {
                        g0.add_edge(edge.clone());
                    }
                }

                let t4_mem_loop = mem
                    .subpath(right.0, right.1)
                    .unwrap()
                    .loopify_on_last()
                    .unwrap();
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
                    if new_path.depth() <= max_depth && new_path.get_w() <= self.w {
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
    use std::{collections::HashSet, str::FromStr};

    use crate::io::DotConvertable;

    use super::*;

    #[test]
    fn core() {
        let g = r#"
        digraph {
            node [shape=circle]
            Q0 [style=invisible, height=0, width=0, fixedsize=true]
            Q0 -> 1
        
            1 [start=true]
            3 [end=true,shape=doublecircle]
        
            1 -> 1 [item="a[1", label="a\n[1"]
            1 -> 2 [item="d[2", label="d\n[2"]
            2 -> 2 [item="b]2", label="b\n]2"]
            2 -> 2 [item="c[3", label="c\n[3"]
            2 -> 3 [item="d]3", label="d\n]3"]
            3 -> 3 [item="a]1", label="a\n]1"]
        }
        "#;
        let t1 = "1-d-[2->2-b-]2->2-c-[3->2-d-]3->3";
        let t2 = "1-a-[1->1-d-[2->2-b-]2->2-c-[3->2-d-]3->3-a-]1->3";
        let t3 = "1-a-[1->1-a-[1->1-d-[2->2-b-]2->2-c-[3->2-d-]3->3-a-]1->3-a-]1->3";

        let core00 = HashSet::from([Path::from_str(t1).unwrap()]);
        let core11 = HashSet::from([Path::from_str(t1).unwrap(), Path::from_str(t2).unwrap()]);
        let core12 = HashSet::from([
            Path::from_str(t1).unwrap(),
            Path::from_str(t2).unwrap(),
            Path::from_str(t3).unwrap(),
        ]);
        let g = LGraph::read_dot(g).unwrap();
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

    //     #[test]
    //     fn normal_form() {
    //         let g = r#"
    //         digraph {
    //             node [shape=circle]
    //             Q0 [style=invisible, height=0, width=0, fixedsize=true]
    //             Q0 -> 1

    //             1 [start=true]
    //             3 [end=true,shape=doublecircle]

    //             1 -> 1 [item="a[1", label="a\n[1"]
    //             1 -> 2 [item="d[2", label="d\n[2"]
    //             2 -> 2 [item="b]2", label="b\n]2"]
    //             2 -> 2 [item="c[3", label="c\n[3"]
    //             2 -> 3 [item="d]3", label="d\n]3"]
    //             3 -> 3 [item="a]1", label="a\n]1"]
    //         }
    //         "#;
    //         let g = LGraph::read_dot(g).unwrap();
    //         let g1 = g.normal_form::<DefaultGraph<_, _>>(1);
    //         g1.graph().write_dot(&mut std::io::stdout()).unwrap();

    //         let g = r#"
    //         digraph {
    //             node [shape=circle];
    //             Q0 [style=invisible, height=0, width=0, fixedsize=true];

    //             1 [start=true];
    //             Q0 -> 1;

    //             6 [end=true];

    //             1 -> 2 [item="[1", label="[1"];
    //             2 -> 2 [item="a[2", label="a\n[2"];
    //             2 -> 3 [item="b", label="b"];
    //             3 -> 3 [item="a]2", label="a\n]2"];
    //             3 -> 4 [item="b", label="b"];
    //             4 -> 4 [item="]2", label="]2"];
    //             3 -> 5 [item="a]1", label="a\n]1"];
    //             5 -> 5 [item="a", label="a"];
    //             5 -> 6 [item="b", label="b"];
    //             4 -> 6 [item="]1", label="]1"];
    //         }
    // "#;
    //         let g = LGraph::read_dot(g).unwrap();
    //         let g1 = g.normal_form::<DefaultGraph<_, _>>(1);
    //         g1.graph().write_dot(&mut std::io::stdout()).unwrap();

    //         panic!("check the graph manually...");
    //     }
}
