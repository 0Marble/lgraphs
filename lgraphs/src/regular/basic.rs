use crate::{
    graph::{
        lgraph::{LGraph, LGraphLetter},
        Graph,
    },
    path::{Letter, Node},
};

impl<G, N, L> LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>>,
    N: Node,
    L: Letter,
{
    pub fn no_brackets(&self) -> bool {
        self.edges().all(|e| e.bracket().is_none())
    }

    pub fn no_letters(&self) -> bool {
        self.edges().all(|e| e.item().is_none())
    }

    pub fn core11_no_letters_on_loops(&self) -> bool {
        for t in self.core(1, 1) {
            let mut had_letter = false;
            for (left, right) in t.simple_paired_loops() {
                for i in left.0..left.1 {
                    if t.nth_edge(i).unwrap().item().is_some() {
                        had_letter = true;
                        break;
                    }
                }

                for i in right.0..right.1 {
                    if t.nth_edge(i).unwrap().item().is_some() && had_letter {
                        return false;
                    }
                }
            }
        }

        true
    }
}
