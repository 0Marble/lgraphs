use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

use super::{
    graph_trait::{Builder, Graph},
    iters::AllPairs,
    lgraph::{BracketStack, Item, LGraph, Memory},
    refs::{NodeRef, Path},
};

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Display + Debug + Hash,
    E: Eq + Clone + Display + Debug,
{
    fn core00(&self) -> impl Iterator<Item = Path<'_, N, Item<E>>> + '_ {
        self.core(0, 0, None)
    }

    #[allow(clippy::too_many_arguments)]
    fn scout_progenetors<'a>(
        &'a self,
        w: usize,
        d: usize,
        depth_budget: usize,
        node: NodeRef<'a, N>,
        path: &Path<'a, N, Item<E>>,
        stack: &BracketStack,
        cycle_count: &HashMap<Memory<NodeRef<'a, N>>, usize>,
        progenetors: &mut HashSet<Path<'a, N, Item<E>>>,
        core_paths: &mut HashSet<Path<'a, N, Item<E>>>,
        core00: &[Path<'a, N, Item<E>>],
    ) -> bool {
        let mut res = false;
        let this_progenetor = self.progenetor(path);

        if stack.len() > depth_budget {
            if core00.iter().any(|path| path.has_prefix(&this_progenetor)) {
                return progenetors.insert(this_progenetor);
            } else {
                return true;
            }
        }

        if self.is_end_node(node) && stack.is_empty() && self.is_in_core_w_checked(path, d) {
            core_paths.insert(path.clone());
            println!("!!!\t{}\n\t{}", path.print(), this_progenetor.print());

            if progenetors.insert(this_progenetor) {
                res = true;
            }
        }

        for edge in self.edges_from(node) {
            let Some(new_stack) = stack.accept_clone(edge.contents().bracket()) else {
                    continue;
                };
            let mut new_cycle_count = cycle_count.clone();
            let mem = Memory::new(edge.target(), new_stack.clone());
            let count: &mut usize = new_cycle_count
                .entry(mem)
                .and_modify(|count| *count += 1)
                .or_default();
            if *count > w {
                continue;
            }

            if self.scout_progenetors(
                w,
                d,
                depth_budget,
                edge.target(),
                &path.push(edge),
                &new_stack,
                &new_cycle_count,
                progenetors,
                core_paths,
                core00,
            ) {
                res = true;
            }
        }

        res
    }

    fn core11<'a>(
        &'a self,
        core00: &[Path<'a, N, Item<E>>],
    ) -> impl Iterator<Item = Path<'a, N, Item<E>>> + 'a {
        let mut res = HashSet::new();
        let max_depth = 2 * self.node_count() * self.node_count();
        let mut progenetors = HashSet::new();

        for d in 0..=max_depth {
            println!("Depth budget = {}", d);

            if !self.scout_progenetors(
                1,
                1,
                d,
                self.start_node(),
                &Path::default(),
                &BracketStack::default(),
                &HashMap::new(),
                &mut progenetors,
                &mut res,
                core00,
            ) {
                break;
            }

            println!("Progenetor count = {}", progenetors.len());
            for path in &progenetors {
                println!("\t{}", path.print());
            }

            // std::io::stdin().read_line(&mut String::new()).unwrap();
        }

        res.into_iter()
    }
    fn progenetor<'a>(&'a self, path: &Path<'a, N, Item<E>>) -> Path<'a, N, Item<E>> {
        let loops: Vec<_> = path.loops().collect();

        let mut removed_loops = vec![];
        for ((a1, a2), (b1, b2)) in
            AllPairs::new(loops.iter(), loops.iter()).filter(|((_, a2), (b1, _))| a2 < b1)
        {
            let full = Path::new(path[*a1..*b2].to_vec());
            let mid = Path::new(path[*a2..*b1].to_vec());

            if self.path_balance(&full).map_or(false, |s| s.is_empty())
                && self.path_balance(&mid).map_or(false, |s| s.is_empty())
            {
                removed_loops.push((*a1, *a2));
                removed_loops.push((*b1, *b2));
            }
        }

        for (a, b) in loops {
            if self
                .path_balance(&Path::new(&path[a..b]))
                .map_or(false, |stack| stack.is_empty())
            {
                removed_loops.push((a, b));
            }
        }

        let mut new_path = Path::default();

        'outer: for (i, edge) in path.edges().enumerate() {
            for (a, b) in &removed_loops {
                if a <= &i && b > &i {
                    continue 'outer;
                }
            }
            new_path.push_mut(edge);
        }

        new_path
    }

    pub fn progenetors<B>(&self, builder: &mut B) -> Vec<LGraph<N, E, B::TargetGraph>>
    where
        B: Builder<N, Item<E>>,
        B::TargetGraph: Graph<N, Item<E>>,
    {
        let core00: Vec<_> = self.core00().collect();
        let core11 = self.core11(&core00);
        vec![]
    }
}
