use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use super::{
    graph_trait::Graph,
    lgraph::{BracketStack, Item, LGraph, Memory},
    refs::{NodeRef, Path},
};

#[derive(Debug)]
enum ScoutResult<'a, N, E> {
    Success(HashSet<Path<'a, N, Item<E>>>),
    NotEnoughDepth,
    Failure,
}

impl<'a, N, E> Clone for ScoutResult<'a, N, E> {
    fn clone(&self) -> Self {
        match self {
            Self::Success(arg0) => Self::Success(arg0.clone()),
            Self::NotEnoughDepth => Self::NotEnoughDepth,
            Self::Failure => Self::Failure,
        }
    }
}

impl<'a, N, E> ScoutResult<'a, N, E> {
    #[must_use]
    fn is_not_enough_depth(&self) -> bool {
        matches!(self, Self::NotEnoughDepth)
    }

    fn is_failure(&self) -> bool {
        matches!(self, Self::Failure)
    }
}

#[derive(Debug)]
struct ScoutCache<'a, N, E> {
    #[allow(clippy::type_complexity)]
    cache: HashMap<Memory<NodeRef<'a, N>>, Vec<Option<ScoutResult<'a, N, E>>>>,
    max_depth: usize,
}

impl<'a, N, E> ScoutCache<'a, N, E> {
    fn print(&self) -> String
    where
        N: Display + Clone,
        E: Display,
    {
        let mut s = String::new();

        for (mem, caches) in &self.cache {
            s = format!("{}{} ->\n", s, mem.deref());
            for (depth, res) in caches.iter().enumerate() {
                s = format!(
                    "{}\t[{}]->{}\n",
                    s,
                    depth,
                    match res.as_ref() {
                        Some(ScoutResult::Failure) => "Failure".to_string(),
                        Some(ScoutResult::NotEnoughDepth) => "NotEnoughDepth".to_string(),
                        Some(ScoutResult::Success(paths)) => {
                            let mut s = String::new();
                            for path in paths {
                                s += &format!(",{}", path.print());
                            }
                            s
                        }
                        None => "_".to_string(),
                    }
                );
            }
        }

        s
    }

    fn new(max_depth: usize) -> Self {
        Self {
            cache: HashMap::new(),
            max_depth: max_depth + 1,
        }
    }

    fn add(
        &mut self,
        node: NodeRef<'a, N>,
        stack: BracketStack,
        res: ScoutResult<'a, N, E>,
        depth_budget: usize,
    ) {
        let old = self
            .cache
            .entry(Memory::new(node, stack))
            .or_insert_with(|| (0..self.max_depth).map(|_| None).collect());
        old[depth_budget] = Some(res);
    }

    fn get(
        &self,
        node: NodeRef<'a, N>,
        stack: BracketStack,
        depth_budget: usize,
    ) -> Option<ScoutResult<'a, N, E>> {
        let cached_results = self.cache.get(&Memory::new(node, stack))?;
        if let Some(res) = &cached_results[depth_budget] {
            return Some(res.clone());
        } else {
            for (depth, res) in cached_results.iter().enumerate() {
                if depth >= depth_budget
                    && res.as_ref().map_or(false, |res| res.is_not_enough_depth())
                {
                    return Some(ScoutResult::NotEnoughDepth);
                } else if depth <= depth_budget
                    && res.as_ref().map_or(false, |res| res.is_failure())
                {
                    return Some(ScoutResult::Failure);
                }
            }
        }

        None
    }
}

impl<N, E, G> LGraph<N, E, G>
where
    G: Graph<N, Item<E>>,
    N: Eq + Clone + Debug + Display,
    E: Eq + Clone + Debug + Display,
{
    #[allow(clippy::too_many_arguments)]
    fn scout<'a>(
        &'a self,
        w: usize,
        d: usize,
        depth_budget: usize,
        node: NodeRef<'a, N>,
        path: &Path<'a, N, Item<E>>,
        stack: &BracketStack,
        cycle_count: &HashMap<Memory<NodeRef<'a, N>>, usize>,
        cache: &mut ScoutCache<'a, N, E>,
    ) -> ScoutResult<N, E> {
        // print!("Examining: {}\t", path.print());

        if !self.is_end_node(node) {
            match cache.get(node, stack.clone(), depth_budget) {
                Some(ScoutResult::Failure) => {
                    // println!("Cached failure");
                    return ScoutResult::Failure;
                }
                Some(ScoutResult::NotEnoughDepth) => {
                    // println!("Cached NotEnoughDepth");
                    return ScoutResult::NotEnoughDepth;
                }
                Some(ScoutResult::Success(paths)) => {
                    if paths.iter().any(|p| p.has_prefix(path)) {
                        // println!("Cached Success");
                        return ScoutResult::Success(paths);
                    }
                }
                None => {
                    // println!("Not cached");
                }
            }
        }

        if stack.len() > depth_budget {
            cache.add(
                node,
                stack.clone(),
                ScoutResult::NotEnoughDepth,
                depth_budget,
            );
            return ScoutResult::NotEnoughDepth;
        }
        let mut successful_paths = HashSet::new();

        if self.is_end_node(node) && stack.is_empty() && self.is_in_core_w_checked(path, d) {
            successful_paths.insert(path.clone());
        }

        let mut had_not_enough_depth = false;
        for edge in self.edges_from(node) {
            let Some(new_stack) = stack.accept_clone(edge.contents().bracket()) else {
                    had_not_enough_depth = true;
                    // println!("Skipping {}-{}->{} (bracket check)", edge.source().contents(),edge.contents(), edge.target().contents());
                    continue;
                };
            let mut new_cycle_count = cycle_count.clone();
            let mem = Memory::new(edge.target(), new_stack.clone());
            let count: &mut usize = new_cycle_count
                .entry(mem)
                .and_modify(|count| *count += 1)
                .or_default();
            if *count > w {
                // println!(
                //     "Skipping {}-{}->{} (loops check)",
                //     edge.source().contents(),
                //     edge.contents(),
                //     edge.target().contents()
                // );
                continue;
            }

            // println!(
            //     "Taking {}-{}->{}",
            //     edge.source().contents(),
            //     edge.contents(),
            //     edge.target().contents()
            // );

            let next_scout = self.scout(
                w,
                d,
                depth_budget,
                edge.target(),
                &path.push(edge),
                &new_stack,
                &new_cycle_count,
                cache,
            );

            match next_scout {
                ScoutResult::Success(paths) => {
                    for path in paths {
                        successful_paths.insert(path);
                    }
                }
                ScoutResult::NotEnoughDepth => had_not_enough_depth = true,
                ScoutResult::Failure => {}
            }
        }

        if !successful_paths.is_empty() {
            cache.add(
                node,
                stack.clone(),
                ScoutResult::Success(successful_paths.clone()),
                depth_budget,
            );
            ScoutResult::Success(successful_paths)
        } else if had_not_enough_depth {
            if !self.is_end_node(node) {
                cache.add(
                    node,
                    stack.clone(),
                    ScoutResult::NotEnoughDepth,
                    depth_budget,
                );
            }
            ScoutResult::NotEnoughDepth
        } else {
            if !self.is_end_node(node) {
                cache.add(node, stack.clone(), ScoutResult::Failure, depth_budget);
            }
            ScoutResult::Failure
        }
    }

    pub fn faster_core(
        &self,
        w: usize,
        d: usize,
        d_helper: Option<usize>,
    ) -> impl Iterator<Item = Path<'_, N, Item<E>>> + '_
    where
        N: Display,
        E: Display,
    {
        let max_depth = d_helper.unwrap_or((d + 1) * self.node_count() * self.node_count());
        let mut res = HashSet::new();
        let mut cache = ScoutCache::new(max_depth);
        for depth_budget in 0..=max_depth {
            println!("depth_budget={}", depth_budget);
            let scout_res = self.scout(
                w,
                d,
                depth_budget,
                self.start_node(),
                &Path::default(),
                &BracketStack::default(),
                &HashMap::from([(Memory::new(self.start_node(), BracketStack::default()), 1)]),
                &mut cache,
            );

            match scout_res {
                ScoutResult::Success(paths) => {
                    for path in paths {
                        println!("{}", path.print());
                        res.insert(path);
                    }
                }
                ScoutResult::NotEnoughDepth => continue,
                ScoutResult::Failure => break,
            }
        }

        println!("{}", cache.print());
        res.into_iter()
    }
}
