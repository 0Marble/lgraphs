use std::{collections::HashMap, marker::PhantomData};

use super::{
    default::DefaultBuilder,
    graph_trait::{Builder, Graph, WithReachableFrom},
    iters::{AllPairs, Subsets},
    refs::{EdgeRef, NodeRef},
};

#[derive(Debug)]
pub struct StateMachine<N, E, G> {
    graph: G,
    _p: PhantomData<(N, E)>,
}

impl<N, E, G> WithReachableFrom<N, E> for StateMachine<N, E, G> where G: Graph<N, E> {}

impl<N, E, G> Graph<N, E> for StateMachine<N, E, G>
where
    G: Graph<N, E>,
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

    fn edges(&self) -> Box<dyn Iterator<Item = EdgeRef<'_, N, E>> + '_> {
        self.graph.edges()
    }
}

impl<N, E, G> StateMachine<N, Option<E>, G>
where
    G: Graph<N, Option<E>>,
    N: Eq + Clone,
    E: Eq + Clone,
{
    pub fn remove_nones<'a, B>(
        &'a self,
        builder: &mut B,
    ) -> StateMachine<&'a N, &'a E, B::TargetGraph>
    where
        B: Builder<&'a N, &'a E>,
    {
        let mut end_nodes = vec![];
        for node in self.nodes() {
            for (item, target) in self
                .lambda_closure(node)
                .map(|n| {
                    if self.is_end_node(n) {
                        end_nodes.push(node.contents());
                    }

                    n
                })
                .flat_map(|n| self.edges_from(n))
                .filter_map(|e| e.contents().as_ref().map(|item| (item, e.target())))
            {
                builder.add_edge(node.contents(), item, target.contents());
            }
        }

        StateMachine::new(builder.build(self.start_node().contents(), end_nodes))
    }

    fn lambda_closure<'a>(&'a self, node: NodeRef<'a, N>) -> impl Iterator<Item = NodeRef<'a, N>> {
        self.reachable_from(node)
            .filter(|(path, _)| path.edges().all(|e| e.contents().is_none()))
            .map(|(_, node)| node)
    }
}

impl<G, N, E> StateMachine<N, E, G>
where
    G: Graph<N, E>,
    N: Eq + Clone,
    E: Eq + Clone,
{
    pub fn new(graph: G) -> Self {
        Self {
            graph,
            _p: PhantomData,
        }
    }

    pub fn determine<B>(&self, builder: &mut B) -> StateMachine<usize, E, B::TargetGraph>
    where
        B: Builder<usize, E>,
    {
        builder.clear();
        let mut end_nodes = vec![];

        let mut translation: HashMap<Vec<NodeRef<N>>, usize> = HashMap::new();

        let mut count = 0;
        let mut start_node = 0;

        for mut source_node in Subsets::new(self.nodes()) {
            source_node.sort();
            source_node.dedup();

            let mut targets = vec![];
            for item in source_node
                .iter()
                .flat_map(|n| self.edges_from(*n))
                .map(|e| e.contents())
            {
                let mut target: Vec<_> = source_node
                    .iter()
                    .flat_map(|n| self.edges_from_with(*n, item))
                    .map(|e| e.target())
                    .collect();

                target.sort();
                target.dedup();
                let target_name = translation.entry(target).or_insert_with(|| {
                    count += 1;
                    count - 1
                });

                targets.push((item, *target_name));
            }

            let source_name = translation
                .entry(source_node.clone())
                .or_insert_with(|| {
                    count += 1;
                    count - 1
                })
                .to_owned();
            if source_node == vec![self.start_node()] {
                start_node = source_name;
            }

            if source_node.iter().any(|n| self.is_end_node(*n)) {
                end_nodes.push(source_name);
            }

            let _ = translation.entry(source_node).or_insert(source_name);
            for (item, target_name) in targets {
                builder.add_edge(source_name, item.clone(), target_name);
            }
        }

        StateMachine::new(builder.build(start_node, end_nodes))
    }

    pub fn is_determined(&self) -> bool
    where
        E: Eq,
    {
        self.nodes().all(|node| {
            self.edges_from(node).map(|e| e.contents()).all(|item| {
                self.edges_from_with(node, item)
                    .try_fold(None, |prev, cur| match prev {
                        Some(prev) => {
                            if prev == cur {
                                Ok(Some(prev))
                            } else {
                                Err(())
                            }
                        }
                        None => Ok(Some(cur)),
                    })
                    .is_ok()
            })
        })
    }

    fn are_next_equivalent<'a>(
        &'a self,
        a: NodeRef<'a, N>,
        b: NodeRef<'a, N>,
        prev: &[Vec<NodeRef<'a, N>>],
    ) -> bool
    where
        E: Eq,
    {
        if !self.are_equivalent(a, b, prev) {
            return false;
        }

        for item in self
            .edges_from(a)
            .map(|e| e.contents())
            .chain(self.edges_from(b).map(|e| e.contents()))
        {
            let mut success = false;
            for (a_next, b_next) in AllPairs::new(
                self.edges_from_with(a, item).map(|e| e.target()),
                self.edges_from_with(b, item).map(|e| e.target()),
            ) {
                if !self.are_equivalent(a_next, b_next, prev) {
                    return false;
                } else {
                    success = true;
                }
            }

            if !success {
                return false;
            }
        }

        true
    }

    fn are_equivalent<'a>(
        &'a self,
        a: NodeRef<'a, N>,
        b: NodeRef<'a, N>,
        prev: &[Vec<NodeRef<'a, N>>],
    ) -> bool {
        prev.iter()
            .any(|group| group.contains(&a) && group.contains(&b))
    }

    fn next_equivalence_group<'a>(
        &'a self,
        prev: &[Vec<NodeRef<'a, N>>],
    ) -> Vec<Vec<NodeRef<'a, N>>>
    where
        E: Eq,
    {
        let mut new_groups: Vec<Vec<NodeRef<'a, N>>> = vec![];

        let mut section_start = 0;
        for group in prev {
            for a in group {
                let mut pushed = false;
                for new_group in &mut new_groups[section_start..] {
                    let b = new_group[0];
                    if self.are_next_equivalent(*a, b, prev) {
                        new_group.push(*a);
                        pushed = true;
                        break;
                    }
                }

                if !pushed {
                    new_groups.push(vec![*a]);
                }
            }
            section_start = new_groups.len();
        }

        new_groups.iter_mut().for_each(|g| {
            g.sort();
            g.dedup();
        });

        new_groups
    }

    pub fn minimize<B>(&self, builder: &mut B) -> StateMachine<usize, E, B::TargetGraph>
    where
        B: Builder<usize, E>,
    {
        let g = self.remove_unreachable(&mut DefaultBuilder::default());

        let mut prev = vec![vec![], vec![]];
        for node in g.nodes() {
            if g.is_end_node(node) {
                prev[0].push(node);
            } else {
                prev[1].push(node);
            }
        }
        prev.iter_mut().for_each(|g| {
            g.sort();
            g.dedup();
        });

        loop {
            let next = g.next_equivalence_group(&prev);
            if next == prev {
                break;
            } else {
                prev = next;
            }
        }
        let translation: HashMap<_, _> = prev.iter().enumerate().map(|(i, g)| (g, i)).collect();

        builder.clear();

        for group in &prev {
            for edge in g.edges_from(group[0]) {
                if let Some(target) = prev.iter().find(|g| g.iter().any(|n| n == &edge.target())) {
                    builder.add_edge(
                        *translation.get(group).unwrap(),
                        (*edge.contents()).clone().clone(),
                        *translation.get(target).unwrap(),
                    )
                }
            }
        }

        StateMachine::new(
            builder.build(
                *translation
                    .get(
                        prev.iter()
                            .find(|group| group.iter().any(|n| n == &g.start_node()))
                            .unwrap(),
                    )
                    .unwrap(),
                g.end_nodes().map(|end_node| {
                    *translation
                        .get(
                            prev.iter()
                                .find(|group| group.iter().any(|n| n == &end_node))
                                .unwrap(),
                        )
                        .unwrap()
                }),
            ),
        )
    }

    pub fn remove_unreachable<'a, B>(
        &'a self,
        builder: &mut B,
    ) -> StateMachine<&'a N, &'a E, B::TargetGraph>
    where
        B: Builder<&'a N, &'a E>,
    {
        builder.clear();
        let mut end_nodes = vec![];
        for (_, node) in self.reachable() {
            builder.add_node(node.contents());
            if self.is_end_node(node) {
                end_nodes.push(node);
            }
            for edge in self.edges_from(node) {
                builder.add_edge(node.contents(), edge.contents(), edge.target().contents());
            }
        }
        end_nodes.sort();
        end_nodes.dedup();

        StateMachine::new(builder.build(
            self.start_node().contents(),
            end_nodes.iter().map(|n| n.contents()),
        ))
    }
}

#[cfg(test)]
mod tests {

    use super::StateMachine;
    use crate::graphs::default::DefaultBuilder;
    use crate::graphs::graph_trait::*;

    #[test]
    fn determine() {
        let edges = [
            (0, 1, 0),
            (0, 2, 0),
            (0, 3, 0),
            (0, 1, 1),
            (0, 2, 2),
            (0, 3, 3),
            (1, 1, 1),
            (1, 2, 1),
            (1, 3, 1),
            (1, 1, 4),
            (2, 1, 1),
            (2, 2, 1),
            (2, 3, 1),
            (2, 2, 4),
            (3, 1, 1),
            (3, 2, 1),
            (3, 3, 1),
            (3, 3, 4),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }

        let g = builder.build(0, [4]);
        let s = StateMachine::new(g);
        let determined = s.determine(&mut DefaultBuilder::default());

        assert!(!s.is_determined());
        assert!(determined.is_determined());
    }

    #[test]
    fn minimize() {
        let edges = [
            ('a', 0, 'b'),
            ('a', 1, 'd'),
            ('b', 0, 'b'),
            ('b', 1, 'c'),
            ('c', 0, 'd'),
            ('d', 0, 'd'),
            ('d', 1, 'e'),
            ('e', 0, 'b'),
            ('e', 1, 'c'),
            ('c', 1, 'e'),
            ('f', 0, 'c'),
            ('f', 1, 'g'),
            ('g', 0, 'f'),
            ('g', 1, 'e'),
        ];
        let mut builder = DefaultBuilder::default();
        for (source, item, target) in edges {
            builder.add_edge(source, item, target);
        }
        let s = StateMachine::new(builder.build('a', ['c', 'e']));
        let minimized = s.minimize(&mut DefaultBuilder::default());
        assert_eq!(minimized.node_count(), 3);
    }
}
