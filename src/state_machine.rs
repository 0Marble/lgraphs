use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    graph::{
        EdgeIndex, EdgeRef, EdgesFrom, EdgesFromWith, Graph, NodeIndex, NodeRef, ReachableFrom,
    },
    iters::{AllPairs, Subsets},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateMachine<L, I> {
    graph: Graph<L, I>,
    start_node: NodeIndex,
    end_nodes: HashSet<NodeIndex>,
}

impl<N, I> StateMachine<N, I> {
    pub fn new(graph: Graph<N, I>, start_node: N, end_nodes: impl IntoIterator<Item = N>) -> Self
    where
        N: Eq + Clone,
    {
        let mut builder = graph.to_builder().add_node(start_node.clone());
        let end_nodes = end_nodes.into_iter().collect::<Vec<_>>();
        for end_node in end_nodes.iter() {
            builder = builder.add_node(end_node.clone());
        }
        let graph = builder.build();

        Self {
            start_node: graph.node_with(&start_node).unwrap(),
            end_nodes: end_nodes
                .into_iter()
                .flat_map(|node| graph.node_with(&node))
                .collect(),
            graph,
        }
    }
    pub fn node_count(&self) -> usize {
        self.graph.node_count()
    }
    pub fn start_node(&self) -> NodeIndex {
        self.start_node
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.end_nodes.iter().cloned()
    }
    pub fn item(&self, edge: EdgeIndex) -> Option<&I> {
        self.graph.item(edge)
    }
    pub fn contents(&self, node: NodeIndex) -> Option<&N> {
        self.graph.node(node)
    }
    pub fn target(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.target(edge)
    }
    pub fn source(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.source(edge)
    }
    pub fn edge_ref(&self, edge: EdgeIndex) -> Option<EdgeRef<'_, N, I>> {
        self.graph.edge_ref(edge)
    }
    pub fn node_ref(&self, node: NodeIndex) -> Option<NodeRef<'_, N>> {
        self.graph.node_ref(node)
    }
    pub fn nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.graph.nodes()
    }
    pub fn edges(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges()
    }
    pub fn edges_from(&self, node: NodeIndex) -> EdgesFrom<N, I> {
        self.graph.edges_from(node)
    }
    pub fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b I,
    ) -> EdgesFromWith<'a, 'b, N, I>
    where
        I: Eq,
        'b: 'a,
    {
        self.graph.edges_from_with(node, item)
    }
    pub fn is_end_node(&self, node: NodeIndex) -> bool {
        self.end_nodes.contains(&node)
    }
    pub fn reachable_from(&self, node: NodeIndex) -> ReachableFrom<N, I> {
        self.graph.reachable_from(node)
    }

    fn convert<A>(node: A, conversion: &mut Vec<A>) -> usize
    where
        A: Eq,
    {
        conversion
            .iter()
            .enumerate()
            .find(|(_, n)| node.eq(n))
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                conversion.push(node);
                conversion.len() - 1
            })
    }

    pub fn determine(&self) -> (StateMachine<usize, I>, Vec<HashSet<NodeIndex>>)
    where
        I: Eq + Hash + Clone,
    {
        let mut conversion: Vec<HashSet<NodeIndex>> = vec![];
        let mut builder = Graph::from_builder();
        let mut end_nodes = HashSet::new();

        for node in Subsets::new(self.nodes()).map(HashSet::from_iter) {
            let mut edges: HashMap<I, HashSet<NodeIndex>> = HashMap::new();
            let source = Self::convert(node, &mut conversion);
            if conversion[source].intersection(&self.end_nodes).count() != 0 {
                end_nodes.insert(source);
            }

            for e in conversion[source]
                .iter()
                .flat_map(|n| self.edges_from(*n))
                .flat_map(|e| self.edge_ref(e))
            {
                edges
                    .entry(e.item().clone())
                    .or_default()
                    .insert(e.target_index());
            }

            if !edges.is_empty() {
                for (item, target) in edges.into_iter() {
                    let target = Self::convert(target, &mut conversion);
                    builder = builder.add_named_edge((source, item, target));
                }
            }
        }

        let start_node = Self::convert(HashSet::from([self.start_node]), &mut conversion);
        let g = builder.build();

        (StateMachine::new(g, start_node, end_nodes), conversion)
    }

    pub fn is_determined(&self) -> bool
    where
        I: Eq,
    {
        self.nodes().all(|node| {
            self.edges_from(node)
                .flat_map(|e| self.edge_ref(e))
                .map(|e| e.item())
                .all(|item| {
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

    pub fn remove_unreachable(&self) -> Self
    where
        N: Clone + Eq,
        I: Clone,
    {
        let mut builder = Graph::from_builder();
        let mut end_nodes = HashSet::new();

        for (_, reached) in self.reachable_from(self.start_node()) {
            for edge in self.edges_from(reached).flat_map(|e| self.edge_ref(e)) {
                builder = builder.add_named_edge((
                    edge.source().clone(),
                    edge.item().clone(),
                    edge.target().clone(),
                ));

                if self.is_end_node(edge.target_index()) {
                    end_nodes.insert(edge.target_index());
                }
                if self.is_end_node(edge.source_index()) {
                    end_nodes.insert(edge.source_index());
                }
            }
        }

        let g = builder.build();

        StateMachine::new(
            g,
            self.contents(self.start_node()).unwrap().clone(),
            end_nodes
                .into_iter()
                .map(|n| self.contents(n).unwrap().clone()),
        )
    }

    fn are_equivalent(&self, a: NodeIndex, b: NodeIndex, groups: &[HashSet<NodeIndex>]) -> bool {
        groups
            .iter()
            .any(|group| group.contains(&a) && group.contains(&b))
    }

    fn are_next_equivalent(&self, a: NodeIndex, b: NodeIndex, prev: &[HashSet<NodeIndex>]) -> bool
    where
        I: Eq,
    {
        if !self.are_equivalent(a, b, prev) {
            return false;
        }

        for item in self
            .edges_from(a)
            .flat_map(|e| self.item(e))
            .chain(self.edges_from(b).flat_map(|e| self.item(e)))
        {
            for (a_next, b_next) in AllPairs::new(
                self.edges_from_with(a, item).flat_map(|e| self.target(e)),
                self.edges_from_with(b, item).flat_map(|e| self.target(e)),
            ) {
                if !self.are_equivalent(a_next, b_next, prev) {
                    return false;
                }
            }
        }

        true
    }

    fn next_eqivalence_group(&self, prev: &[HashSet<NodeIndex>]) -> Vec<HashSet<NodeIndex>>
    where
        I: Eq,
    {
        let mut new_groups: Vec<HashSet<NodeIndex>> = vec![];

        let mut section_start = 0;
        for group in prev {
            for a in group {
                let mut pushed = false;
                for new_group in &mut new_groups[section_start..] {
                    let b = new_group.iter().next().unwrap();
                    if self.are_next_equivalent(*a, *b, prev) {
                        new_group.insert(*a);
                        pushed = true;
                        break;
                    }
                }

                if !pushed {
                    new_groups.push(HashSet::from([*a]));
                }
            }
            section_start = new_groups.len();
        }

        new_groups
    }

    pub fn minimize(&self) -> (StateMachine<usize, I>, Vec<HashSet<NodeIndex>>)
    where
        N: Clone + Eq,
        I: Eq + Clone,
    {
        let s = self.remove_unreachable();
        let mut prev_groups: Vec<HashSet<NodeIndex>> = vec![
            HashSet::from_iter(s.nodes().filter(|n| s.is_end_node(*n))),
            HashSet::from_iter(s.nodes().filter(|n| !s.is_end_node(*n))),
        ];

        loop {
            let cur_groups = s.next_eqivalence_group(&prev_groups);
            if cur_groups == prev_groups {
                break;
            } else {
                prev_groups = cur_groups;
            }
        }

        let mut builder = Graph::from_builder();
        let mut conversion = vec![];

        for group in &prev_groups {
            let source = Self::convert(group.clone(), &mut conversion);

            for edge in s
                .edges_from(*conversion[source].iter().next().unwrap())
                .flat_map(|e| s.edge_ref(e))
            {
                let target_group = prev_groups
                    .iter()
                    .find(|g| g.contains(&edge.target_index()))
                    .unwrap();
                let target = Self::convert(target_group.clone(), &mut conversion);
                builder = builder.add_named_edge((source, edge.item().clone(), target));
            }
        }

        let start_node = conversion
            .iter()
            .enumerate()
            .find(|(_, group)| group.contains(&s.start_node))
            .map(|(i, _)| i)
            .unwrap();
        let g = builder.build();
        let end_nodes = s
            .end_nodes()
            .flat_map(|n| {
                conversion
                    .iter()
                    .enumerate()
                    .find(|(_, group)| group.contains(&n))
                    .map(|(i, _)| i)
            })
            .collect::<Vec<_>>();

        (StateMachine::new(g, start_node, end_nodes), conversion)
    }
}

#[cfg(test)]
mod tests {

    use crate::graph::Graph;

    use super::StateMachine;

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
        let mut builder = Graph::from_builder();
        for edge in edges {
            builder = builder.add_named_edge(edge);
        }

        let g = builder.build();
        let s = StateMachine::new(g, 0, [4]);
        let (determined, _) = s.determine();

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
        let mut builder = Graph::from_builder();
        for edge in edges {
            builder = builder.add_named_edge(edge);
        }
        let s = StateMachine::new(builder.build(), 'a', ['c', 'e']);
        let (minimized, conversion) = s.minimize();
        assert_eq!(minimized.node_count(), 3);
    }
}
