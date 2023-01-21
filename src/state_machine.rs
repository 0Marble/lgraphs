use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::{
    graph::{EdgeIndex, EdgeRef, Graph, Node, NodeIndex, NodeRef},
    iters::{AllPairs, Subsets},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StateMachine<L, I> {
    graph: Graph<L, I>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiStateNode<L>
where
    L: Eq + Hash,
{
    nodes: HashSet<Node<L>>,
}

impl<L> MultiStateNode<L>
where
    L: Eq + Hash,
{
    pub fn new(nodes: HashSet<Node<L>>) -> Self {
        Self { nodes }
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl<L> Hash for MultiStateNode<L>
where
    L: Eq + Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.nodes.iter().for_each(|item| item.hash(state));
    }
}

impl<L, I> StateMachine<L, I> {
    pub fn new(graph: Graph<L, I>) -> Self {
        Self { graph }
    }

    pub fn start_node(&self) -> NodeIndex {
        self.graph.start_nodes().into_iter().next().unwrap()
    }
    pub fn end_nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.graph.end_nodes()
    }
    pub fn item(&self, edge: EdgeIndex) -> Option<&I> {
        self.graph.item(edge)
    }
    pub fn label(&self, node: NodeIndex) -> Option<&L> {
        self.graph.label(node)
    }
    pub fn target(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.target(edge)
    }
    pub fn source(&self, edge: EdgeIndex) -> Option<NodeIndex> {
        self.graph.source(edge)
    }
    pub fn edge_ref(&self, edge: EdgeIndex) -> Option<EdgeRef<'_, I, L>> {
        self.graph.edge_ref(edge)
    }
    pub fn node_ref(&self, node: NodeIndex) -> Option<NodeRef<'_, L>> {
        self.graph.node_ref(node)
    }
    pub fn nodes(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.graph.nodes()
    }
    pub fn edges(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges()
    }
    pub fn edges_from(&self, node: NodeIndex) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges_from(node)
    }
    pub fn edges_to(&self, node: NodeIndex) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edges_to(node)
    }
    pub fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeIndex,
        item: &'b I,
    ) -> impl Iterator<Item = EdgeIndex> + 'a
    where
        'b: 'a,
        I: Eq,
    {
        self.graph.edges_from_with(node, item)
    }
    pub fn is_end_node(&self, node: NodeIndex) -> bool {
        self.graph.is_end_node(node)
    }

    pub fn determine(&self) -> StateMachine<MultiStateNode<L>, I>
    where
        I: Eq + Hash + Clone,
        L: Eq + Hash + Clone,
    {
        let mut new_edges = vec![];
        let mut end_nodes = HashSet::new();

        for new_node in Subsets::new(self.nodes()) {
            let mut edges: HashMap<&I, HashSet<NodeIndex>> = HashMap::new();
            for node in &new_node {
                for edge in self.edges_from(*node).flat_map(|e| self.edge_ref(e)) {
                    edges
                        .entry(edge.item())
                        .or_default()
                        .insert(edge.target_index());
                }
            }
            if !edges.is_empty() {
                for (item, target) in edges {
                    let source = Node::new(MultiStateNode::new(
                        new_node
                            .iter()
                            .flat_map(|i| self.node_ref(*i))
                            .map(|r| Node::new(r.label().clone()))
                            .collect(),
                    ));
                    new_edges.push((
                        source,
                        item.clone(),
                        Node::new(MultiStateNode::new(
                            target
                                .iter()
                                .flat_map(|i| self.node_ref(*i))
                                .map(|r| Node::new(r.label().clone()))
                                .collect(),
                        )),
                    ));
                }
            }

            for end_node in self.end_nodes() {
                if new_node.contains(&end_node) {
                    end_nodes.insert(Node::new(MultiStateNode::new(
                        new_node
                            .iter()
                            .flat_map(|i| self.node_ref(*i))
                            .map(|r| Node::new(r.label().clone()))
                            .collect(),
                    )));
                }
            }
        }

        StateMachine {
            graph: Graph::from_edges(
                new_edges,
                HashSet::from([Node::new(MultiStateNode::new(HashSet::from_iter(
                    self.node_ref(self.start_node())
                        .map(|n| Node::new(n.label().clone())),
                )))]),
                end_nodes,
            ),
        }
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

    pub fn reachable_from(&self, node: NodeIndex) -> ReachableFrom<'_, L, I> {
        ReachableFrom::new(self, node)
    }

    pub fn remove_unreachable(&self) -> Self
    where
        L: Clone + Eq + Hash,
        I: Clone,
    {
        let reachable = self
            .reachable_from(self.start_node())
            .collect::<HashSet<_>>();

        let mut new_edges = vec![];
        for (from, to, item) in self
            .graph
            .edges()
            .flat_map(|e| self.edge_ref(e))
            .map(|e| (e.source_index(), e.target_index(), e))
            .filter_map(|(a, b, e)| Some((self.node_ref(a)?, self.node_ref(b)?, e.item())))
        {
            if reachable.contains(&from.index()) && reachable.contains(&to.index()) {
                new_edges.push((
                    Node::new(from.label().clone()),
                    item.clone(),
                    Node::new(to.label().clone()),
                ))
            }
        }

        Self::new(Graph::from_edges(
            new_edges,
            self.node_ref(self.start_node())
                .map(|n| Node::new(n.label().clone())),
            reachable
                .intersection(&HashSet::from_iter(self.end_nodes()))
                .flat_map(|n| self.node_ref(*n).map(|n| Node::new(n.label().clone()))),
        ))
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
        I: Clone + Eq + Hash,
        L: Clone + Eq + Hash,
    {
        let mut new_groups: Vec<HashSet<NodeIndex>> = vec![];

        let mut section_start = 0;
        for group in prev {
            let mut pushed = false;

            for a in group {
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

    pub fn minimize(&self) -> Self
    where
        I: Clone + Eq + Hash,
        L: Clone + Eq + Hash,
    {
        let g = self.remove_unreachable();
        let mut prev = vec![HashSet::new(), HashSet::new()];
        for a in g.graph.nodes() {
            if self.is_end_node(a) {
                prev[1].insert(a);
            } else {
                prev[0].insert(a);
            }
        }

        for k in 1.. {
            let cur = g.next_eqivalence_group(&prev);
            if cur == prev {
                break;
            } else {
                prev = cur;
            }
        }

        let mut new_edges = vec![];
        for group in &prev {
            let a = group.iter().next().unwrap();

            for b in g.edges_from(*a).flat_map(|e| g.target(e)) {
                for target_group in &prev {
                    if target_group.contains(&b) {
                        new_edges.push(())
                    }
                }
            }
        }

        todo!()
    }
}

pub struct ReachableFrom<'a, L, I> {
    graph: &'a StateMachine<L, I>,
    stack: VecDeque<NodeIndex>,
    reached: HashSet<NodeIndex>,
}

impl<'a, L, I> Iterator for ReachableFrom<'a, L, I> {
    type Item = NodeIndex;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.stack.pop_front()?;

        for target in self
            .graph
            .graph
            .edges_from(node)
            .flat_map(|edge| self.graph.target(edge))
        {
            if self.reached.insert(target) {
                self.stack.push_front(target);
            }
        }

        Some(node)
    }
}

impl<'a, L, I> ReachableFrom<'a, L, I> {
    pub fn new(graph: &'a StateMachine<L, I>, node: NodeIndex) -> Self {
        Self {
            graph,
            stack: VecDeque::from([node]),
            reached: HashSet::from([node]),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::graph::{testing_parser::TestingParser, Edge, Graph, Node};

    use super::StateMachine;

    #[test]
    fn determine() {
        let edges = [
            Edge::new(0, 0, 1),
            Edge::new(0, 0, 2),
            Edge::new(0, 0, 3),
            Edge::new(0, 1, 1),
            Edge::new(0, 2, 2),
            Edge::new(0, 3, 3),
            Edge::new(1, 1, 1),
            Edge::new(1, 1, 2),
            Edge::new(1, 1, 3),
            Edge::new(1, 4, 1),
            Edge::new(2, 1, 1),
            Edge::new(2, 1, 2),
            Edge::new(2, 1, 3),
            Edge::new(2, 4, 2),
            Edge::new(3, 1, 1),
            Edge::new(3, 1, 2),
            Edge::new(3, 1, 3),
            Edge::new(3, 4, 3),
        ];
        let nodes = [
            Node::new("q0".to_string()),
            Node::new("q1".to_string()),
            Node::new("q2".to_string()),
            Node::new("q3".to_string()),
            Node::new("qf".to_string()),
        ];
        let g = Graph::from_parser(&mut TestingParser::new(edges, nodes, [0], [4])).unwrap();
        let s = StateMachine::new(g);

        assert!(!s.is_determined());
        assert!(s.determine().is_determined());
    }
}
