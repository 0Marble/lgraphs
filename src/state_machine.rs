use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    graph::{Graph, Node, NodeIndex},
    iters::Subsets,
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

    pub fn determine(&self) -> StateMachine<MultiStateNode<L>, I>
    where
        I: Eq + Hash + Clone,
        L: Eq + Hash + Clone,
    {
        let mut new_edges = vec![];
        let mut end_nodes = HashSet::new();

        for new_node in Subsets::new(self.graph.nodes()) {
            let mut from: HashMap<&I, HashSet<NodeIndex>> = HashMap::new();
            for node in &new_node {
                for edge in self
                    .graph
                    .edges_from(*node)
                    .flat_map(|e| self.graph.edge_ref(e))
                {
                    from.entry(edge.item())
                        .or_default()
                        .insert(edge.target_index());
                }
            }
            if !from.is_empty() {
                for (item, target) in from {
                    let source = Node::new(MultiStateNode::new(
                        new_node
                            .iter()
                            .flat_map(|i| self.graph.node_ref(*i))
                            .map(|r| Node::new(r.label().clone()))
                            .collect(),
                    ));
                    new_edges.push((
                        source,
                        item.clone(),
                        Node::new(MultiStateNode::new(
                            target
                                .iter()
                                .flat_map(|i| self.graph.node_ref(*i))
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
                            .flat_map(|i| self.graph.node_ref(*i))
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
                    self.graph
                        .node_ref(self.start_node())
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
        self.graph.nodes().all(|node| {
            self.graph
                .edges_from(node)
                .flat_map(|e| self.graph.edge_ref(e))
                .map(|e| e.item())
                .all(|item| {
                    self.graph
                        .edges_from_with(node, item)
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
