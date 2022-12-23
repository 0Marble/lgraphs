use std::{collections::HashSet, fmt::Debug, hash::Hash};

pub trait Token: Debug + Clone + ToString + Eq + Hash {}

pub trait Bracket: Debug + Clone + ToString + Eq {
    fn is_opening(&self) -> bool;
    fn index(&self) -> usize;

    fn closes(&self, other: &Self) -> bool {
        self.is_closing() && other.is_opening() && self.index() == other.index()
    }
    fn is_closing(&self) -> bool {
        !self.is_opening()
    }
}

#[derive(Debug, Clone)]
pub struct Connection<T, B> {
    token: Option<T>,
    brackets: Vec<B>,
    target: Option<usize>,
}

impl<T, B> Connection<T, B> {
    pub fn new(token: Option<T>, brackets: Vec<B>, target: Option<usize>) -> Self {
        Self {
            token,
            brackets,
            target,
        }
    }

    pub fn token(&self) -> Option<&T> {
        self.token.as_ref()
    }

    pub fn brackets(&self) -> &[B] {
        self.brackets.as_ref()
    }

    pub fn target(&self) -> Option<usize> {
        self.target
    }
}

#[derive(Debug, Clone)]
pub struct Node<T, B> {
    label: String,
    outgoing: Vec<Connection<T, B>>,
}

impl<T, B> Node<T, B> {
    pub fn new(label: impl ToString, outgoing: Vec<Connection<T, B>>) -> Self {
        Self {
            label: label.to_string(),
            outgoing,
        }
    }

    pub fn label(&self) -> &str {
        self.label.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct LGraph<T, B> {
    nodes: Vec<Node<T, B>>,
}

impl<T, B> LGraph<T, B>
where
    B: Bracket,
    T: Token,
{
    pub fn new(nodes: Vec<Node<T, B>>) -> Self {
        Self { nodes }
    }

    pub fn direct(&self, node: usize, connection_index: usize) -> HashSet<Option<T>> {
        let mut cur_connections = vec![(node, connection_index)];
        let mut results = HashSet::new();

        loop {
            let mut next_connections = vec![];
            for (node, con) in &cur_connections {
                let con = &self.nodes[*node].outgoing[*con];
                let (cur_token, cur_target) = (&con.token, &con.target);

                if let Some(cur_target) = cur_target {
                    if cur_token.is_none() {
                        let next_node = &self.nodes[*cur_target];
                        for i in 0..next_node.outgoing.len() {
                            next_connections.push((*cur_target, i))
                        }
                    }
                }
                results.insert(cur_token.clone());
            }

            if next_connections.is_empty() {
                break;
            }

            cur_connections = next_connections;
        }

        results
    }

    pub fn edges_from(&self, node: usize) -> &[Connection<T, B>] {
        self.nodes[node].outgoing.as_ref()
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn nodes(&self) -> &[Node<T, B>] {
        self.nodes.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::default_impls::*;

    #[test]
    fn direct() {
        let g: LGraph<char, DefaultBracket> = LGraph::new(vec![
            Node::new("1", vec![Connection::new(None, vec![], Some(1))]),
            Node::new(
                "2",
                vec![
                    Connection::new(Some('a'), vec![], Some(1)),
                    Connection::new(Some('b'), vec![], Some(1)),
                    Connection::new(None, vec![], None),
                ],
            ),
        ]);

        assert_eq!(g.direct(0, 0), HashSet::from([None, Some('a'), Some('b')]));
        assert_eq!(g.direct(1, 0), HashSet::from([Some('a')]));
        assert_eq!(g.direct(1, 1), HashSet::from([Some('b')]));
        assert_eq!(g.direct(1, 2), HashSet::from([None]));

        let g: LGraph<char, DefaultBracket> = LGraph::new(vec![
            Node::new("1", vec![Connection::new(None, vec![], Some(1))]),
            Node::new(
                "2",
                vec![
                    Connection::new(Some('a'), vec![], Some(1)),
                    Connection::new(None, vec![], Some(2)),
                ],
            ),
            Node::new(
                "3",
                vec![
                    Connection::new(Some('b'), vec![], Some(2)),
                    Connection::new(None, vec![], None),
                ],
            ),
        ]);

        assert_eq!(g.direct(0, 0), HashSet::from([None, Some('a'), Some('b')]));
        assert_eq!(g.direct(1, 1), HashSet::from([None, Some('b')]));
        assert_eq!(g.direct(1, 0), HashSet::from([Some('a')]));
        assert_eq!(g.direct(2, 0), HashSet::from([Some('b')]));
        assert_eq!(g.direct(2, 1), HashSet::from([None]));
    }
}
