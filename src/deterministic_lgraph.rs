use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

pub trait Token: Debug + Clone + Eq + Hash {}
impl Token for char {}

pub trait Bracket: Debug + Clone + Eq {
    fn is_opening(&self) -> bool;
    fn index(&self) -> usize;
    fn kind_index(&self) -> usize;

    fn closes(&self, other: &Self) -> bool {
        self.is_closing() && other.is_opening() && self.index() == other.index()
    }
    fn is_closing(&self) -> bool {
        !self.is_opening()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefaultBracket {
    RoundOpen(usize),
    RoundClose(usize),
    AngleOpen(usize),
    AngleClose(usize),
}

impl Bracket for DefaultBracket {
    fn is_opening(&self) -> bool {
        matches!(
            self,
            DefaultBracket::AngleOpen(_) | DefaultBracket::RoundOpen(_)
        )
    }

    fn index(&self) -> usize {
        match self {
            DefaultBracket::RoundOpen(i) => *i,
            DefaultBracket::RoundClose(i) => *i,
            DefaultBracket::AngleOpen(i) => *i,
            DefaultBracket::AngleClose(i) => *i,
        }
    }

    fn kind_index(&self) -> usize {
        match self {
            DefaultBracket::RoundOpen(_) => 0,
            DefaultBracket::RoundClose(_) => 0,
            DefaultBracket::AngleOpen(_) => 1,
            DefaultBracket::AngleClose(_) => 1,
        }
    }
}

pub trait Label: Debug + Clone + Eq + Hash + ToString {}
impl Label for String {}
impl Label for usize {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<T, B, L>
where
    T: Token,
    B: Bracket,
    L: Label,
{
    NonDeterministic {
        at_node: L,
    },
    InvalidBrackets,
    EdgeToUnknownTarget {
        from: L,
        to: L,
    },
    StartingNodeNotInGraph {
        node: L,
    },

    NoWayToContinue {
        at_node: L,
        string: Vec<T>,
    },
    UnbalancedBrackets {
        bracket_stack: HashMap<usize, Vec<B>>,
        string: Vec<T>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge<T, B, L> {
    target: Option<L>,
    token: Option<T>,
    brackets: Vec<B>,
}

impl<T, B, L> Edge<T, B, L>
where
    T: Token,
    B: Bracket,
    L: Label,
{
    pub fn new<TL>(
        target: Option<TL>,
        token: Option<T>,
        brackets: Vec<B>,
    ) -> Result<Self, Error<T, B, L>>
    where
        TL: Into<L>,
    {
        for (i, bi) in brackets.iter().enumerate() {
            for bj in brackets.iter().take(i) {
                if bi.kind_index() == bj.kind_index() {
                    return Err(Error::InvalidBrackets);
                }
            }
        }

        Ok(Self {
            target: target.map(TL::into),
            token,
            brackets,
        })
    }
}

#[derive(Debug, Clone)]
pub struct LGraph<T, B, L> {
    nodes: HashMap<L, Vec<Edge<T, B, L>>>,
    starting_node: L,
}

impl<T, B, L> LGraph<T, B, L>
where
    T: Token,
    B: Bracket,
    L: Label,
{
    pub fn new(
        nodes: impl Into<HashMap<L, Vec<Edge<T, B, L>>>>,
        starting_node: impl Into<L>,
    ) -> Result<Self, Error<T, B, L>> {
        let graph = LGraph {
            nodes: nodes.into(),
            starting_node: starting_node.into(),
        };

        if !graph.nodes.contains_key(&graph.starting_node) {
            return Err(Error::StartingNodeNotInGraph {
                node: graph.starting_node,
            });
        }

        for (node_name, edges) in &graph.nodes {
            #[cfg(test)]
            {
                println!("node {:?}, edges {:?}", node_name, edges);
            }

            for (i, edge) in edges.iter().enumerate() {
                let direct = graph.direct(node_name, i);
                #[cfg(test)]
                {
                    println!("\tdirect(edge[{i}]) = {:?}", direct);
                }

                for (j, other_edge) in edges.iter().enumerate().take(i) {
                    let other_direct = graph.direct(node_name, j);
                    #[cfg(test)]
                    {
                        println!("\t\tdirect(edge[{j}]) = {:?}", other_direct);
                    }

                    if direct.intersection(&other_direct).count() != 0
                        && (edge.brackets.iter().any(|b| b.is_opening())
                            || other_edge.brackets.iter().any(|b| b.is_opening())
                            || edge.brackets.is_empty()
                            || other_edge.brackets.is_empty()
                            || edge
                                .brackets
                                .iter()
                                .all(|b| other_edge.brackets.iter().all(|other_b| other_b.eq(b))))
                    {
                        return Err(Error::NonDeterministic {
                            at_node: node_name.clone(),
                        });
                    }
                }

                if let Some(target_name) = &edge.target {
                    if !graph.nodes.contains_key(target_name) {
                        return Err(Error::EdgeToUnknownTarget {
                            from: node_name.clone(),
                            to: target_name.clone(),
                        });
                    }
                }
            }
        }

        Ok(graph)
    }

    pub fn direct<Q>(&self, node_name: &Q, edge_index: usize) -> HashSet<&Option<T>>
    where
        Q: ?Sized,
        L: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut res = HashSet::new();

        if let Some((node, edges)) = self.nodes.get_key_value(node_name) {
            let mut cur_edges = vec![(node, edges, edge_index)];
            let mut visited = HashSet::from([(node, edge_index)]);

            loop {
                let next_edges: Vec<_> = cur_edges
                    .iter()
                    .filter_map(|(_, edges, edge)| edges.get(*edge))
                    .filter_map(|edge| {
                        res.insert(&edge.token);
                        edge.target
                            .as_ref()
                            .filter(|_| edge.token.is_none())
                            .and_then(|target_name| self.nodes.get_key_value(target_name.borrow()))
                    })
                    .collect();

                if next_edges.is_empty() {
                    break;
                }

                cur_edges.clear();
                for (node, edges) in next_edges {
                    for i in 0..edges.len() {
                        if !visited.contains(&(node, i)) {
                            cur_edges.push((node, edges, i));
                            visited.insert((node, i));
                        }
                    }
                }
            }
        }
        res
    }

    pub fn traverse<'a>(&self, string: &'a [T]) -> Result<&'a [T], Error<T, B, L>> {
        let mut bracket_stack: HashMap<usize, Vec<B>> = HashMap::new();
        let mut cur_node = &self.starting_node;

        let mut i = 0;

        loop {
            let token = string.get(i);

            #[cfg(test)]
            println!(
                "cur string: {:?}, cur node: {:?}",
                string[i..].to_vec(),
                self.nodes.get_key_value(cur_node).unwrap()
            );

            let next = self
                .nodes
                .get(cur_node)
                .unwrap()
                .iter()
                .filter(|edge| edge.token.as_ref().eq(&token) || edge.token.is_none())
                .find(|edge| {
                    edge.brackets.iter().all(|bracket| {
                        let index = bracket.kind_index();
                        let cur_top = bracket_stack.entry(index).or_default().last();

                        if bracket.is_opening() {
                            true
                        } else if let Some(cur_top) = cur_top {
                            bracket.closes(cur_top)
                        } else {
                            false
                        }
                    })
                })
                .map(|edge| {
                    #[cfg(test)]
                    {
                        println!(
                            "found edge {:?} from {:?} for {:?}, using token {:?}",
                            edge,
                            cur_node,
                            string[i..].to_vec(),
                            edge.token
                        );
                    }

                    edge.brackets.iter().for_each(|bracket| {
                        bracket_stack
                            .entry(bracket.kind_index())
                            .and_modify(|stack| {
                                if bracket.is_closing() {
                                    stack.pop();
                                } else {
                                    stack.push(bracket.clone())
                                }
                            });
                    });
                    edge
                })
                .map(|edge| (edge.target.as_ref(), edge.token.as_ref()));

            if let Some((next_node, used_token)) = next {
                if used_token.is_some() {
                    i += 1;
                }

                #[cfg(test)]
                {
                    println!("stack: {:?}", bracket_stack);
                }

                if let Some(next_node) = next_node {
                    cur_node = next_node;
                } else if bracket_stack.iter().all(|(_, stack)| stack.is_empty()) {
                    return Ok(&string[i..]);
                } else {
                    return Err(Error::UnbalancedBrackets {
                        bracket_stack,
                        string: string[i..].to_vec(),
                    });
                }
            } else {
                return Err(Error::NoWayToContinue {
                    at_node: cur_node.clone(),
                    string: string[i..].to_vec(),
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn edge_construction() {
        let e: Result<Edge<char, DefaultBracket, String>, Error<_, _, _>> =
            Edge::new(Some("goodbye"), Some('a'), vec![]);
        assert_eq!(
            e,
            Ok(Edge {
                target: Some("goodbye".to_string()),
                token: Some('a'),
                brackets: vec![]
            })
        );

        let e: Result<Edge<char, DefaultBracket, String>, Error<_, _, _>> = Edge::new(
            None::<String>,
            None::<char>,
            vec![DefaultBracket::AngleOpen(0), DefaultBracket::RoundClose(1)],
        );

        assert_eq!(
            e,
            Ok(Edge {
                target: None,
                token: None,
                brackets: vec![DefaultBracket::AngleOpen(0), DefaultBracket::RoundClose(1)]
            })
        );

        let e: Result<Edge<char, DefaultBracket, String>, Error<_, _, _>> = Edge::new(
            None::<String>,
            None::<char>,
            vec![DefaultBracket::AngleOpen(0), DefaultBracket::AngleClose(1)],
        );

        assert_eq!(e, Err(Error::InvalidBrackets));
    }

    #[test]
    fn direct() {
        let g: LGraph<char, DefaultBracket, String> = LGraph {
            starting_node: "1".to_string(),
            nodes: HashMap::from([
                (
                    "1".to_string(),
                    vec![Edge::new(Some("2"), None, vec![]).unwrap()],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('a'), vec![]).unwrap(),
                        Edge::new(Some("3"), None, vec![]).unwrap(),
                    ],
                ),
                (
                    "3".to_string(),
                    vec![
                        Edge::new(Some("3"), Some('b'), vec![]).unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
            ]),
        };

        assert_eq!(
            g.direct("1", 0),
            HashSet::from([&None, &Some('a'), &Some('b')])
        );
        assert_eq!(g.direct("2", 1), HashSet::from([&None, &Some('b')]));
        assert_eq!(g.direct("2", 0), HashSet::from([&Some('a')]));
        assert_eq!(g.direct("3", 0), HashSet::from([&Some('b')]));
        assert_eq!(g.direct("3", 1), HashSet::from([&None]));
    }

    #[test]
    fn new() {
        let _: LGraph<char, DefaultBracket, String> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![Edge::new(Some("2"), Some('a'), vec![]).unwrap()],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('a'), vec![]).unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
            ],
            "1",
        )
        .unwrap();

        let err: Result<LGraph<char, DefaultBracket, String>, _> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![
                        Edge::new(Some("1"), Some('a'), vec![]).unwrap(),
                        Edge::new(Some("2"), Some('a'), vec![]).unwrap(),
                    ],
                ),
                (
                    "2".to_string(),
                    vec![Edge::new(None::<String>, None, vec![]).unwrap()],
                ),
            ],
            "1",
        );
        assert!(err.is_err());

        let _: LGraph<char, DefaultBracket, String> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![
                        Edge::new(Some("1"), Some('a'), vec![]).unwrap(),
                        Edge::new(Some("2"), None, vec![]).unwrap(),
                    ],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('b'), vec![]).unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
            ],
            "1",
        )
        .unwrap();

        let err: Result<LGraph<char, DefaultBracket, String>, _> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![
                        Edge::new(Some("1"), Some('a'), vec![]).unwrap(),
                        Edge::new(Some("2"), None, vec![]).unwrap(),
                    ],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('b'), vec![]).unwrap(),
                        Edge::new(Some("2"), None, vec![]).unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
            ],
            "1",
        );

        assert!(err.is_err());

        let _: LGraph<char, DefaultBracket, String> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![Edge::new(Some("2"), None, vec![DefaultBracket::RoundOpen(1)]).unwrap()],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('a'), vec![DefaultBracket::RoundOpen(2)])
                            .unwrap(),
                        Edge::new(Some("3"), None, vec![]).unwrap(),
                    ],
                ),
                (
                    "3".to_string(),
                    vec![
                        Edge::new(Some("3"), None, vec![DefaultBracket::RoundClose(2)]).unwrap(),
                        Edge::new(Some("4"), None, vec![DefaultBracket::RoundClose(1)]).unwrap(),
                    ],
                ),
                (
                    "4".to_string(),
                    vec![Edge::new(None::<String>, None, vec![]).unwrap()],
                ),
            ],
            "1",
        )
        .unwrap();

        let err: Result<LGraph<char, DefaultBracket, String>, _> = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![Edge::new(Some("2"), None, vec![DefaultBracket::RoundOpen(1)]).unwrap()],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('a'), vec![DefaultBracket::RoundOpen(1)])
                            .unwrap(),
                        Edge::new(Some("3"), None, vec![]).unwrap(),
                    ],
                ),
                (
                    "3".to_string(),
                    vec![
                        Edge::new(Some("3"), None, vec![DefaultBracket::RoundClose(1)]).unwrap(),
                        Edge::new(Some("4"), None, vec![DefaultBracket::RoundClose(1)]).unwrap(),
                    ],
                ),
                (
                    "4".to_string(),
                    vec![Edge::new(None::<String>, None, vec![]).unwrap()],
                ),
            ],
            "1",
        );

        assert!(err.is_err());
    }

    #[test]
    fn traverse_anban() {
        let g = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![
                        Edge::new(Some("1"), Some('a'), vec![DefaultBracket::RoundOpen(1)])
                            .unwrap(),
                        Edge::new(Some("2"), Some('b'), vec![]).unwrap(),
                    ],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("2"), Some('a'), vec![DefaultBracket::RoundClose(1)])
                            .unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
            ],
            "1",
        )
        .unwrap();

        assert_eq!(
            g.traverse("b".chars().collect::<Vec<_>>().as_slice()),
            Ok([].as_slice())
        );
        assert_eq!(
            g.traverse("aba".chars().collect::<Vec<_>>().as_slice()),
            Ok([].as_slice())
        );
        assert_eq!(
            g.traverse(
                "aaaaaaaaaaaaabaaaaaaaaaaaaa"
                    .chars()
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok([].as_slice())
        );
        assert_eq!(
            g.traverse("abaa".chars().collect::<Vec<_>>().as_slice()),
            Ok(['a'].as_slice())
        );
        assert_eq!(
            g.traverse("ab".chars().collect::<Vec<_>>().as_slice()),
            Err(Error::UnbalancedBrackets {
                bracket_stack: HashMap::from([(0, vec![DefaultBracket::RoundOpen(1)])]),
                string: vec![]
            })
        );
        assert!(g
            .traverse("c".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("".chars().collect::<Vec<_>>().as_slice())
            .is_err());
        assert!(g
            .traverse("aaabaab".chars().collect::<Vec<_>>().as_slice())
            .is_err());
    }

    #[test]
    fn traverse_exp2() {
        let g = LGraph::new(
            [
                (
                    "1".to_string(),
                    vec![
                        Edge::new(Some("1"), Some('|'), vec![DefaultBracket::RoundOpen(1)])
                            .unwrap(),
                        Edge::new(Some("2"), Some('='), vec![DefaultBracket::RoundOpen(2)])
                            .unwrap(),
                    ],
                ),
                (
                    "2".to_string(),
                    vec![
                        Edge::new(Some("3"), Some('|'), vec![DefaultBracket::RoundClose(2)])
                            .unwrap(),
                        Edge::new(Some("4"), None, vec![DefaultBracket::AngleOpen(1)]).unwrap(),
                    ],
                ),
                (
                    "3".to_string(),
                    vec![
                        Edge::new(Some("3"), Some('|'), vec![DefaultBracket::RoundClose(2)])
                            .unwrap(),
                        Edge::new(None::<String>, None, vec![]).unwrap(),
                    ],
                ),
                (
                    "4".to_string(),
                    vec![
                        Edge::new(
                            Some("4"),
                            None,
                            vec![DefaultBracket::RoundClose(2), DefaultBracket::AngleOpen(2)],
                        )
                        .unwrap(),
                        Edge::new(Some("5"), None, vec![DefaultBracket::RoundClose(1)]).unwrap(),
                    ],
                ),
                (
                    "5".to_string(),
                    vec![
                        Edge::new(Some("2"), None, vec![DefaultBracket::AngleClose(1)]).unwrap(),
                        Edge::new(
                            Some("6"),
                            None,
                            vec![DefaultBracket::RoundOpen(2), DefaultBracket::AngleClose(2)],
                        )
                        .unwrap(),
                    ],
                ),
                (
                    "6".to_string(),
                    vec![Edge::new(Some("5"), None, vec![DefaultBracket::RoundOpen(2)]).unwrap()],
                ),
            ],
            "1",
        )
        .unwrap();

        assert_eq!(
            g.traverse("||=||||".chars().collect::<Vec<_>>().as_slice()),
            Ok([].as_slice())
        );
        assert_eq!(
            g.traverse("|||=||||||||".chars().collect::<Vec<_>>().as_slice()),
            Ok([].as_slice())
        );
        assert_eq!(
            g.traverse(
                "||||=||||||||||||||||"
                    .chars()
                    .collect::<Vec<_>>()
                    .as_slice()
            ),
            Ok([].as_slice())
        );
    }
}
