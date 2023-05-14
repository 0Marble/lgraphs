use std::{collections::HashSet, fmt::Display, str::FromStr};

use crate::{
    graph::{
        default_graph::DefaultGraph,
        lgraph::{LGraph, LGraphLetter},
        Graph, ModifyableGraph,
    },
    path::{Edge, Letter, Node},
};

impl<G, N, L> FromStr for LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>> + FromStr,
    N: Node,
    L: Letter,
{
    type Err = G::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(G::from_str(s)?))
    }
}

impl<G, N, L> Display for LGraph<G, N, L>
where
    G: Graph<N, LGraphLetter<L>> + Display,
    N: Node,
    L: Letter,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.graph())
    }
}

#[derive(Debug)]
pub enum DefaultGraphParseError {
    ParseError(String, usize, Box<dyn std::error::Error>),
    NoStartNode,
    ExpectedNode(String, usize),
    ExpectedLetter(String, usize),
}

impl std::error::Error for DefaultGraphParseError {}

impl Display for DefaultGraphParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// graph: (statement ';')*
// statement: ('->' node  | node '->' | node ('-' edge)? '->' node

impl<N> FromStr for DefaultGraph<N, LGraphLetter<char>>
where
    N: Node + FromStr,
    N::Err: std::error::Error + 'static,
{
    type Err = DefaultGraphParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        type Error = DefaultGraphParseError;
        let mut start_node = None;
        let mut end_nodes = HashSet::new();
        let mut edges = vec![];

        for (i, statement) in s.split(';').enumerate() {
            let statement = statement.trim();
            if statement.is_empty() {
                continue;
            }

            if let Some(node) = statement.strip_prefix("->") {
                start_node = Some(
                    node.trim_start()
                        .parse()
                        .map_err(|e| Error::ParseError(s.to_string(), i, Box::new(e)))?,
                );
                continue;
            }

            let (node, next) = statement
                .split_once('-')
                .ok_or_else(|| Error::ExpectedNode(s.to_string(), i))?;
            let from = node
                .trim_end()
                .parse()
                .map_err(|e| Error::ParseError(s.to_string(), i, Box::new(e)))?;
            let next = next.trim();
            if next == ">" {
                end_nodes.insert(from);
                continue;
            }

            let (edge, to) = if let Some(node) = next.strip_prefix('>') {
                (LGraphLetter::default(), node)
            } else {
                let (edge, node) = next
                    .split_once("->")
                    .ok_or_else(|| Error::ExpectedLetter(s.to_string(), i))?;
                (
                    edge.parse()
                        .map_err(|e| Error::ParseError(s.to_string(), i, Box::new(e)))?,
                    node,
                )
            };

            let to = to
                .parse()
                .map_err(|e| Error::ParseError(s.to_string(), i, Box::new(e)))?;
            edges.push(Edge::new(from, to, edge));
        }

        let Some(start_node) = start_node else {return Err(Error::NoStartNode);};
        let mut g = DefaultGraph::new(start_node, end_nodes);

        for edge in edges {
            g.add_edge(edge);
        }

        Ok(g)
    }
}

impl<N> Display for DefaultGraph<N, LGraphLetter<char>>
where
    N: Node + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "->{};", self.start_node())?;
        self.end_nodes()
            .try_for_each(|node| write!(f, "{}->;", node))?;

        for edge in self.edges() {
            write!(f, "{}", edge.beg())?;
            if edge.bracket().is_some() || edge.item().is_some() {
                write!(f, "-{}", edge.letter())?;
            }
            write!(f, "->{};", edge.end())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::path::Bracket;

    use super::*;

    #[test]
    fn parse() {
        let g = DefaultGraph::new(1, [2]);
        assert_eq!(DefaultGraph::from_str("->1;2->;").unwrap(), g);

        let g = DefaultGraph::new(1, [2, 3, 4]);
        assert_eq!(DefaultGraph::from_str("->1;2->;3->;4->;").unwrap(), g);

        let mut g = DefaultGraph::new(1, [2]);
        g.add_edge(Edge::new(1, 2, LGraphLetter::default()));
        assert_eq!(DefaultGraph::from_str("->1;2->;1->2;").unwrap(), g);

        let mut g = DefaultGraph::new(1, [2]);
        g.add_edge(Edge::new(1, 2, LGraphLetter::default()));
        g.add_edge(Edge::new(2, 3, LGraphLetter::default()));
        assert_eq!(DefaultGraph::from_str("->1;2->;1->2;2->3;").unwrap(), g);

        let mut g = DefaultGraph::new(1, [2]);
        g.add_edge(Edge::new(
            1,
            2,
            LGraphLetter::new(Some('a'), Some(Bracket::new(0, true))),
        ));
        assert_eq!(DefaultGraph::from_str("->1;2->;1-a[0->2;").unwrap(), g);
    }
}
