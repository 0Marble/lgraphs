use crate::{
    graph::{
        default_graph::DefaultGraph,
        lgraph::{LGraph, LGraphLetter},
        Graph,
    },
    path::{Bracket, Edge, Letter, Node, Path},
};
use std::{fmt::Display, fmt::Write, str::FromStr};

// format: path = number | number ('-' letter )? ('-' '[' | ']' digit )? '->' path
impl FromStr for Path<usize, LGraphLetter<char>> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum State {
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
        }

        fn make_error(s: &str, i: usize, msg: &str) -> Result<(), String> {
            Err(format!(
                "Error parsing path:\n\"{}\"\n {}^: {}",
                s,
                (0..i).map(|_| '~').collect::<String>(),
                msg
            ))
        }

        fn read_number(
            s: &str,
            i: usize,
            it: &mut impl Iterator<Item = (usize, char)>,
        ) -> Option<usize> {
            let mut res = 0;
            if let Some(c) = s.chars().nth(i) {
                if c.is_ascii_digit() {
                    res = res * 10 + c.to_digit(10).unwrap();
                } else {
                    return None;
                }
            }

            for i in i + 1..s.len() {
                if let Some(c) = s.chars().nth(i) {
                    if c.is_ascii_digit() {
                        res = res * 10 + c.to_digit(10).unwrap();
                        it.next();
                    } else {
                        break;
                    }
                }
            }

            Some(res as usize)
        }

        let mut path: Option<Path<_, _>> = None;
        let mut prev_node = 0;
        let mut item = None;
        let mut bracket = None;

        let mut state = State::A;
        let mut it = s.char_indices();
        loop {
            let Some((i, c)) = it.next() else {
                break;
            };
            if c.is_whitespace() {
                continue;
            }

            match state {
                State::A => {
                    if let Some(node) = read_number(s, i, &mut it) {
                        if let Some(path) = path.as_mut() {
                            path.add_edge(Edge::new(
                                prev_node,
                                node,
                                LGraphLetter::new(item.take(), bracket.take()),
                            ));
                        } else {
                            path = Some(Path::new(node));
                        }

                        prev_node = node;
                        state = State::B;
                    } else {
                        make_error(s, i, "Expected a digit")?;
                    }
                }
                State::B => {
                    if c == '-' {
                        state = State::C;
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::C => match c {
                    '[' => {
                        bracket = Some(Bracket::new(0, true));
                        state = State::E;
                    }
                    ']' => {
                        bracket = Some(Bracket::new(0, false));
                        state = State::E;
                    }
                    c if c.is_alphabetic() => {
                        item = Some(c);
                        state = State::D;
                    }
                    '>' => state = State::A,
                    _ => make_error(s, i, "Expected a letter, '>', '[' or ']'")?,
                },
                State::D => {
                    if c == '-' {
                        state = State::F
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::E => {
                    if let Some(index) = read_number(s, i, &mut it) {
                        let b = bracket.clone().unwrap();
                        *bracket.as_mut().unwrap() = Bracket::new(index, b.is_open());

                        state = State::G;
                    } else {
                        make_error(s, i, "Expected a digit")?;
                    }
                }
                State::F => match c {
                    '[' => {
                        bracket = Some(Bracket::new(0, true));
                        state = State::E;
                    }
                    ']' => {
                        bracket = Some(Bracket::new(0, false));
                        state = State::E;
                    }
                    '>' => state = State::A,
                    _ => make_error(s, i, "Expected a '>', '[' or ']'")?,
                },
                State::G => {
                    if c == '-' {
                        state = State::H
                    } else {
                        make_error(s, i, "Expected a '-'")?;
                    }
                }
                State::H => {
                    if c == '>' {
                        state = State::A
                    } else {
                        make_error(s, i, "Expected a '>'")?;
                    }
                }
            }
        }

        let i = s.len();
        match state {
            State::A => make_error(s, i, "Expected a letter")?,
            State::B => {}
            State::C => make_error(s, i, "Expected a digit, '>', '[' or ']'")?,
            State::D => make_error(s, i, "Expected a '-'")?,
            State::E => make_error(s, i, "Expected a digit")?,
            State::F => make_error(s, i, "Expected a '>', '[' or ']'")?,
            State::G => make_error(s, i, "Expected a '-'")?,
            State::H => make_error(s, i, "Expected a '>'")?,
        }

        Ok(path.unwrap())
    }
}

use dot_parser::{ast::Graph as AstDotGraph, canonical::Graph as DotGraph};

#[derive(Debug)]
pub enum DefaultGraphParseError {
    DotError(Box<dyn std::error::Error>),
    NoStartNode,
}

impl Display for DefaultGraphParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for DefaultGraphParseError {}

impl<N, L> DefaultGraph<N, L>
where
    L: Letter,
    N: Node,
{
    pub fn parse_dot_custom<F1, F2, E1, E2>(
        s: &str,
        edge_attr_reader: F1,
        node_transformer: F2,
    ) -> Result<Self, DefaultGraphParseError>
    where
        F1: Fn((&str, &str)) -> Option<Result<L, E1>>,
        F2: Fn(&str) -> Result<N, E2>,
        E1: std::error::Error + 'static,
        E2: std::error::Error + 'static,
    {
        let g = DotGraph::from(
            AstDotGraph::read_dot(s).map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?,
        );

        let start_node = g
            .nodes
            .set
            .iter()
            .find(|(_, node)| node.attr.elems.contains(&("start", "true")))
            .map(|(name, _)| name)
            .ok_or(DefaultGraphParseError::NoStartNode)?;

        let end_nodes: Vec<_> = g
            .nodes
            .set
            .iter()
            .filter(|(_, node)| node.attr.elems.contains(&("end", "true")))
            .map(|(name, _)| node_transformer(name))
            .collect::<Result<_, _>>()
            .map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?;

        let mut res = DefaultGraph::new(
            node_transformer(start_node)
                .map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?,
            end_nodes,
        );

        for edge in g.edges.set {
            for letter in edge.attr.elems.into_iter().filter_map(&edge_attr_reader) {
                let letter = letter.map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?;
                res.add_edge(Edge::new(
                    node_transformer(edge.from)
                        .map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?,
                    node_transformer(edge.to)
                        .map_err(|e| DefaultGraphParseError::DotError(Box::new(e)))?,
                    letter,
                ));
            }
        }

        Ok(res)
    }
}

#[derive(Debug)]
pub enum LGraphLetterParseError {
    ExpectedDigit,
    ExpectedLetterOrBrackets,
    ExpectedBrackets,
}

impl std::error::Error for LGraphLetterParseError {}

impl Display for LGraphLetterParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl FromStr for LGraphLetter<char> {
    type Err = LGraphLetterParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // (letter)? ('[' | ']' number)?
        // A - letter -> B
        // A - '[' | ']' -> C
        // B - '[' | ']' -> C
        // B - end -> D
        // C - digit -> C
        // C - end -> D

        let mut letter = None;
        let mut had_bracket = false;
        let mut open = false;
        let mut index = 0;

        #[derive(Debug, Clone, PartialEq, Eq)]
        enum State {
            A,
            B,
            C,
        }

        let mut state = State::A;
        for c in s.chars() {
            if c.is_whitespace() {
                continue;
            }

            match state {
                State::A => match c {
                    '[' => {
                        had_bracket = true;
                        open = true;
                        state = State::C;
                    }
                    ']' => {
                        had_bracket = true;
                        open = false;
                        state = State::C;
                    }
                    c if c.is_alphabetic() => {
                        letter = Some(c);
                        state = State::B;
                    }
                    _ => Err(LGraphLetterParseError::ExpectedLetterOrBrackets)?,
                },
                State::B => match c {
                    '[' => {
                        had_bracket = true;
                        open = true;
                        state = State::C;
                    }
                    ']' => {
                        had_bracket = true;
                        open = false;
                        state = State::C;
                    }
                    _ => Err(LGraphLetterParseError::ExpectedBrackets)?,
                },
                State::C => {
                    if c.is_ascii_digit() {
                        index = index * 10
                            + c.to_digit(10)
                                .ok_or(LGraphLetterParseError::ExpectedDigit)?
                                as usize;
                    } else {
                        Err(LGraphLetterParseError::ExpectedDigit)?
                    }
                }
            }
        }

        Ok(Self::new(
            letter,
            if had_bracket {
                Some(Bracket::new(index, open))
            } else {
                None
            },
        ))
    }
}

impl DefaultGraph<usize, LGraphLetter<char>> {
    pub fn parse_dot(s: &str) -> Result<Self, DefaultGraphParseError> {
        Self::parse_dot_custom(
            s,
            |(attr, val)| match attr {
                "item" => Some(LGraphLetter::from_str(
                    val.trim_start_matches('\"').trim_end_matches('\"'),
                )),
                _ => None,
            },
            |name| name.parse(),
        )
    }

    pub fn write_dot(&self) -> String {
        let mut s = String::new();
        writeln!(s, "digraph {{\n\tnode [shape=circle];\n\tQ0 [style=invisible, height=0, width=0, fixedsize=true];").unwrap();

        writeln!(s, "\n\t{} [start=true];", self.start_node()).unwrap();
        writeln!(s, "\tQ0 -> {};\n", self.start_node()).unwrap();
        for node in self.end_nodes() {
            writeln!(s, "\t{} [end=true];", node).unwrap();
        }
        writeln!(s).unwrap();

        for node in self.nodes() {
            writeln!(s, "\t{};", node).unwrap();
        }
        writeln!(s).unwrap();

        for edge in self.edges() {
            write!(s, "\t{} -> {}", edge.beg(), edge.end()).unwrap();
            let mut item_string = String::new();
            let mut label_string = String::new();
            if let Some(item) = edge.item() {
                item_string.push(*item);
                label_string.push(*item);
            }
            if let Some(bracket) = edge.bracket() {
                write!(
                    item_string,
                    "{}{}",
                    if bracket.is_open() { '[' } else { ']' },
                    bracket.index()
                )
                .unwrap();

                if !label_string.is_empty() {
                    label_string.push('\\');
                    label_string.push('n');
                }

                write!(
                    label_string,
                    "{}{}",
                    if bracket.is_open() { '[' } else { ']' },
                    bracket.index()
                )
                .unwrap();
            }

            if !label_string.is_empty() {
                write!(s, " [item=\"{}\", label=\"{}\"]", item_string, label_string).unwrap();
            }
            writeln!(s, ";").unwrap();
        }

        write!(s, "}}").unwrap();
        s
    }
}

impl LGraph<DefaultGraph<usize, LGraphLetter<char>>, usize, char> {
    pub fn parse_dot(s: &str) -> Result<Self, DefaultGraphParseError> {
        Ok(Self::new(DefaultGraph::<_, LGraphLetter<_>>::parse_dot(s)?))
    }

    pub fn write_dot(&self) -> String {
        self.graph().write_dot()
    }
}

impl Display for Path<usize, LGraphLetter<char>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.beg())?;

        for edge in self.edges() {
            if let Some(item) = edge.item() {
                write!(f, "-{}", item)?;
            }

            if let Some(bracket) = edge.bracket() {
                write!(
                    f,
                    "-{}{}",
                    if bracket.is_open() { '[' } else { ']' },
                    bracket.index()
                )?;
            }

            write!(f, "->{}", edge.end())?;
        }

        Ok(())
    }
}
