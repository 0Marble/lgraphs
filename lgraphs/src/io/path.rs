use std::{fmt::Display, str::FromStr, write, writeln};

use crate::{
    graph::lgraph::LGraphLetter,
    path::{Bracket, BracketStack, Edge, Letter, Memory, Node, Path},
};

#[derive(Debug)]
pub enum LGraphPathParseError {
    ExpectedDigit(String, usize),
    ParseError(String, usize, Box<dyn std::error::Error>),
}

impl LGraphPathParseError {
    fn str_and_offset(&self) -> (&str, usize) {
        match self {
            LGraphPathParseError::ExpectedDigit(s, i) => (s, *i),
            LGraphPathParseError::ParseError(s, i, _) => (s, *i),
        }
    }

    fn error_msg(&self) -> String {
        match self {
            LGraphPathParseError::ExpectedDigit(_, _) => "Expected a digit".to_string(),
            LGraphPathParseError::ParseError(_, _, e) => e.to_string(),
        }
    }
}

impl std::error::Error for LGraphPathParseError {}

impl Display for LGraphPathParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (s, offset) = self.str_and_offset();
        writeln!(f, "{}", s)?;
        (0..offset).try_for_each(|_| write!(f, "~"))?;
        writeln!(f, "^: Error: {}", self.error_msg())
    }
}

impl FromStr for Path<usize, LGraphLetter<char>> {
    type Err = LGraphPathParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split('-');
        type Error = LGraphPathParseError;

        let mut t: Option<Self> = None;
        let mut letter = LGraphLetter::default();
        let mut offset = 0;
        let mut next_node = None;

        loop {
            let node = next_node
                .take()
                .or_else(|| it.next())
                .map(|n| {
                    offset += n.chars().count();
                    n
                })
                .map(|n| n.trim_start_matches('>'))
                .ok_or_else(|| Error::ExpectedDigit(s.to_string(), offset))?
                .parse()
                .map_err(|e| Error::ParseError(s.to_string(), offset, Box::new(e)))?;
            match t.as_mut() {
                Some(t) => {
                    t.add_edge(Edge::new(*t.end(), node, letter));
                    #[allow(unused_assignments)]
                    {
                        letter = LGraphLetter::default();
                    }
                }
                None => t = Some(Path::new(node)),
            }
            offset += 1;
            match it.next() {
                Some(next) if !next.starts_with('>') => {
                    letter = LGraphLetter::from_str(next)
                        .map_err(|e| Error::ParseError(s.to_string(), offset, Box::new(e)))?;
                    offset += next.chars().count();
                }
                Some(next) => {
                    if let Some(node) = next.strip_prefix('>') {
                        offset += 1;
                        next_node = Some(node);
                    }
                }
                None => break,
            }
        }

        Ok(t.unwrap())
    }
}

#[derive(Debug)]
pub enum LGraphLetterParseError {
    ExpectedDigit(usize),
    ExpectedLetterOrBrackets(usize),
    ExpectedBrackets(usize),
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
        // B - '|' -> B
        // B - ',' -> B
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
        for (i, c) in s.char_indices() {
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
                    _ => Err(LGraphLetterParseError::ExpectedLetterOrBrackets(i))?,
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
                    '|' | ',' => {}
                    _ => Err(LGraphLetterParseError::ExpectedBrackets(i))?,
                },
                State::C => {
                    if c.is_ascii_digit() {
                        index = index * 10
                            + c.to_digit(10)
                                .ok_or(LGraphLetterParseError::ExpectedDigit(i))?
                                as usize;
                    } else {
                        Err(LGraphLetterParseError::ExpectedDigit(i))?
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

impl Display for LGraphLetter<char> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(letter) = self.letter() {
            write!(f, "{}", letter)?;
        }

        if let Some(bracket) = self.bracket() {
            if self.letter().is_some() {
                write!(f, ",")?;
            }

            write!(
                f,
                "{}{}",
                if bracket.is_open() { '[' } else { ']' },
                bracket.index()
            )?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum MemoryParseError {
    ExpectedDigitOrOpenCurly(usize),
    ExpectedDigit(usize),
    ExpectedDigitOrComaOrCloseCurly(usize),
    ExpectedDigitOrCloseCurly(usize),
}

impl std::error::Error for MemoryParseError {}

impl Display for MemoryParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl FromStr for Memory<usize> {
    type Err = MemoryParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // number '{' (number )? (',' number)* '}'
        // A - digit -> A
        // A - '{' -> B
        // B - digit -> D
        // B - '}' -> End
        // C - digit -> D
        // D - digit -> D
        // D - ',' -> C
        // D - '}' -> End

        #[derive(Debug, PartialEq, Eq)]
        enum State {
            A,
            B,
            C,
            D,
        }
        let mut state = State::A;
        let mut node = 0;
        let mut indices = vec![];
        for (i, c) in s.char_indices() {
            if c.is_whitespace() {
                continue;
            }

            match state {
                State::A => match c {
                    '{' => state = State::B,
                    c if c.is_ascii_digit() => {
                        state = State::A;
                        node = node * 10 + c.to_digit(10).unwrap() as usize;
                    }
                    _ => Err(MemoryParseError::ExpectedDigitOrOpenCurly(i))?,
                },
                State::B => match c {
                    '}' => break,
                    c if c.is_ascii_digit() => {
                        state = State::D;
                        indices.push(c.to_digit(10).unwrap() as usize);
                    }
                    _ => Err(MemoryParseError::ExpectedDigitOrCloseCurly(i))?,
                },
                State::C => {
                    if c.is_ascii_digit() {
                        let cur = indices.last_mut().unwrap();
                        *cur = *cur * 10 + c.to_digit(10).unwrap() as usize;
                        state = State::D;
                    } else {
                        Err(MemoryParseError::ExpectedDigit(i))?
                    }
                }
                State::D => match c {
                    '}' => break,
                    ',' => {
                        state = State::C;
                        indices.push(0);
                    }
                    c if c.is_ascii_digit() => {
                        let cur = indices.last_mut().unwrap();
                        *cur = *cur * 10 + c.to_digit(10).unwrap() as usize;
                        state = State::D;
                    }
                    _ => Err(MemoryParseError::ExpectedDigitOrComaOrCloseCurly(i))?,
                },
            }
        }

        let mut stack = BracketStack::default();
        for index in indices {
            stack.accept(Bracket::new(index, true));
        }
        Ok(Memory::new(node, stack))
    }
}

impl Display for Memory<usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node())?;

        use std::fmt::Write;

        let mut s = String::new();
        for bracket in self.brackets().state() {
            write!(s, "{}, ", bracket)?;
        }
        write!(f, ",{{{}}}", s.trim_end_matches(", "))
    }
}

impl Display for Path<usize, LGraphLetter<char>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.beg())?;

        for edge in self.edges() {
            if edge.bracket().is_some() || edge.item().is_some() {
                write!(f, "-{}", edge.letter())?;
            }

            write!(f, "->{}", edge.end())?;
        }

        Ok(())
    }
}

impl<N, L> Path<N, L>
where
    N: Node + ToString,
    L: Letter + ToString,
{
    pub fn to_dot(&self) -> String {
        let mut s = "digraph {\n".to_string();
        s += "\tQ0 [style=invisible,height=0,width=0,fixedsize=true];\n";
        s += "\tQ1 [style=invisible,height=0,width=0,fixedsize=true];\n";
        s += "\tnode [shape=circle];\n\tgraph [rankdir=\"LR\"];\n";

        for (i, node) in self.nodes().enumerate() {
            s += &format!("\t{} [label=\"{}\"]", i, node.to_string());
        }

        s += "\tQ0 -> 0;\n";
        for (i, edge) in self.edges().enumerate() {
            s += &format!(
                "\t{} -> {} [label=\"{}\", fontcolor=red];\n",
                i,
                i + 1,
                edge.letter().to_string()
            );
        }

        s += &format!("\t{}->Q1;\n", self.len_in_nodes() - 1);

        s.push('}');
        s
    }
}
