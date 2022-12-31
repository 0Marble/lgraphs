/*
    ident '('token?')' '{' bracket (, bracket)* '}' '('ident?')'
*/

use std::{collections::HashMap, fmt::Debug, hash::Hash, str::FromStr};

use super::{
    defaults::{Edge, LGraph},
    lgraph_trait::{Bracket, BracketSet, Label, TargetNode, Token},
};

#[derive(Debug, Clone)]
pub enum LGraphParseError<T, B, L>
where
    T: FromStr,
    L: FromStr,
    B: FromStr,
{
    ExpectedLabel { line: usize },
    ExpectedToken { line: usize },
    ExpectedBrackets { line: usize },
    ExpectedDelimeter { line: usize, delimeter: String },
    TokenParse { line: usize, err: T::Err },
    BracketParse { line: usize, err: B::Err },
    LabelParse { line: usize, err: L::Err },
}

impl<'a, T, B, L, BS> FromStr for LGraph<'a, T, B, L, BS>
where
    T: FromStr + Token,
    B: FromStr + Bracket + 'a,
    L: FromStr + Clone + Eq + Hash + Label,
    BS: FromIterator<B> + BracketSet<'a, B>,
{
    type Err = LGraphParseError<T, B, L>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let edges = s
            .lines()
            .map(|s| s.trim())
            .enumerate()
            .filter(|(_, line)| !line.is_empty())
            .map(|(line_num, line)| -> Result<_, LGraphParseError<T, B, L>> {
                let mut split = line.split_whitespace();
                let from = split
                    .next()
                    .ok_or(LGraphParseError::ExpectedLabel { line: line_num })
                    .map(|s| s.trim())
                    .and_then(|from| {
                        L::from_str(from).map_err(|e| LGraphParseError::LabelParse::<T, B, L> {
                            line: line_num,
                            err: e,
                        })
                    })?;

                let token = split
                    .next()
                    .ok_or(LGraphParseError::ExpectedToken { line: line_num })
                    .and_then(|token| {
                        token
                            .strip_prefix('(')
                            .and_then(|t| t.strip_suffix(')'))
                            .ok_or(LGraphParseError::ExpectedDelimeter {
                                line: line_num,
                                delimeter: "( )".to_string(),
                            })
                            .map(|s| s.trim())
                    })
                    .and_then(|token| {
                        if token.is_empty() {
                            Ok(None)
                        } else {
                            Ok(Some(T::from_str(token).map_err(|e| {
                                LGraphParseError::TokenParse::<T, B, L> {
                                    line: line_num,
                                    err: e,
                                }
                            })?))
                        }
                    })?;

                let brackets = split
                    .next()
                    .ok_or(LGraphParseError::ExpectedBrackets::<T, B, L> { line: line_num })
                    .and_then(|brackets| {
                        brackets
                            .strip_prefix('{')
                            .and_then(|brackets| brackets.strip_suffix('}'))
                            .ok_or(LGraphParseError::ExpectedDelimeter {
                                line: line_num,
                                delimeter: "{ }".to_string(),
                            })
                            .map(|s| s.trim())
                    })
                    .and_then(|brackets| {
                        brackets
                            .split(',')
                            .map(|bracket| bracket.trim())
                            .filter(|s| !s.is_empty())
                            .map(|bracket| {
                                B::from_str(bracket).map_err(|e| LGraphParseError::BracketParse {
                                    line: line_num,
                                    err: e,
                                })
                            })
                            .collect::<Result<BS, _>>()
                    })?;

                let to = split
                    .next()
                    .ok_or(LGraphParseError::ExpectedLabel { line: line_num })
                    .and_then(|s| {
                        s.strip_prefix('(')
                            .and_then(|s| s.strip_suffix(')'))
                            .ok_or(LGraphParseError::ExpectedDelimeter {
                                line: line_num,
                                delimeter: "( )".to_string(),
                            })
                            .map(|s| s.trim())
                    })
                    .and_then(|to| {
                        if to.is_empty() {
                            Ok(None)
                        } else {
                            Ok(Some(L::from_str(to).map_err(|e| {
                                LGraphParseError::LabelParse::<T, B, L> {
                                    line: line_num,
                                    err: e,
                                }
                            })?))
                        }
                    })?;

                Ok((from, token, brackets, to))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut node_names = vec![];
        for name in edges
            .iter()
            .map(|(from, _, _, _)| from)
            .chain(edges.iter().filter_map(|(_, _, _, to)| to.as_ref()))
        {
            if node_names.iter().all(|old_name: &L| old_name.ne(name)) {
                node_names.push(name.clone());
            }
        }

        let node_indices = node_names
            .iter()
            .enumerate()
            .map(|(i, n)| (n, i))
            .collect::<HashMap<_, _>>();
        let edges = edges
            .into_iter()
            .map(|(from, token, brackets, to)| {
                let from = *node_indices.get(&from).unwrap();
                let to = to.map_or(TargetNode::End, |to| {
                    TargetNode::Node(*node_indices.get(&to).unwrap())
                });
                Edge::new(from, token, brackets, to)
            })
            .collect::<Vec<_>>();

        Ok(Self::new(edges, node_names))
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::lgraph::defaults::{Bracket, LGraph};

    use super::*;

    #[test]
    fn parse() -> Result<(), LGraphParseError<char, Bracket, String>> {
        let s = "
            A (a) {[0,<0} (B)
            B (b) {} (C)
            C ()  {} () 
        ";

        let _: LGraph<char, Bracket, String> = LGraph::from_str(s)?;

        Ok(())
    }
}
