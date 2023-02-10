use std::fmt::Display;

use json::JsonValue;

use crate::graphs::{
    graph_trait::{Builder, Graph},
    lgraph::{Bracket, BracketType, Item, LGraph},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Label {
    Int(i32),
    Char(char),
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Int(i) => write!(f, "{}", i),
            Label::Char(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    ExpectedLabel(String),
    ExpectedArrayOfLabels(String),
    ExpectedArrayOfEdges(String),
    ExpectedBracket(String),
    ExpectedBool(String),
    ExpectedNumber(String),
    ExpectedArrayOfLocations,
    ExpectedLocation(usize),
    NoSuchNode(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

pub fn read_label(val: &JsonValue, field_name: &str) -> Result<Label, Box<dyn std::error::Error>> {
    let label = val
        .as_i32()
        .map(Label::Int)
        .or_else(|| val.as_str().and_then(|s| s.chars().next()).map(Label::Char))
        .ok_or_else(|| ParseError::ExpectedLabel(field_name.to_string()))?;

    Ok(label)
}

fn read_edge(
    val: &JsonValue,
    field_name: &str,
) -> Result<(Label, Option<Label>, Label), Box<dyn std::error::Error>> {
    let source = read_label(&val["source"], &format!("{}[source]", field_name))?;
    let target = read_label(&val["target"], &format!("{}[target]", field_name))?;
    let item = read_label(&val["item"], &format!("{}[item]", field_name)).ok();

    Ok((source, item, target))
}

pub fn read_graph<B>(
    src: &str,
    builder: &mut B,
) -> Result<B::TargetGraph, Box<dyn std::error::Error>>
where
    B: Builder<Label, Option<Label>>,
{
    let json = json::parse(src)?;
    let start_node = read_label(&json["start_node"], "start_node")?;
    let end_nodes = match &json["end_nodes"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_label(v, &format!("end_nodes[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfLabels("end_nodes".to_string()))?,
    };
    let edges = match &json["edges"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_edge(v, &format!("edges[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfEdges("edges".to_string()))?,
    };

    builder.clear();
    for (source, edge, target) in edges {
        builder.add_edge(source, edge, target);
    }

    Ok(builder.build(start_node, end_nodes))
}

fn read_lgraph_edge(
    val: &JsonValue,
    field_name: &str,
) -> Result<(Label, Item<Label>, Label), Box<dyn std::error::Error>> {
    let source = read_label(&val["source"], &format!("{}[source]", field_name))?;
    let target = read_label(&val["target"], &format!("{}[target]", field_name))?;
    let item = read_label(&val["item"], &format!("{}[item]", field_name)).ok();
    let bracket = match &val["bracket"] {
        JsonValue::Object(obj) => Bracket::new(
            obj["index"].as_usize().ok_or_else(|| {
                ParseError::ExpectedNumber(format!("{}[bracket][index]", field_name))
            })?,
            match obj["is_open"].as_bool() {
                Some(true) => BracketType::Open,
                Some(false) => BracketType::Close,
                _ => Err(ParseError::ExpectedBool(format!(
                    "{}[bracket][is_open]",
                    field_name
                )))?,
            },
        ),
        _ => Err(ParseError::ExpectedBracket(format!(
            "{}[bracket]",
            field_name
        )))?,
    };

    Ok((source, Item::new(item, bracket), target))
}

pub fn read_lgraph<B>(
    src: &str,
    builder: &mut B,
) -> Result<LGraph<Label, Label, B::TargetGraph>, Box<dyn std::error::Error>>
where
    B: Builder<Label, Item<Label>>,
    B::TargetGraph: Graph<Label, Item<Label>>,
{
    let json = json::parse(src)?;
    let start_node = read_label(&json["start_node"], "start_node")?;
    let end_nodes = match &json["end_nodes"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_label(v, &format!("end_nodes[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfLabels("end_nodes".to_string()))?,
    };
    let edges = match &json["edges"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_lgraph_edge(v, &format!("edges[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfEdges("edges".to_string()))?,
    };

    builder.clear();
    for (source, edge, target) in edges {
        builder.add_edge(source, edge, target);
    }

    Ok(LGraph::new_unchecked(builder.build(start_node, end_nodes)))
}
