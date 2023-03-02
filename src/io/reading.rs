use std::fmt::Display;

use crate::graphs::{
    graph_trait::{Builder, Graph},
    lgraph::Bracket,
};

use super::{Item, Label};

#[derive(Debug, Clone)]
pub enum Error {
    ExpectedString,
    Json(String),
    ExpectedNumber,
    ExpectedBool,
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

// fn read_label(val: &json::JsonValue) -> Result<Label, Error> {

// }

impl Label {
    pub fn read(val: &json::JsonValue) -> Result<Self, Error> {
        let str = match val {
            json::JsonValue::Short(s) => s.to_string(),
            json::JsonValue::String(s) => s.clone(),
            json::JsonValue::Number(n) => n.to_string(),
            _ => return Err(Error::ExpectedString),
        };

        Ok(Label { contents: str })
    }
}

fn read_bracket(val: &json::JsonValue) -> Result<(usize, bool), Error> {
    let index = val["index"].as_usize().ok_or(Error::ExpectedNumber)?;
    let is_open = val["is_open"].as_bool().ok_or(Error::ExpectedBool)?;

    Ok((index, is_open))
}

impl Item {
    pub fn read(val: &json::JsonValue) -> Result<Self, Error> {
        let mut item = Item::default();
        for (key, val) in val.entries() {
            match key {
                "bracket" => {
                    let (index, is_open) = read_bracket(val)?;
                    item.bracket = Some(Bracket::new(index, is_open));
                }
                "item" => item.item = Some(Label::read(val)?),
                _ => continue,
            }
        }
        Ok(item)
    }
}

fn read_edge(val: &json::JsonValue) -> Result<(Label, Item, Label), Error> {
    let source = Label::read(&val["source"])?;
    let target = Label::read(&val["target"])?;
    let item = Item::read(&val["item"])?;
    Ok((source, item, target))
}

pub fn read<B>(src: &str, builder: &mut B) -> Result<B::TargetGraph, Error>
where
    B: Builder<Label, Item>,
    B::TargetGraph: Graph<Label, Item>,
{
    builder.clear();
    let json = json::parse(src).map_err(|e| Error::Json(e.to_string()))?;

    let start_node = Label::read(&json["start_node"])?;
    let end_nodes: Vec<_> = json["end_nodes"]
        .members()
        .map(Label::read)
        .collect::<Result<_, _>>()?;
    json["edges"]
        .members()
        .map(read_edge)
        .try_for_each(|edge| {
            edge.map(|(source, edge, target)| builder.add_edge(source, edge, target))
        })?;

    Ok(builder.build(start_node, end_nodes))
}
