use std::fmt::Display;

use crate::graphs::{
    graph_trait::{Builder, Graph},
    lgraph::{Bracket, Item as LGraphItem, LGraph},
};

pub mod drawing;
pub mod reading;
pub mod writing;

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct Label {
    contents: String,
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.contents())
    }
}

impl Label {
    pub fn new(contents: String) -> Self {
        Self { contents }
    }

    pub fn contents(&self) -> &str {
        self.contents.as_ref()
    }
}

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct Item {
    item: Option<Label>,
    bracket: Option<Bracket>,
}

impl Item {
    pub fn new(item: Option<Label>, bracket: Option<Bracket>) -> Self {
        Self { item, bracket }
    }

    pub fn item(&self) -> Option<&Label> {
        self.item.as_ref()
    }

    pub fn bracket(&self) -> Option<&Bracket> {
        self.bracket.as_ref()
    }
}

pub fn to_lgraph<B>(
    g: &impl Graph<Label, Item>,
    builder: &mut B,
) -> LGraph<Label, Label, B::TargetGraph>
where
    B: Builder<Label, LGraphItem<Label>>,
    B::TargetGraph: Graph<Label, LGraphItem<Label>>,
{
    todo!()
}
