use crate::graphs::{
    graph_trait::Graph,
    lgraph::{Item, Mangled, Memory},
    refs::EdgeRef,
};

use super::{Item as JsonItem, Label};

pub trait ToJson {
    fn to_json(&self) -> String;
}

impl ToJson for Label {
    fn to_json(&self) -> String {
        format!("\"{}\"", self.contents())
    }
}

impl ToJson for JsonItem {
    fn to_json(&self) -> String {
        let mut s = "\"item\":{".to_string();
        if let Some(item) = self.item() {
            s.push_str(&format!("\"item\":{}", item.to_json()));
        }
        if let Some(bracket) = self.bracket() {
            s.push_str(&format!(
                "\"bracket\":{{\"index\":{},\"is_open\":{}}}",
                bracket.index(),
                bracket.is_open()
            ));
        }

        s.push('}');
        s
    }
}

impl ToJson for i32 {
    fn to_json(&self) -> String {
        self.to_string()
    }
}

impl ToJson for usize {
    fn to_json(&self) -> String {
        self.to_string()
    }
}

impl ToJson for char {
    fn to_json(&self) -> String {
        format!("\"{}\"", self)
    }
}

impl<E> ToJson for Item<E>
where
    E: ToString,
{
    fn to_json(&self) -> String {
        let bracket = self.bracket();
        JsonItem::new(
            self.item().as_ref().map(|i| Label::new(i.to_string())),
            Some(bracket),
        )
        .to_json()
    }
}

impl<N: ToString> ToJson for Memory<N> {
    fn to_json(&self) -> String {
        let mut s = "\"".to_string();
        s.push_str(&self.node().to_string());
        s.push(',');
        for bracket in self.stack().brackets() {
            s.push_str(&bracket.index().to_string());
        }

        s.push('\"');
        s
    }
}

impl<N: ToString> ToJson for Mangled<N, usize> {
    fn to_json(&self) -> String {
        match self {
            Mangled::Node(n) => format!("\"{}\"", n.to_string()),
            Mangled::Mangled(i) => format!("\"mangled{i}\""),
        }
    }
}

fn write_edge<N: ToJson, E: ToJson>(edge: &EdgeRef<N, E>) -> String {
    format!(
        "{{\"source\":{},\"target\":{},{}}}",
        edge.source().contents().to_json(),
        edge.target().contents().to_json(),
        edge.contents().to_json()
    )
}

pub fn write<N: ToJson, E: ToJson>(g: &impl Graph<N, E>) -> String {
    let mut s = "{".to_string();

    s.push_str(&format!(
        "\"start_node\":{},",
        g.start_node().contents().to_json()
    ));

    let end_nodes: Vec<_> = g.end_nodes().collect();
    s.push_str("\"end_nodes\":[");
    s.push_str(&end_nodes[0].contents().to_json());
    for node in &end_nodes[1..] {
        s.push_str(&format!(",{}", node.contents().to_json()));
    }
    s.push_str("],");

    let edges: Vec<_> = g.edges().collect();
    s.push_str("\"edges\":[");
    s.push_str(&write_edge(&edges[0]));
    for edge in &edges[1..] {
        s.push_str(&format!(",{}", write_edge(edge)));
    }

    s.push_str("]}\n");
    s
}
