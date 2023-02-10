use std::marker::PhantomData;

use crate::graphs::{
    graph_trait::Graph,
    lgraph::{Item, Mangled, Memory},
    refs::{EdgeRef, NodeRef},
};

pub trait ToJson {
    type Obj;
    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>>;
}

pub fn as_json<'a, N: 'a, E: 'a>(
    graph: &'a impl Graph<N, E>,
) -> Result<String, Box<dyn std::error::Error>>
where
    ToJsonImpl<NodeRef<'a, N>>: ToJson<Obj = NodeRef<'a, N>>,
    ToJsonImpl<EdgeRef<'a, N, E>>: ToJson<Obj = EdgeRef<'a, N, E>>,
{
    use std::fmt::Write;
    let node_writer = ToJsonImpl::<NodeRef<'a, N>>::new();
    let edge_writer = ToJsonImpl::<EdgeRef<'a, N, E>>::new();

    let mut s = String::new();
    let start_node = node_writer.to_json(graph.start_node())?;

    let mut end_nodes = String::new();
    if let Some(first) = graph.end_nodes().next() {
        write!(end_nodes, "{}", node_writer.to_json(first)?)?;

        for node in graph.end_nodes().skip(1) {
            write!(end_nodes, ",{}", node_writer.to_json(node)?)?;
        }
    }

    let mut edges = String::new();
    if let Some(first) = graph.edges().next() {
        write!(edges, "{}", edge_writer.to_json(first)?)?;

        for edge in graph.edges().skip(1) {
            write!(edges, ",{}", edge_writer.to_json(edge)?)?;
        }
    }

    write!(
        s,
        "{{\"start_node\":{},\"end_nodes\":[{}],\"edges\":[{}]}}",
        start_node, end_nodes, edges
    )?;

    Ok(s)
}

pub struct ToJsonImpl<T> {
    _p: PhantomData<*const T>,
}

impl<T> ToJsonImpl<T> {
    pub fn new() -> Self {
        Self { _p: PhantomData }
    }
}

impl<T> Default for ToJsonImpl<T> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> ToJson for ToJsonImpl<&'a i32> {
    type Obj = &'a i32;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", obj))
    }
}
impl<'a> ToJson for ToJsonImpl<&'a char> {
    type Obj = &'a char;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("\"{}\"", obj))
    }
}
impl<'a> ToJson for ToJsonImpl<&'a usize> {
    type Obj = &'a usize;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", obj))
    }
}
impl ToJson for ToJsonImpl<bool> {
    type Obj = bool;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", obj))
    }
}

impl<'a> ToJson for ToJsonImpl<&'a Mangled<Memory<i32>, usize>>
where
    ToJsonImpl<&'a i32>: ToJson<Obj = &'a i32>,
{
    type Obj = &'a Mangled<Memory<i32>, usize>;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        match obj {
            Mangled::Node(mem) => {
                let node = ToJsonImpl::<&'a i32>::new().to_json(mem.node())?;
                if mem.stack().is_empty() {
                    return Ok(node);
                }

                let mut s = String::new();
                for bracket in mem.stack().brackets() {
                    s = format!("{s}{}", bracket.index());
                }
                Ok(format!("{node},[{s}"))
            }
            Mangled::Mangled(i) => ToJsonImpl::<&'a usize>::new().to_json(i),
        }
    }
}
impl<'a, T> ToJson for ToJsonImpl<Option<&'a T>>
where
    ToJsonImpl<&'a T>: ToJson<Obj = &'a T>,
{
    type Obj = Option<&'a T>;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        match obj {
            Some(obj) => ToJsonImpl::<&'a T>::new().to_json(obj),
            None => Ok("null".to_string()),
        }
    }
}
impl<'a, N> ToJson for ToJsonImpl<NodeRef<'a, N>>
where
    ToJsonImpl<&'a N>: ToJson<Obj = &'a N>,
{
    type Obj = NodeRef<'a, N>;

    fn to_json(&self, obj: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        ToJsonImpl::<&'a N>::new().to_json(obj.contents())
    }
}
impl<'a, N, E> ToJson for ToJsonImpl<EdgeRef<'a, N, E>>
where
    ToJsonImpl<&'a N>: ToJson<Obj = &'a N>,
    ToJsonImpl<&'a E>: ToJson<Obj = &'a E>,
{
    type Obj = EdgeRef<'a, N, E>;

    fn to_json(&self, edge: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!(
            "\"source\":{},\"target:\"{},\"item\":{}",
            ToJsonImpl::<&'a N>::new().to_json(edge.source().contents())?,
            ToJsonImpl::<&'a N>::new().to_json(edge.target().contents())?,
            ToJsonImpl::<&'a E>::new().to_json(edge.contents())?
        ))
    }
}
impl<'a, N, E> ToJson for ToJsonImpl<EdgeRef<'a, N, Item<E>>>
where
    ToJsonImpl<&'a N>: ToJson<Obj = &'a N>,
    ToJsonImpl<&'a E>: ToJson<Obj = &'a E>,
{
    type Obj = EdgeRef<'a, N, Item<E>>;

    fn to_json(&self, edge: Self::Obj) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!(
            "\"source\":{},\"target:\"{},\"item\":{},\"bracket\":{{\"index\":{},\"is_open\":{}}}",
            ToJsonImpl::<&'a N>::new().to_json(edge.source().contents())?,
            ToJsonImpl::<&'a N>::new().to_json(edge.target().contents())?,
            ToJsonImpl::<Option<&'a E>>::new().to_json(edge.contents().item().as_ref())?,
            ToJsonImpl::<&usize>::new().to_json(&edge.contents().bracket().index())?,
            ToJsonImpl::<bool>::new().to_json(edge.contents().bracket().is_open())?,
        ))
    }
}
