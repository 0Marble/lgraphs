use std::collections::HashSet;

use super::refs::{EdgeRef, NodeRef, Path};

pub trait Graph<N, E> {
    fn start_node(&self) -> NodeRef<'_, N>;
    fn is_end_node(&self, node: NodeRef<'_, N>) -> bool;
    fn nodes(&self) -> Box<dyn Iterator<Item = NodeRef<'_, N>> + '_>;
    fn edges(&self) -> Box<dyn Iterator<Item = EdgeRef<'_, N, E>> + '_>;

    fn end_nodes(&self) -> Box<dyn Iterator<Item = NodeRef<'_, N>> + '_> {
        Box::new(self.nodes().filter(|node| self.is_end_node(*node)))
    }
    fn node_with_contents<'a, 'b>(&'a self, contents: &'b N) -> Option<NodeRef<'a, N>>
    where
        'a: 'b,
        N: Eq,
    {
        self.nodes().find(|node| node.contents() == contents)
    }
    fn edge_with_contents<'a, 'b>(&'a self, contents: &'b E) -> Option<EdgeRef<'a, N, E>>
    where
        'a: 'b,
        E: Eq,
    {
        self.edges().find(|edge| edge.contents() == contents)
    }
    fn edges_from<'a>(
        &'a self,
        node: NodeRef<'a, N>,
    ) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a> {
        Box::new(
            self.edges()
                .map(|e| {
                    let _ = 1 + 2;
                    e
                })
                .filter(move |edge| node.eq(&edge.source())),
        )
    }
    fn edges_to<'a>(
        &'a self,
        node: NodeRef<'a, N>,
    ) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a> {
        Box::new(self.edges().filter(move |edge| node.eq(&edge.target())))
    }
    fn edges_from_to<'a>(
        &'a self,
        from: NodeRef<'a, N>,
        to: NodeRef<'a, N>,
    ) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'a> {
        Box::new(
            self.edges()
                .filter(move |edge| from.eq(&edge.source()) && to.eq(&edge.target())),
        )
    }
    fn edges_from_with<'a, 'b>(
        &'a self,
        node: NodeRef<'a, N>,
        contents: &'b E,
    ) -> Box<dyn Iterator<Item = EdgeRef<'a, N, E>> + 'b>
    where
        'a: 'b,
        E: Eq,
    {
        Box::new(self.edges_from(node).filter(|e| contents.eq(e.contents())))
    }

    fn rebuild_to_node_nums<'a, B>(&'a self, builder: &mut B) -> B::TargetGraph
    where
        B: Builder<usize, E>,
        N: 'a,
        E: Clone,
    {
        builder.clear();
        for edge in self.edges() {
            builder.add_edge(
                edge.source_index(),
                edge.contents().clone(),
                edge.target_index(),
            );
        }
        for node in self.nodes() {
            builder.add_node(node.index());
        }
        builder.build(
            self.start_node().index(),
            self.end_nodes().map(|n| n.index()),
        )
    }

    fn node_count(&self) -> usize {
        self.nodes().count()
    }
}

pub trait WithReachableFrom<N, E>: Graph<N, E> {
    fn reachable_from<'a>(
        &'a self,
        node: NodeRef<'a, N>,
    ) -> Box<dyn Iterator<Item = (Path<'a, N, E>, NodeRef<'a, N>)> + 'a>
    where
        Self: Sized,
    {
        Box::new(ReachableFrom {
            graph: self,
            reached: HashSet::new(),
            stack: vec![(vec![], node)],
        })
    }
    fn reachable<'a>(&'a self) -> Box<dyn Iterator<Item = (Path<'a, N, E>, NodeRef<'a, N>)> + 'a>
    where
        Self: Sized,
    {
        self.reachable_from(self.start_node())
    }
}

struct ReachableFrom<'a, G, N, E>
where
    G: Graph<N, E>,
{
    graph: &'a G,
    reached: HashSet<NodeRef<'a, N>>,
    stack: Vec<(Vec<EdgeRef<'a, N, E>>, NodeRef<'a, N>)>,
}

impl<'a, G, N, E> Iterator for ReachableFrom<'a, G, N, E>
where
    G: Graph<N, E>,
{
    type Item = (Path<'a, N, E>, NodeRef<'a, N>);

    fn next(&mut self) -> Option<Self::Item> {
        let (path, node) = self.stack.pop()?;

        for edge in self.graph.edges_from(node) {
            if !self.reached.contains(&edge.target()) {
                let mut new_path = path.clone();
                new_path.push(edge);
                self.stack.push((new_path, edge.target()));
            }
        }

        self.reached.insert(node);
        Some((Path::new(path), node))
    }
}

pub trait Builder<N, E> {
    type TargetGraph: Graph<N, E>;

    fn add_edge(&mut self, source: N, edge: E, target: N);
    fn add_node(&mut self, node: N);

    fn clear(&mut self);
    fn build(&mut self, start_node: N, end_nodes: impl IntoIterator<Item = N>)
        -> Self::TargetGraph;
}
