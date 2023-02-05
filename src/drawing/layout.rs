use std::{collections::HashMap, marker::PhantomData};

use crate::graphs::{
    graph_trait::Graph,
    refs::{NodeRef, Path},
};

use super::{djikstra::djikstra, geometry::*};

pub trait Layout<'a, N, E, G> {
    fn node(&self, node: NodeRef<'a, N>) -> Circle;
    fn graph(&self) -> &'a G;
    fn width(&self) -> f32;
    fn height(&self) -> f32;

    fn start(&self) -> Vec2;
    fn end(&self) -> Vec2;
}

pub struct DefaultLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    width: f32,
    height: f32,
    node_radius: f32,
    spacing: f32,
    distances: HashMap<usize, Vec<NodeRef<'a, N>>>,
    graph: &'a G,
    _p: PhantomData<(N, E)>,
}

impl<'a, N, E, G> DefaultLayout<'a, N, E, G>
where
    G: Graph<N, E>,
    N: 'a,
    E: 'a,
{
    pub fn new(node_radius: f32, spacing: f32, graph: &'a G) -> Self {
        let distances = djikstra(graph);
        let mut grouped_distances = HashMap::new();
        for (path, dist, node) in distances {
            let entry: &mut Vec<(Path<N, E>, NodeRef<N>)> =
                grouped_distances.entry(dist).or_default();
            entry.push((path, node));
        }

        let horizontal_count = grouped_distances.keys().cloned().max().unwrap_or_default();
        let vertical_count = grouped_distances
            .iter()
            .max_by_key(|(_, nodes)| nodes.len())
            .map(|(_, nodes)| nodes.len())
            .unwrap_or_default();

        let width = (horizontal_count + 1) as f32 * spacing + horizontal_count as f32 * node_radius;
        let height = (vertical_count + 1) as f32 * spacing + vertical_count as f32 * node_radius;

        Self {
            width,
            height,
            spacing,
            distances: grouped_distances
                .into_iter()
                .map(|(dist, nodes)| (dist, nodes.into_iter().map(|(_, node)| node).collect()))
                .collect(),
            graph,
            node_radius,
            _p: PhantomData,
        }
    }
}

impl<'a, N, E, G> Layout<'a, N, E, G> for DefaultLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    fn node(&self, node: NodeRef<'a, N>) -> Circle {
        let (x, y) = if let Some(d) = self.distances.iter().find_map(|(dist, nodes)| {
            nodes
                .iter()
                .enumerate()
                .find(|(_, n)| n == &&node)
                .map(|(i, _)| (*dist, i))
        }) {
            d
        } else {
            return Circle::new(0.0, 0.0, 0.0);
        };

        let x = (x + 1) as f32 * self.spacing + x as f32 * self.node_radius;
        let y = (y + 1) as f32 * self.spacing + y as f32 * self.node_radius;

        Circle::new(x, y, self.node_radius)
    }

    fn graph(&self) -> &'a G {
        self.graph
    }

    fn width(&self) -> f32 {
        self.width
    }

    fn height(&self) -> f32 {
        self.height
    }

    fn start(&self) -> Vec2 {
        Vec2::new(0.0, self.height * 0.5)
    }

    fn end(&self) -> Vec2 {
        Vec2::new(self.width, self.height * 0.5)
    }
}
