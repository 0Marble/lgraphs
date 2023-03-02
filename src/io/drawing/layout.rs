use std::{collections::HashMap, iter::once, marker::PhantomData};

use crate::graphs::{
    graph_trait::Graph,
    refs::{NodeRef, Path},
};

use super::{djikstra::djikstra, geometry::*};

pub trait Layout<'a, N, E, G> {
    fn node(&self, node: NodeRef<'a, N>) -> Option<Circle>;
    fn graph(&self) -> &'a G;
    fn width(&self) -> f32;
    fn height(&self) -> f32;
    fn spacing(&self) -> f32;

    fn start(&self) -> Vec2;
}

impl<'a, L, N, E, G> Layout<'a, N, E, G> for Box<L>
where
    L: Layout<'a, N, E, G> + ?Sized,
{
    fn node(&self, node: NodeRef<'a, N>) -> Option<Circle> {
        self.as_ref().node(node)
    }

    fn graph(&self) -> &'a G {
        self.as_ref().graph()
    }

    fn width(&self) -> f32 {
        self.as_ref().width()
    }

    fn height(&self) -> f32 {
        self.as_ref().height()
    }

    fn spacing(&self) -> f32 {
        self.as_ref().spacing()
    }

    fn start(&self) -> Vec2 {
        self.as_ref().start()
    }
}

#[derive(Debug)]
pub struct ManualGridLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    width: f32,
    height: f32,
    node_radius: f32,
    spacing: f32,
    locations: HashMap<NodeRef<'a, N>, (usize, usize)>,
    graph: &'a G,
    _p: PhantomData<(N, E)>,
}

impl<'a, N, E, G> Layout<'a, N, E, G> for ManualGridLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    fn node(&self, node: NodeRef<'a, N>) -> Option<Circle> {
        let (x, y) = self.locations.get(&node)?;

        let x = (x + 1) as f32 * self.spacing + (2 * x + 1) as f32 * self.node_radius;
        let y = (y + 1) as f32 * self.spacing + (2 * y + 1) as f32 * self.node_radius;

        Some(Circle::new(x, y, self.node_radius))
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

    fn spacing(&self) -> f32 {
        self.spacing
    }

    fn start(&self) -> Vec2 {
        Vec2::new(0.0, self.height * 0.5)
    }
}

impl<'a, N, E, G> ManualGridLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    pub fn new(
        node_radius: f32,
        spacing: f32,
        graph: &'a G,
        mut locations: HashMap<NodeRef<'a, N>, (usize, usize)>,
    ) -> Self {
        let min_x = locations
            .iter()
            .map(|(_, (x, _))| *x)
            .min()
            .unwrap_or_default();
        let min_y = locations
            .iter()
            .map(|(_, (_, y))| *y)
            .min()
            .unwrap_or_default();

        locations.values_mut().for_each(|(x, y)| {
            *x -= min_x;
            *y -= min_y;
        });

        let horizontal_count = locations
            .values()
            .map(|(x, _)| x + 1)
            .max()
            .unwrap_or_default();
        let vertical_count = locations
            .values()
            .map(|(_, y)| y + 1)
            .max()
            .unwrap_or_default();

        let width =
            (horizontal_count + 1) as f32 * spacing + (2 * horizontal_count) as f32 * node_radius;
        let height =
            (vertical_count + 1) as f32 * spacing + (2 * vertical_count) as f32 * node_radius;

        Self {
            width,
            height,
            node_radius,
            spacing,
            locations,
            graph,
            _p: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct MinGridLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    inner: ManualGridLayout<'a, N, E, G>,
}

impl<'a, N, E, G> MinGridLayout<'a, N, E, G>
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
        let mut locations = HashMap::new();
        for (x, nodes) in grouped_distances {
            for (y, (_, node)) in nodes.into_iter().enumerate() {
                locations.insert(node, (x, y));
            }
        }

        Self {
            inner: ManualGridLayout::new(node_radius, spacing, graph, locations),
        }
    }
}

impl<'a, N, E, G> Layout<'a, N, E, G> for MinGridLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    fn node(&self, node: NodeRef<'a, N>) -> Option<Circle> {
        self.inner.node(node)
    }

    fn graph(&self) -> &'a G {
        self.inner.graph()
    }

    fn width(&self) -> f32 {
        self.inner.width()
    }

    fn spacing(&self) -> f32 {
        self.inner.spacing()
    }

    fn height(&self) -> f32 {
        self.inner.height()
    }

    fn start(&self) -> Vec2 {
        self.inner.start()
    }
}

pub struct MinIntersectionLayout<'a, N, E, G>
where
    G: Graph<N, E>,
{
    inner: ManualGridLayout<'a, N, E, G>,
}

impl<'a, N, E, G> MinIntersectionLayout<'a, N, E, G>
where
    G: Graph<N, E>,
    E: 'a,
    N: 'a,
{
    pub fn new(node_radius: f32, spacing: f32, graph: &'a G, max_step_count: usize) -> Self {
        let mut locations = Self::starting_locations(node_radius, spacing, graph);
        Self::minimize(node_radius, spacing, graph, &mut locations, max_step_count);
        let min_x = locations
            .iter()
            .map(|(_, (x, _))| *x)
            .min()
            .unwrap_or_default();
        let min_y = locations
            .iter()
            .map(|(_, (_, y))| *y)
            .min()
            .unwrap_or_default();

        Self {
            inner: ManualGridLayout::new(
                node_radius,
                spacing,
                graph,
                locations
                    .into_iter()
                    .map(|(node, (x, y))| (node, ((x + min_x) as usize, (y + min_y) as usize)))
                    .collect(),
            ),
        }
    }

    fn starting_locations(
        node_radius: f32,
        spacing: f32,
        graph: &'a G,
    ) -> HashMap<NodeRef<'a, N>, (i32, i32)> {
        MinGridLayout::new(node_radius, spacing, graph)
            .inner
            .locations
            .into_iter()
            .map(|(node, (x, y))| (node, (x as i32, y as i32)))
            .collect()
    }

    fn score(
        node_radius: f32,
        spacing: f32,
        graph: &'a G,
        locations: &HashMap<NodeRef<'a, N>, (i32, i32)>,
    ) -> f32 {
        1.0
    }

    fn minimize(
        node_radius: f32,
        spacing: f32,
        graph: &'a G,
        locations: &mut HashMap<NodeRef<'a, N>, (i32, i32)>,
        max_step_count: usize,
    ) {
        let mut min_score = Self::score(node_radius, spacing, graph, locations);
        let nodes: Vec<_> = locations.keys().cloned().collect();
        let mut coords: Vec<_> = nodes
            .iter()
            .flat_map(|node| locations.get(node))
            .cloned()
            .collect();

        for i in (0..coords.len()).cycle().take(max_step_count) {
            let (original_x, original_y) = coords[i];

            for (x, y) in [
                (original_x + 1, original_y),
                (original_x - 1, original_y),
                (original_x, original_y + 1),
                (original_x, original_y - 1),
            ] {
                if coords
                    .iter()
                    .enumerate()
                    .filter(|(j, _)| j != &i)
                    .any(|(_, (a, b))| a == &x && b == &y)
                {
                    continue;
                }

                locations.insert(nodes[i], (x, y));
                let score = Self::score(node_radius, spacing, graph, locations);
                if score < min_score {
                    min_score = score;
                    coords[i] = (x, y);
                    continue;
                } else {
                    locations.insert(nodes[i], (original_x, original_y));
                }
            }
        }
    }
}
