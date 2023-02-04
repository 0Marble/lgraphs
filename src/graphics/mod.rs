use std::{collections::BTreeMap, fmt::Display, marker::PhantomData};

pub mod genometry;

use crate::graphs::{
    graph_trait::{EdgeRef, Graph, NodeIndex, NodeRef},
    lgraph::Item,
};

use self::genometry::{Rect, RotatedRect, Vec2};

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Node { clip: Rect },
    Line { from: Vec2, to: Vec2 },
    Text { text: String, clip: RotatedRect },
}

impl<E> Display for Item<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.item() {
            Some(item) => write!(
                f,
                "{}, {}{}",
                item,
                if self.bracket().is_opening() {
                    '['
                } else {
                    ']'
                },
                self.bracket().index()
            ),
            None => write!(
                f,
                "{}{}",
                if self.bracket().is_opening() {
                    '['
                } else {
                    ']'
                },
                self.bracket().index()
            ),
        }
    }
}

pub struct GraphDrawer<'a, N, E, G> {
    spacing: f32,
    text_height: f32,

    _p: PhantomData<&'a (N, E, G)>,
}

impl<'a, N, E, G> GraphDrawer<'a, N, E, G>
where
    G: Graph<N, E>,
    N: ToString,
    E: ToString,
{
    pub fn new(spacing: f32, text_height: f32) -> Self {
        Self {
            spacing,
            text_height,
            _p: PhantomData,
        }
    }

    pub fn draw(
        &self,
        graph: &'a G,
        start_node: NodeIndex,
        end_nodes: impl Iterator<Item = NodeIndex>,
        clip: Rect,
    ) -> Vec<DrawCommand> {
        let mut node_distances: BTreeMap<usize, Vec<_>> = BTreeMap::new();

        for (path, node) in graph.reachable_from(start_node) {
            let nodes_at_distance = node_distances.entry(path.len()).or_default();
            nodes_at_distance.push(node);
        }

        let Some(vertical_count) = node_distances.values().map(|l| l.len()).max() else {
            return vec![]
        };
        let horizontal_count = node_distances.len();

        let total_spacing = (
            horizontal_count as f32 * self.spacing,
            vertical_count as f32 * self.spacing,
        );

        if total_spacing.0 > clip.width() || total_spacing.1 > clip.height() {
            return vec![];
        }

        let node_size = (
            (clip.width() - total_spacing.0) / (horizontal_count as f32),
            (clip.height() - total_spacing.1) / (vertical_count as f32),
        );

        let node_positions: BTreeMap<_, _> = node_distances
            .into_iter()
            .flat_map(|(dist, nodes)| {
                nodes
                    .into_iter()
                    .enumerate()
                    .map(move |(i, node)| (dist, i, node))
            })
            .map(|(x, y, node)| {
                let pos = Vec2::new(
                    x as f32 * node_size.0 + (x + 1) as f32 * self.spacing,
                    y as f32 * node_size.1 + (y + 1) as f32 * self.spacing,
                );

                (
                    node.index(),
                    Rect::new(pos.x, pos.y, pos.x + node_size.0, pos.y + node_size.1),
                )
            })
            .collect();

        let mut commands = vec![];

        for edge in graph.edges() {
            let mut source = self.node(edge.source(), &node_positions);
            let mut target = self.node(edge.target(), &node_positions);
            let mut edge = self.edge(edge, &node_positions);

            commands.append(&mut source);
            commands.append(&mut target);
            commands.append(&mut edge);
        }

        let mut in_arrow = self.arrow(
            clip.uv(0.0, 0.5),
            node_positions.get(&start_node).unwrap().uv(0.0, 0.5),
        );
        let mut out_arrows = vec![];
        for end_node in end_nodes {
            let Some(rect) = node_positions.get(&end_node) else{continue;};
            let mut arrow = self.arrow(rect.uv(1.0, 0.5), clip.uv(1.0, 0.5));
            out_arrows.append(&mut arrow);
        }

        commands.append(&mut in_arrow);
        commands.append(&mut out_arrows);

        commands
    }

    fn arrow(&self, from: Vec2, to: Vec2) -> Vec<DrawCommand> {
        vec![DrawCommand::Line { from, to }]
    }

    fn edge(
        &self,
        edge: EdgeRef<'a, N, E>,
        node_positions: &BTreeMap<NodeIndex, Rect>,
    ) -> Vec<DrawCommand> {
        let Some(source_rect) = node_positions.get(&edge.source_index()) else {
            return vec![];
        };
        let Some(target_rect) = node_positions.get(&edge.target_index()) else {
            return vec![];
        };

        let p1 = source_rect.uv(1.0, 0.5);
        let p2 = target_rect.uv(0.0, 0.5);
        let mut arrow = self.arrow(p1, p2);

        let dir = p2 - p1;

        let mut commands = vec![DrawCommand::Text {
            text: edge.contents().to_string(),
            clip: RotatedRect::new(
                dir.t(0.5) + p1,
                (dir.len(), self.text_height),
                dir.normalize(),
            ),
        }];
        commands.append(&mut arrow);
        commands
    }

    fn node(
        &self,
        node: NodeRef<'a, N>,
        node_positions: &BTreeMap<NodeIndex, Rect>,
    ) -> Vec<DrawCommand> {
        let Some(clip) = node_positions.get(&node.index()) else {
            return vec![];
        };
        vec![
            DrawCommand::Node { clip: *clip },
            DrawCommand::Text {
                text: node.contents().to_string(),
                clip: clip.into(),
            },
        ]
    }
}
