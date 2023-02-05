use std::fmt::Display;

use crate::graphs::{
    graph_trait::Graph,
    refs::{EdgeRef, NodeRef},
};

use super::{
    geometry::{Circle, Line, Rect, Vec2},
    layout::Layout,
};

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Rect(Rect),
    Circle(Circle),
    Line(Line),
    Text { bounds: Rect, text: String },
}

pub fn draw_graph<'a, N, E, G, L>(layout: &L, text_size: f32) -> Vec<DrawCommand>
where
    G: Graph<N, E>,
    N: Display,
    E: Display,
    L: Layout<'a, N, E, G>,
    N: 'a,
    E: 'a,
    G: 'a,
{
    let mut commands = vec![];
    for node in layout.graph().nodes() {
        draw_node(&mut commands, node, layout, text_size);
    }
    for edge in layout.graph().edges() {
        draw_edge(&mut commands, edge, layout, text_size);
    }
    for end_node in layout.graph().end_nodes() {
        let pos = layout.node(end_node);
        let line = pos.to_point(layout.end());
        draw_arrow(&mut commands, line.start(), line.end(), layout);
    }

    let pos = layout.node(layout.graph().start_node());
    let line = pos.to_point(layout.start());
    draw_arrow(&mut commands, line.start(), line.end(), layout);

    commands
}

fn draw_arrow<'a, N, E, G, L>(commands: &mut Vec<DrawCommand>, from: Vec2, to: Vec2, layout: &L)
where
    G: Graph<N, E>,
    N: Display,
    E: Display,
    L: Layout<'a, N, E, G>,
{
    commands.push(DrawCommand::Line(Line::new(from.x, from.y, to.x, to.y)));
    let dir = to - from;
    let angle = std::f32::consts::FRAC_PI_6;
    let diag = Vec2::new(layout.width(), layout.height()).len();
    let len = (dir.len() * 0.1).clamp(diag * 0.01, diag * 0.2);
    let a = len * angle.sin();
    let b = len * angle.cos();

    let c = from + dir.normalize() * (dir.len() - a);
    let normal = dir.normal().normalize() * b;

    commands.push(DrawCommand::Line(Line::new(
        (c + normal).x,
        (c + normal).y,
        to.x,
        to.y,
    )));
    commands.push(DrawCommand::Line(Line::new(
        (c - normal).x,
        (c - normal).y,
        to.x,
        to.y,
    )));
}

fn draw_node<'a, N, E, G, L>(
    commands: &mut Vec<DrawCommand>,
    node: NodeRef<'a, N>,
    layout: &L,
    text_size: f32,
) where
    G: Graph<N, E>,
    N: Display,
    E: Display,
    L: Layout<'a, N, E, G>,
{
    let pos = layout.node(node);
    let bounds = pos.bounds();
    let char_count = ((bounds.width() / text_size) as usize).clamp(1, 5);
    let text: String = node
        .contents()
        .to_string()
        .chars()
        .take(char_count)
        .collect();
    let bounds = pos.center_rect((char_count as f32) * text_size, text_size);
    commands.push(DrawCommand::Circle(pos));
    commands.push(DrawCommand::Text { bounds, text });
}

fn draw_edge<'a, N, E, G, L>(
    commands: &mut Vec<DrawCommand>,
    edge: EdgeRef<'a, N, E>,
    layout: &L,
    text_size: f32,
) where
    G: Graph<N, E>,
    N: Display,
    E: Display,
    L: Layout<'a, N, E, G>,
{
    let line = layout
        .node(edge.source())
        .line_between(layout.node(edge.target()));
    draw_arrow(commands, line.start(), line.end(), layout);
    let bounds = line.bounds();
    let char_count = ((bounds.width() / text_size) as usize).clamp(1, 5);
    let text: String = edge
        .contents()
        .to_string()
        .chars()
        .take(char_count)
        .collect();
    let bounds = line.center_rect((char_count as f32) * text_size, text_size);
    commands.push(DrawCommand::Text { bounds, text });
}
