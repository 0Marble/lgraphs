use std::fmt::Display;

use crate::graphs::{
    graph_trait::Graph,
    refs::{EdgeRef, NodeRef},
};

use super::{
    geometry::{Circle, Curve, Line, Rect, Vec2},
    layout::Layout,
};

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Rect(Rect),
    Circle(Circle),
    Line(Line),
    Curve(Curve),
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
        let Some(pos) = layout.node(end_node) else {continue;};
        let line = pos.to_point(layout.end());
        draw_arrow(
            &mut commands,
            line.start(),
            line.end(),
            line.t(0.25),
            line.t(0.75),
            layout,
        );
    }

    if let Some(pos) = layout.node(layout.graph().start_node()) {
        let line = pos.to_point(layout.start()).reverse();
        draw_arrow(
            &mut commands,
            line.start(),
            line.end(),
            line.t(0.25),
            line.t(0.75),
            layout,
        );
    }

    commands
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
    let Some(pos) = layout.node(node) else {return};
    let bounds = pos.bounds();
    let allowed_char_count = ((bounds.width() / text_size) as usize).clamp(1, 5);
    let text: String = node
        .contents()
        .to_string()
        .chars()
        .take(allowed_char_count)
        .collect();
    let bounds = pos.rect_at((text.len() as f32) * text_size, text_size, 0.0, 0.0);
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
    if edge.source() == edge.target() {
        draw_loop(commands, edge, layout, text_size);
        return;
    }

    let Some(from) = layout.node(edge.source()) else {return};
    let Some(to) = layout.node(edge.target()) else {return};
    let line = from.line_between(to);

    draw_arrow(
        commands,
        line.start(),
        line.end(),
        line.t(0.25),
        line.t(0.75),
        layout,
    );
    let bounds = line.bounds();
    let allowed_char_count = ((bounds.width() / text_size) as usize).clamp(1, 5);
    let text: String = edge
        .contents()
        .to_string()
        .chars()
        .take(allowed_char_count)
        .collect();
    let bounds = line.rect_at((text.len() as f32) * text_size, text_size, 0.3);
    commands.push(DrawCommand::Text { bounds, text });
}

fn draw_loop<'a, N, E, G, L>(
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
    let Some(pos) = layout.node(edge.source()) else {return;};

    let angle = std::f32::consts::FRAC_PI_8;
    let dir = Vec2::new(0.0, pos.r).normalize();
    let left = dir.rotate(angle);
    let right = dir.rotate(-angle);

    let p1 = pos.center() + left * pos.r;
    let p4 = pos.center() + right * pos.r;
    let p2 = pos.center() + left * (pos.r + 0.5 * layout.spacing());
    let p3 = pos.center() + right * (pos.r + 0.5 * layout.spacing());

    draw_arrow(commands, p1, p4, p2, p3, layout);
    let curve = Curve::from_points(p1, p2, p3, p4);

    let bounds = curve.bounds();
    let allowed_char_count = ((bounds.width() / text_size) as usize).clamp(1, 5);
    let text: String = edge
        .contents()
        .to_string()
        .chars()
        .take(allowed_char_count)
        .collect();
    let bounds = curve.rect_at((text.len() as f32) * text_size, text_size, 0.3);
    commands.push(DrawCommand::Text { bounds, text });
}

fn draw_arrow<'a, N, E, G, L>(
    commands: &mut Vec<DrawCommand>,
    from: Vec2,
    to: Vec2,
    control1: Vec2,
    control2: Vec2,
    layout: &L,
) where
    G: Graph<N, E>,
    N: Display,
    E: Display,
    L: Layout<'a, N, E, G>,
{
    let p1 = from;
    let p2 = control1;
    let p3 = control2;
    let p4 = to;

    let curve = Curve::from_points(p1, p2, p3, p4);
    commands.push(DrawCommand::Curve(curve));

    let dir = ((p4 - p3) * 3.0).normalize();
    let angle = std::f32::consts::FRAC_PI_6;
    let diag = Vec2::new(layout.width(), layout.height()).len();
    let len = (dir.len() * 0.1).clamp(diag * 0.01, diag * 0.2);
    let left = (dir * -1.0).rotate(angle).normalize() * len;
    let right = (dir * -1.0).rotate(-angle).normalize() * len;
    commands.push(DrawCommand::Line(Line::new(
        (p4 + left).x,
        (p4 + left).y,
        p4.x,
        p4.y,
    )));
    commands.push(DrawCommand::Line(Line::new(
        (p4 + right).x,
        (p4 + right).y,
        p4.x,
        p4.y,
    )));
}
