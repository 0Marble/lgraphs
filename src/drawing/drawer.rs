use std::{collections::HashSet, marker::PhantomData};

use crate::{
    graphs::{
        graph_trait::Graph,
        lgraph::{Bracket, BracketStack, Item},
        refs::{EdgeRef, NodeRef},
    },
    io::reading::Label,
};

use super::{
    geometry::{Circle, Curve, Rect, Vec2},
    layout::Layout,
};

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Rect(Rect),
    Circle(Circle),
    Curve(Curve),
    Text { bounds: Rect, text: String },
}

impl DrawCommand {
    fn priority(&self) -> usize {
        match self {
            DrawCommand::Rect(_) => 0,
            DrawCommand::Circle(_) => 0,
            DrawCommand::Curve(_) => 0,
            DrawCommand::Text { .. } => 1,
        }
    }
}

pub trait LabelDrawer {
    type Obj;
    fn draw(&self, label: Self::Obj) -> String;
}

pub struct LabelDrawerImpl<T> {
    _p: PhantomData<*const T>,
}
impl<T> LabelDrawerImpl<T> {
    pub fn new() -> Self {
        Self { _p: PhantomData }
    }
}

impl<T> Default for LabelDrawerImpl<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> LabelDrawer for LabelDrawerImpl<&'a char> {
    type Obj = &'a char;
    fn draw(&self, obj: &'a char) -> String {
        format!("{}", obj)
    }
}
impl<'a> LabelDrawer for LabelDrawerImpl<&'a i32> {
    type Obj = &'a i32;
    fn draw(&self, obj: &'a i32) -> String {
        obj.to_string()
    }
}
impl<'a> LabelDrawer for LabelDrawerImpl<&'a Label> {
    type Obj = &'a Label;
    fn draw(&self, obj: &'a Label) -> String {
        match obj {
            Label::Int(i) => i.to_string(),
            Label::Char(c) => format!("{}", c),
        }
    }
}
impl<'a> LabelDrawer for LabelDrawerImpl<&'a usize> {
    type Obj = &'a usize;
    fn draw(&self, obj: &'a usize) -> String {
        obj.to_string()
    }
}
impl<'a> LabelDrawer for LabelDrawerImpl<&'a BracketStack> {
    type Obj = &'a BracketStack;
    fn draw(&self, obj: &'a BracketStack) -> String {
        let mut s = String::new();
        for index in obj.brackets().map(|b| b.index()) {
            s = format!("{s}{}", index)
        }

        s
    }
}
impl<'a> LabelDrawer for LabelDrawerImpl<&'a Bracket> {
    type Obj = &'a Bracket;
    fn draw(&self, obj: &'a Bracket) -> String {
        format!("{}{}", if obj.is_open() { "[" } else { "]" }, obj.index())
    }
}

impl<'a, T> LabelDrawer for LabelDrawerImpl<&'a Option<T>>
where
    LabelDrawerImpl<&'a T>: LabelDrawer<Obj = &'a T>,
{
    type Obj = &'a Option<T>;
    fn draw(&self, obj: &'a Option<T>) -> String {
        match obj {
            Some(l) => LabelDrawerImpl::<&'a T>::new().draw(l),
            None => "_".to_string(),
        }
    }
}
impl<'a, T> LabelDrawer for LabelDrawerImpl<&'a Item<T>>
where
    LabelDrawerImpl<&'a T>: LabelDrawer<Obj = &'a T>,
{
    type Obj = &'a Item<T>;
    fn draw(&self, obj: &'a Item<T>) -> String {
        let item = match obj.item() {
            Some(item) => LabelDrawerImpl::<&'a T>::new().draw(item),
            None => "_".to_string(),
        };
        format!(
            "{item},{}",
            LabelDrawerImpl::<&'a Bracket>::new().draw(&obj.bracket())
        )
    }
}

pub fn draw_graph<'a, N, E, G, L>(
    layout: &L,
    text_size: f32,
    min_char_count: usize,
    max_char_count: usize,
    node_drawer: &impl LabelDrawer<Obj = &'a N>,
    edge_drawer: &impl LabelDrawer<Obj = &'a E>,
) -> Vec<DrawCommand>
where
    G: Graph<N, E>,
    L: Layout<'a, N, E, G>,
    N: 'a,
    E: 'a,
    G: 'a,
{
    let mut commands = vec![];
    for node in layout.graph().nodes() {
        draw_node(
            &mut commands,
            node,
            layout,
            text_size,
            min_char_count,
            max_char_count,
            node_drawer,
            edge_drawer,
        );
    }

    for source in layout.graph().nodes() {
        let targets: HashSet<_> = layout
            .graph()
            .edges_from(source)
            .map(|e| e.target())
            .collect();
        for target in targets {
            let edges: Vec<_> = layout.graph().edges_from_to(source, target).collect();
            let edge_count = edges.len();
            for (i, edge) in edges.into_iter().enumerate() {
                draw_edge(
                    &mut commands,
                    edge,
                    layout,
                    text_size,
                    min_char_count,
                    max_char_count,
                    node_drawer,
                    edge_drawer,
                    edge_count,
                    i,
                );
            }
        }
    }

    for end_node in layout.graph().end_nodes() {
        let Some(pos) = layout.node(end_node) else {continue;};
        commands.push(DrawCommand::Circle(Circle {
            x: pos.x,
            y: pos.y,
            r: pos.r * 0.8,
        }))
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
            node_drawer,
            edge_drawer,
        );
    }

    commands.sort_by_key(|a| a.priority());

    commands
}

#[allow(clippy::too_many_arguments)]
fn draw_node<'a, N, E: 'a, G, L>(
    commands: &mut Vec<DrawCommand>,
    node: NodeRef<'a, N>,
    layout: &L,
    text_size: f32,
    min_char_count: usize,
    max_char_count: usize,
    node_drawer: &impl LabelDrawer<Obj = &'a N>,
    _edge_drawer: &impl LabelDrawer<Obj = &'a E>,
) where
    G: Graph<N, E>,
    L: Layout<'a, N, E, G>,
{
    let Some(pos) = layout.node(node) else {return};
    let full_text = node_drawer.draw(node.contents());
    let allowed_char_count = full_text.len().clamp(min_char_count, max_char_count);
    let cropped_text: String = full_text.chars().take(allowed_char_count).collect();
    let bounds = pos.rect_at((cropped_text.len() as f32) * text_size, text_size, 0.0, 0.0);
    commands.push(DrawCommand::Circle(pos));
    commands.push(DrawCommand::Text {
        bounds,
        text: cropped_text,
    });
}

#[allow(clippy::too_many_arguments)]
fn draw_edge<'a, N, E, G, L>(
    commands: &mut Vec<DrawCommand>,
    edge: EdgeRef<'a, N, E>,
    layout: &L,
    text_size: f32,
    min_char_count: usize,
    max_char_count: usize,
    node_drawer: &impl LabelDrawer<Obj = &'a N>,
    edge_drawer: &impl LabelDrawer<Obj = &'a E>,

    edge_count: usize,
    cur_edge: usize,
) where
    G: Graph<N, E>,
    L: Layout<'a, N, E, G>,
{
    let Some(from) = layout.node(edge.source()) else {return;};

    let total_angle = match edge_count {
        1 => 0.0,
        2 => std::f32::consts::FRAC_PI_4,
        3 => std::f32::consts::FRAC_PI_3,
        _ => std::f32::consts::FRAC_PI_2,
    };
    let segment_size = total_angle / (edge_count as f32);
    let cur_segment =
        segment_size * (((cur_edge + 1) / 2) as f32) * (-1.0f32).powi(cur_edge as i32);

    let line = if edge.source() == edge.target() {
        let angle = std::f32::consts::FRAC_PI_8;
        let dir = Vec2::new(0.0, from.r).normalize();
        let left = dir.rotate(angle + cur_segment);
        let right = dir.rotate(-angle + cur_segment);

        let p1 = from.center() + left * from.r;
        let p4 = from.center() + right * from.r;
        let p2 = from.center() + left * (from.r + 0.5 * layout.spacing());
        let p3 = from.center() + right * (from.r + 0.5 * layout.spacing());
        Curve::from_points(p1, p2, p3, p4)
    } else {
        let Some(to) = layout.node(edge.target()) else {return};
        let dist = from.dist(to);
        let droopiness =
            (dist / Vec2::new(layout.width(), layout.height()).len()).powf(0.7) * layout.spacing();
        let dir = (to.center() - from.center()).normalize();
        let start = dir.rotate(cur_segment) * from.r + from.center();
        let end = (dir * -1.0).rotate(-cur_segment) * to.r + to.center();

        Curve::between(droopiness, start, end)
    };

    draw_arrow(
        commands,
        line.p1,
        line.p4,
        line.p2,
        line.p3,
        layout,
        node_drawer,
        edge_drawer,
    );
    let bounds = line.bounds();
    let allowed_char_count =
        ((bounds.width() / text_size) as usize).clamp(min_char_count, max_char_count);
    let text: String = edge_drawer
        .draw(edge.contents())
        .chars()
        .take(allowed_char_count)
        .collect();
    let bounds = line.rect_at((text.len() as f32) * text_size, text_size, 0.3);
    commands.push(DrawCommand::Text { bounds, text });
}

#[allow(clippy::too_many_arguments)]
fn draw_arrow<'a, N: 'a, E: 'a, G, L>(
    commands: &mut Vec<DrawCommand>,
    from: Vec2,
    to: Vec2,
    control1: Vec2,
    control2: Vec2,
    layout: &L,
    _node_drawer: &impl LabelDrawer<Obj = &'a N>,
    _edge_drawer: &impl LabelDrawer<Obj = &'a E>,
) where
    G: Graph<N, E>,
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
    let len = (dir.len() * 0.1).clamp(layout.spacing() * 0.1, layout.spacing() * 0.2);
    let left = (dir * -1.0).rotate(angle).normalize() * len;
    let right = (dir * -1.0).rotate(-angle).normalize() * len;
    commands.push(DrawCommand::Curve(Curve::straight(p4 + left, p4)));
    commands.push(DrawCommand::Curve(Curve::straight(p4 + right, p4)));
}
