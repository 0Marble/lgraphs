use imageproc::{
    drawing::{
        draw_cubic_bezier_curve_mut, draw_hollow_circle_mut, draw_hollow_rect_mut,
        draw_line_segment_mut, draw_text_mut,
    },
    rect::Rect,
};
use lgraphs::{
    drawing::{
        drawer::{draw_graph, DrawCommand},
        layout::{DefaultLayout, Layout},
    },
    graphs::{
        default::{DefaultBuilder, DefaultGraph},
        graph_trait::{Builder, Graph},
        state_machine::StateMachine,
    },
};

use image::{Rgb, RgbImage};
use rusttype::{Font, Scale};

fn get_graph() -> StateMachine<char, i32, DefaultGraph<char, i32>> {
    let edges = [
        ('a', 0, 'b'),
        ('a', 1, 'd'),
        ('b', 0, 'b'),
        ('b', 1, 'c'),
        ('c', 0, 'd'),
        ('d', 0, 'd'),
        ('d', 1, 'e'),
        ('e', 0, 'b'),
        ('e', 1, 'c'),
        ('c', 1, 'e'),
        ('f', 0, 'c'),
        ('f', 1, 'g'),
        ('g', 0, 'f'),
        ('g', 1, 'e'),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }

    StateMachine::new(builder.build('a', ['c', 'e']))
}

fn main() {
    let node_radius = 50.0;
    let spacing = 60.0;
    let text_scale = 20.0;

    let graph = get_graph();
    let layout = DefaultLayout::new(node_radius, spacing, &graph);
    let commands = draw_graph(&layout, text_scale);

    let mut image = RgbImage::from_pixel(
        layout.width().ceil() as u32,
        layout.height().ceil() as u32,
        Rgb([255, 255, 255]),
    );
    let font = Vec::from(include_bytes!("../../fonts/gnu-free/FreeMonoBoldOblique.otf") as &[u8]);
    let font = Font::try_from_vec(font).expect("Could not load font");

    for command in commands {
        match command {
            DrawCommand::Rect(rect) => draw_hollow_rect_mut(
                &mut image,
                Rect::at(rect.x1 as i32, rect.y1 as i32)
                    .of_size(rect.width() as u32, rect.height() as u32),
                Rgb([0, 0, 0]),
            ),
            DrawCommand::Circle(circ) => draw_hollow_circle_mut(
                &mut image,
                (circ.center().x as i32, circ.center().y as i32),
                circ.r as i32,
                Rgb([0, 0, 0]),
            ),
            DrawCommand::Line(line) => draw_line_segment_mut(
                &mut image,
                (line.start().x, line.start().y),
                (line.end().x, line.end().y),
                Rgb([0, 0, 0]),
            ),
            DrawCommand::Text { bounds, text } => draw_text_mut(
                &mut image,
                Rgb([0, 0, 0]),
                bounds.x1 as i32,
                bounds.y1 as i32,
                Scale::uniform(text_scale),
                &font,
                text.as_str(),
            ),
            DrawCommand::Curve(curve) => draw_cubic_bezier_curve_mut(
                &mut image,
                curve.p1.as_tuple(),
                curve.p4.as_tuple(),
                curve.p2.as_tuple(),
                curve.p3.as_tuple(),
                Rgb([0, 0, 0]),
            ),
        }
    }

    image.save("images/g1.png").expect("Could not save image");
}
