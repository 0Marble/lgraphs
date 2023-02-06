use imageproc::{
    drawing::{
        draw_cubic_bezier_curve_mut, draw_filled_circle_mut, draw_hollow_circle_mut,
        draw_hollow_rect_mut, draw_line_segment_mut, draw_text_mut,
    },
    rect::Rect,
};
use lgraphs::{
    drawing::{
        drawer::{draw_graph, DrawCommand},
        layout::{Layout, MinGridLayout},
    },
    graphs::{
        default::DefaultBuilder,
        graph_trait::{Builder, Graph},
        lgraph::{Bracket, BracketType, Item, LGraph},
    },
};

use image::{Rgb, RgbImage};
use rusttype::{Font, Scale};

fn get_graph() -> impl Graph<i32, Item<char>> {
    let edges = [
        (1, Item::new(None, Bracket::new(1, BracketType::Open)), 2),
        (
            2,
            Item::new(Some('a'), Bracket::new(2, BracketType::Open)),
            2,
        ),
        (
            2,
            Item::new(Some('b'), Bracket::new(3, BracketType::Open)),
            3,
        ),
        (3, Item::new(None, Bracket::new(3, BracketType::Close)), 4),
        (
            4,
            Item::new(Some('a'), Bracket::new(2, BracketType::Close)),
            4,
        ),
        (
            4,
            Item::new(Some('b'), Bracket::new(3, BracketType::Open)),
            5,
        ),
        (5, Item::new(None, Bracket::new(3, BracketType::Close)), 6),
        (6, Item::new(None, Bracket::new(2, BracketType::Close)), 6),
        (6, Item::new(None, Bracket::new(1, BracketType::Close)), 10),
        (
            4,
            Item::new(Some('a'), Bracket::new(1, BracketType::Close)),
            7,
        ),
        (
            7,
            Item::new(Some('a'), Bracket::new(3, BracketType::Open)),
            8,
        ),
        (8, Item::new(None, Bracket::new(3, BracketType::Close)), 7),
        (
            7,
            Item::new(Some('b'), Bracket::new(3, BracketType::Open)),
            9,
        ),
        (9, Item::new(None, Bracket::new(3, BracketType::Close)), 10),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new(builder.build(1, [10]))
}

fn main() {
    let node_radius = 50.0;
    let spacing = 60.0;
    let text_scale = 15.0;

    let graph = get_graph();
    let layout = MinGridLayout::new(node_radius, spacing, &graph);
    let commands = draw_graph(&layout, text_scale, 3, 5);

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
            DrawCommand::Text { bounds, text } => draw_text_mut(
                &mut image,
                Rgb([0, 0, 0]),
                bounds.x1 as i32,
                bounds.y1 as i32,
                Scale::uniform(text_scale),
                &font,
                text.as_str(),
            ),
            DrawCommand::Curve(curve) => {
                draw_cubic_bezier_curve_mut(
                    &mut image,
                    curve.p1.as_tuple(),
                    curve.p4.as_tuple(),
                    curve.p2.as_tuple(),
                    curve.p3.as_tuple(),
                    Rgb([0, 0, 0]),
                );
                // draw_filled_circle_mut(
                //     &mut image,
                //     (curve.p2.x as i32, curve.p2.y as i32),
                //     3,
                //     Rgb([255, 0, 0]),
                // );
                // draw_filled_circle_mut(
                //     &mut image,
                //     (curve.p3.x as i32, curve.p3.y as i32),
                //     3,
                //     Rgb([255, 0, 0]),
                // );
            }
        }
    }

    image.save("images/g1.png").expect("Could not save image");
}
