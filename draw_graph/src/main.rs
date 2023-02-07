use std::fmt::Display;

use imageproc::{
    drawing::{
        draw_cubic_bezier_curve_mut, draw_hollow_circle_mut, draw_hollow_rect_mut, draw_text_mut,
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

fn edge(
    from: i32,
    item: Option<char>,
    index: usize,
    open: bool,
    to: i32,
) -> (i32, Item<char>, i32) {
    (
        from,
        Item::new(
            item,
            Bracket::new(
                index,
                if open {
                    BracketType::Open
                } else {
                    BracketType::Close
                },
            ),
        ),
        to,
    )
}

fn render_graph<'a, N, E, G, L>(layout: &L, file_name: &str) -> Result<(), impl std::error::Error>
where
    N: Display + 'a,
    E: Display + 'a,
    G: Graph<N, E> + 'a,
    L: Layout<'a, N, E, G>,
{
    let mut image = RgbImage::from_pixel(
        layout.width().ceil() as u32,
        layout.height().ceil() as u32,
        Rgb([255, 255, 255]),
    );

    let text_scale = 15.0;
    let font = Vec::from(include_bytes!("../../fonts/gnu-free/FreeMonoBoldOblique.otf") as &[u8]);
    let font = Font::try_from_vec(font).unwrap();
    let commands = draw_graph(layout, text_scale, 3, 10);

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
            }
        }
    }

    image.save(file_name)
}

fn get_graph() -> LGraph<i32, char, impl Graph<i32, Item<char>>> {
    let edges = [
        edge(1, None, 1, true, 2),
        edge(2, Some('a'), 2, true, 3),
        edge(3, Some('b'), 3, true, 4),
        edge(4, None, 3, false, 3),
        edge(3, Some('c'), 4, true, 5),
        edge(5, None, 3, true, 6),
        edge(6, None, 3, false, 2),
        edge(2, Some('d'), 3, true, 7),
        edge(7, None, 3, false, 8),
        edge(8, None, 2, false, 8),
        edge(8, None, 4, false, 8),
        edge(8, None, 1, false, 9),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new(builder.build(1, [9]))
}

fn main() {
    let node_radius = 50.0;
    let spacing = 60.0;

    let g = get_graph();
    {
        let layout = MinGridLayout::new(node_radius, spacing, &g);
        render_graph(&layout, "images/graph5.png").expect("Could not render")
    }

    for d in 1..10 {
        let c = g.stack_core_graph(1, d, &mut DefaultBuilder::default());
        let layout = MinGridLayout::new(node_radius, spacing, &c);
        render_graph(&layout, format!("images/c1{d}-5.png").as_str()).expect("Could not render");

        // if g.nodes()
        //     .all(|node| c.nodes().any(|n| n.contents().node() == node.contents()))
        // {
        //     let c = g.stack_core_graph(1, d + 1, &mut DefaultBuilder::default());
        //     let layout = MinGridLayout::new(node_radius, spacing, &c);
        //     render_graph(&layout, format!("images/c1{}.png", d + 1).as_str())
        //         .expect("Could not render");

        let dc = g.delta_stack_core_graph(1, d + 1, &mut DefaultBuilder::default());
        let layout = MinGridLayout::new(node_radius, spacing, &dc);
        render_graph(&layout, format!("images/dc1{}-5.png", d + 1).as_str())
            .expect("Could not render");

        //     break;
        // }
    }

    let normal = g.normal_form(&mut DefaultBuilder::default());
    {
        let layout = MinGridLayout::new(node_radius, spacing, &normal);
        render_graph(&layout, "images/normal5.png").expect("Could not render")
    }

    let img = normal.regular_image(&mut DefaultBuilder::default());
    let no_nones = img.remove_nones(&mut DefaultBuilder::default());
    {
        let layout = MinGridLayout::new(node_radius, spacing, &no_nones);
        render_graph(&layout, "images/img5.png").expect("Could not render")
    }

    // let determined = no_nones.determine(&mut DefaultBuilder::default());
    // {
    //     let layout = MinGridLayout::new(node_radius, spacing, &determined);
    //     render_graph(&layout, "images/determined.png").expect("Could not render")
    // }

    // let minimized = determined.minimize(&mut DefaultBuilder::default());
    // {
    //     let layout = MinGridLayout::new(node_radius, spacing, &minimized);
    //     render_graph(&layout, "images/minimized.png").expect("Could not render")
    // }
}
