use image::{imageops, Rgb, RgbImage, Rgba, RgbaImage};
use imageproc::point::Point;
use lgraphs::graphics::genometry::{Rect, Vec2};
use lgraphs::graphics::{DrawCommand, GraphDrawer};
use lgraphs::graphs::default::{Graph, GraphBuilder};
use lgraphs::graphs::graph_trait::GraphBuilder as GraphBuilderTrait;
use lgraphs::graphs::lgraph::{Bracket, Item, LGraph};
use rusttype::{Font, Scale};
use std::fs::File;

fn make_graph(src: String) -> Option<LGraph<i32, char, Graph<i32, Item<char>>>> {
    let json = json::parse(&src).ok()?;

    let mut builder = GraphBuilder::new();
    let json::JsonValue::Number(start_node) = json["start_node"] else {
        return None;
    };
    let json::JsonValue::Array(end_nodes) = &json["end_nodes"] else {
        return None;
    };

    let json::JsonValue::Array(edges) = &json["edges"] else {
        return None;
    };

    for edge in edges
        .iter()
        .flat_map(|v| {
            if let json::JsonValue::Array(e) = v {
                Some(e)
            } else {
                None
            }
        })
        .filter(|e| e.len() == 5)
    {
        let (json::JsonValue::Number(source), json::JsonValue::Number(target), json::JsonValue::Number(bracket_index), json::JsonValue::Boolean(is_open)) = (&edge[0], &edge[4], &edge[2], &edge[3]) else {
            continue;
        };

        let token = match &edge[1] {
            json::JsonValue::Null => None,
            json::JsonValue::String(s) => s.chars().next(),
            json::JsonValue::Short(s) => s.as_str().chars().next(),
            _ => continue,
        };

        let source: i32 = (*source).try_into().ok()?;
        let target: i32 = (*target).try_into().ok()?;
        let bracket_index: i32 = (*bracket_index).try_into().ok()?;
        let bracket = Bracket::new(bracket_index as usize, *is_open);

        builder.add_edge(source, Item::new(token, bracket), target);
    }

    Some(LGraph::new_unchecked::<GraphBuilder<_, _>, Graph<_, _>>(
        builder.build(),
        start_node.try_into().ok()?,
        end_nodes
            .iter()
            .flat_map(|v| {
                if let json::JsonValue::Number(n) = v {
                    Some(n)
                } else {
                    None
                }
            })
            .map(|n| (*n).try_into())
            .collect::<Result<Vec<i32>, _>>()
            .ok()?,
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let (graph_path, img_path) = match (std::env::args().nth(1), std::env::args().nth(2)) {
    //     (Some(n1), Some(n2)) => (n1, n2),
    //     _ => {
    //         return Err(Box::new(std::io::Error::new(
    //             std::io::ErrorKind::InvalidInput,
    //             format!(
    //                 "Usage: {} {{graph file}} {{output file}}",
    //                 std::env::args().next().unwrap(),
    //             ),
    //         )))
    //     }
    // };

    let graph_path = "assets/g2.json";
    let image_path = "images/g2.png";

    let graph = make_graph(
        std::io::read_to_string(
            &mut File::open(graph_path).unwrap_or_else(|_| panic!("Could not open {graph_path}")),
        )
        .unwrap_or_else(|_| panic!("Could not read {graph_path}")),
    )
    .unwrap_or_else(|| panic!("Could not parse {graph_path}"));

    let commands = GraphDrawer::new(20.0, 20.0).draw(
        &graph,
        graph.start_node().index(),
        graph.end_nodes().map(|n| n.index()),
        Rect::new(0.0, 0.0, 200.0, 200.0),
    );

    let mut image = RgbaImage::from_pixel(200, 200, Rgba([255, 255, 255, 255]));
    let font = Vec::from(include_bytes!("../fonts/gnu-free/FreeMono.otf") as &[u8]);
    let font = Font::try_from_vec(font).expect("Could not initialize font");

    dbg!(&commands);
    for command in commands {
        match command {
            DrawCommand::Node { clip } => {
                image = imageproc::drawing::draw_hollow_rect(
                    &image,
                    imageproc::rect::Rect::at(clip.x0 as i32, clip.y0 as i32)
                        .of_size(clip.width() as u32, clip.height() as u32),
                    Rgba([0, 0, 0, 255]),
                );
            }
            DrawCommand::Line { from, to } => {
                image = imageproc::drawing::draw_line_segment(
                    &image,
                    (from.x, from.y),
                    (to.x, to.y),
                    Rgba([0, 0, 0, 255]),
                )
            }
            DrawCommand::Text { text, clip } => {
                let size = f32::max(clip.width(), clip.height()) * f32::sqrt(2.0);
                let top_left = clip.uv(0.0, 0.0);
                let full_rect =
                    Rect::new(top_left.x, top_left.y, top_left.y + size, top_left.y + size);

                let mut text_image = RgbaImage::from_pixel(
                    full_rect.width() as u32,
                    full_rect.height() as u32,
                    Rgba([0, 0, 0, 0]),
                );
                let text_in_image = full_rect.center_rect(clip.width(), clip.height());
                dbg!(&text_in_image);
                imageproc::drawing::draw_text_mut(
                    &mut text_image,
                    Rgba([0, 0, 0, 255]),
                    (text_in_image.x0 - full_rect.x0) as i32,
                    (text_in_image.y0 - full_rect.y0) as i32,
                    Scale::uniform(13.0),
                    &font,
                    &text,
                );

                // text_image = imageproc::geometric_transformations::rotate(
                //     &text_image,
                //     (full_rect.width() * 0.5, full_rect.height() * 0.5),
                //     clip.x_facing.dot(&Vec2::new(1.0, 0.0)).acos(),
                //     imageproc::geometric_transformations::Interpolation::Bicubic,
                //     Rgba([0, 0, 0, 0]),
                // );

                imageops::overlay(
                    &mut image,
                    &text_image,
                    top_left.x as i64,
                    top_left.y as i64,
                );
            }
        }
    }

    image.save(image_path)?;
    Ok(())
}
