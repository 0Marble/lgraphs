use std::{collections::HashMap, fmt::Display, fs::File, io::Read};

use imageproc::{
    drawing::{
        draw_cubic_bezier_curve_mut, draw_hollow_circle_mut, draw_hollow_rect_mut, draw_text_mut,
    },
    rect::Rect,
};
use json::JsonValue;
use lgraphs::{
    drawing::{
        drawer::{draw_graph, DrawCommand, LabelDrawer},
        layout::{Layout, ManualGridLayout, MinGridLayout},
    },
    graphs::{
        default::DefaultBuilder,
        graph_trait::{Builder, Graph},
        lgraph::{Bracket, Item, LGraph},
    },
};

use image::{Rgb, RgbImage};
use rusttype::{Font, Scale};

fn render_graph<'a, N, E, G, L>(
    layout: &L,
    file_name: &str,
    node_drawer: &impl LabelDrawer<N>,
    edge_drawer: &impl LabelDrawer<E>,
) -> Result<(), impl std::error::Error>
where
    N: 'a,
    E: 'a,
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
    let commands = draw_graph(layout, text_scale, 3, 10, node_drawer, edge_drawer);

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
                if text.starts_with('a') {
                    Rgb([255, 0, 0])
                } else if text.starts_with('b') {
                    Rgb([0, 255, 0])
                } else if text.starts_with('c') {
                    Rgb([0, 0, 255])
                } else {
                    Rgb([0, 0, 0])
                },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Label {
    Int(i32),
    Char(char),
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Int(i) => write!(f, "{}", i),
            Label::Char(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Clone)]
enum ParseError {
    ExpectedLabel(String),
    ExpectedArrayOfLabels(String),
    ExpectedArrayOfEdges(String),
    ExpectedBracket(String),
    ExpectedBool(String),
    ExpectedNumber(String),
    ExpectedArrayOfLocations,
    ExpectedLocation(usize),
    NoSuchNode(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

fn read_label(val: &JsonValue, field_name: &str) -> Result<Label, Box<dyn std::error::Error>> {
    let label = val
        .as_i32()
        .map(Label::Int)
        .or_else(|| val.as_str().and_then(|s| s.chars().next()).map(Label::Char))
        .ok_or_else(|| ParseError::ExpectedLabel(field_name.to_string()))?;

    Ok(label)
}

fn read_edge(
    val: &JsonValue,
    field_name: &str,
) -> Result<(Label, Option<Label>, Label), Box<dyn std::error::Error>> {
    let source = read_label(&val["source"], &format!("{}[source]", field_name))?;
    let target = read_label(&val["target"], &format!("{}[target]", field_name))?;
    let item = read_label(&val["item"], &format!("{}[item]", field_name)).ok();

    Ok((source, item, target))
}

fn read_graph<B>(src: &str, builder: &mut B) -> Result<B::TargetGraph, Box<dyn std::error::Error>>
where
    B: Builder<Label, Option<Label>>,
{
    let json = json::parse(src)?;
    let start_node = read_label(&json["start_node"], "start_node")?;
    let end_nodes = match &json["end_nodes"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_label(v, &format!("end_nodes[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfLabels("end_nodes".to_string()))?,
    };
    let edges = match &json["edges"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_edge(v, &format!("edges[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfEdges("edges".to_string()))?,
    };

    builder.clear();
    for (source, edge, target) in edges {
        builder.add_edge(source, edge, target);
    }

    Ok(builder.build(start_node, end_nodes))
}

fn read_lgraph_edge(
    val: &JsonValue,
    field_name: &str,
) -> Result<(Label, Item<Label>, Label), Box<dyn std::error::Error>> {
    let source = read_label(&val["source"], &format!("{}[source]", field_name))?;
    let target = read_label(&val["target"], &format!("{}[target]", field_name))?;
    let item = read_label(&val["item"], &format!("{}[item]", field_name)).ok();
    let bracket = match &val["bracket"] {
        JsonValue::Object(obj) => Bracket::new(
            obj["index"].as_usize().ok_or_else(|| {
                ParseError::ExpectedNumber(format!("{}[bracket][index]", field_name))
            })?,
            match obj["is_open"].as_bool() {
                Some(true) => lgraphs::graphs::lgraph::BracketType::Open,
                Some(false) => lgraphs::graphs::lgraph::BracketType::Close,
                _ => Err(ParseError::ExpectedBool(format!(
                    "{}[bracket][is_open]",
                    field_name
                )))?,
            },
        ),
        _ => Err(ParseError::ExpectedBracket(format!(
            "{}[bracket]",
            field_name
        )))?,
    };

    Ok((source, Item::new(item, bracket), target))
}

fn read_lgraph<B>(
    src: &str,
    builder: &mut B,
) -> Result<LGraph<Label, Label, B::TargetGraph>, Box<dyn std::error::Error>>
where
    B: Builder<Label, Item<Label>>,
    B::TargetGraph: Graph<Label, Item<Label>>,
{
    let json = json::parse(src)?;
    let start_node = read_label(&json["start_node"], "start_node")?;
    let end_nodes = match &json["end_nodes"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_label(v, &format!("end_nodes[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfLabels("end_nodes".to_string()))?,
    };
    let edges = match &json["edges"] {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, v)| read_lgraph_edge(v, &format!("edges[{}]", i)))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfEdges("edges".to_string()))?,
    };

    builder.clear();
    for (source, edge, target) in edges {
        builder.add_edge(source, edge, target);
    }

    Ok(LGraph::new(builder.build(start_node, end_nodes)))
}

struct Drawer;
impl LabelDrawer<Label> for Drawer {
    fn draw(&self, label: &Label) -> String {
        match label {
            Label::Int(i) => i.to_string(),
            Label::Char(c) => c.to_string(),
        }
    }
}
impl LabelDrawer<Option<Label>> for Drawer {
    fn draw(&self, label: &Option<Label>) -> String {
        match label {
            Some(l) => self.draw(l),
            None => "_".to_string(),
        }
    }
}
impl LabelDrawer<Item<Label>> for Drawer {
    fn draw(&self, label: &Item<Label>) -> String {
        label.to_string()
    }
}

fn read_location(
    val: &JsonValue,
    index: usize,
) -> Result<(Label, usize, usize), Box<dyn std::error::Error>> {
    let loc = match val {
        JsonValue::Array(a) => (
            a.get(0)
                .map(|l| read_label(l, val.to_string().as_str()))
                .ok_or_else(|| {
                    ParseError::ExpectedLabel(format!("location {}, index 0", index))
                })??,
            a.get(1).and_then(|v| v.as_usize()).ok_or_else(|| {
                ParseError::ExpectedNumber(format!("location {}, index 1", index))
            })?,
            a.get(2).and_then(|v| v.as_usize()).ok_or_else(|| {
                ParseError::ExpectedNumber(format!("location {}, index 2", index))
            })?,
        ),
        _ => Err(ParseError::ExpectedLocation(index))?,
    };

    Ok(loc)
}

fn manual_layout<'a, E, G>(
    src: &str,
    graph: &'a G,
) -> Result<ManualGridLayout<'a, Label, E, G>, Box<dyn std::error::Error>>
where
    G: Graph<Label, E>,
{
    let json = json::parse(src)?;
    let locations = match &json {
        JsonValue::Array(a) => a
            .iter()
            .enumerate()
            .map(|(i, val)| read_location(val, i))
            .collect::<Result<Vec<_>, _>>()?,
        _ => Err(ParseError::ExpectedArrayOfLocations)?,
    };

    let mut node_locations = HashMap::new();
    for (node, x, y) in locations {
        let node = graph
            .node_with_contents(&node)
            .ok_or_else(|| ParseError::NoSuchNode(node.to_string()))?;
        node_locations.insert(node, (x, y));
    }

    Ok(ManualGridLayout::new(50.0, 60.0, graph, node_locations))
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let input = args
        .iter()
        .find_map(|arg| arg.strip_prefix("input="))
        .expect("Please specify an input file with input=file");
    let output = args
        .iter()
        .find_map(|arg| arg.strip_prefix("output="))
        .expect("Please specify an output file with output=file");
    let layout = args
        .iter()
        .find_map(|arg| arg.strip_prefix("layout="))
        .unwrap_or("auto");
    let is_lgraph = args.iter().any(|arg| arg.as_str().eq("--lgraph"));
    let manual_locations_file = args
        .iter()
        .find_map(|arg| arg.strip_prefix("locations="))
        .unwrap_or("-");

    let mut src = String::new();

    if input == "-" {
        println!("Reading graph from stdin, press Control+D to finish input");
        std::io::stdin()
            .read_to_string(&mut src)
            .expect("Could not read source");
        println!("Graph successfully read");
    } else {
        File::open(input)
            .unwrap_or_else(|e| panic!("Could not open file {}: {}", input, e))
            .read_to_string(&mut src)
            .unwrap_or_else(|e| panic!("Could not read from file {}: {}", input, e));
    }

    if is_lgraph {
        let g = read_lgraph(&src, &mut DefaultBuilder::default())
            .unwrap_or_else(|e| panic!("Could not parse lgraph: {}", e));
        let layout: Box<dyn Layout<_, _, _>> = match layout {
            "auto" => Box::new(MinGridLayout::new(50.0, 60.0, &g)),
            "manual" => {
                let mut locations = String::new();
                if manual_locations_file == "-" {
                    println!("Reading node locations from stdin, press Control+D to finish input");
                    std::io::stdin()
                        .read_to_string(&mut locations)
                        .expect("Could not read source");
                    println!("Locations successfully read");
                } else {
                    File::open(manual_locations_file)
                        .unwrap_or_else(|e| {
                            panic!("Could not open file {}: {}", manual_locations_file, e)
                        })
                        .read_to_string(&mut locations)
                        .unwrap_or_else(|e| {
                            panic!("Could not read from file {}: {}", manual_locations_file, e)
                        });
                }
                Box::new(
                    manual_layout(locations.as_str(), &g)
                        .unwrap_or_else(|e| panic!("Could not parse node locations: {}", e)),
                )
            }
            other => panic!("Unknown layout {}", other),
        };

        render_graph(&layout, output, &Drawer, &Drawer)
            .unwrap_or_else(|e| panic!("Could not render a graph to {}: {}", output, e));
    } else {
        let g = read_graph(&src, &mut DefaultBuilder::default())
            .unwrap_or_else(|e| panic!("Could not parse graph: {}", e));
        let layout: Box<dyn Layout<_, _, _>> = match layout {
            "auto" => Box::new(MinGridLayout::new(50.0, 60.0, &g)),
            "manual" => {
                let mut locations = String::new();
                if manual_locations_file == "-" {
                    println!("Reading node locations from stdin, press Control+D to finish input");
                    std::io::stdin()
                        .read_to_string(&mut locations)
                        .expect("Could not read source");
                    println!("Locations successfully read");
                } else {
                    File::open(manual_locations_file)
                        .unwrap_or_else(|e| {
                            panic!("Could not open file {}: {}", manual_locations_file, e)
                        })
                        .read_to_string(&mut locations)
                        .unwrap_or_else(|e| {
                            panic!("Could not read from file {}: {}", manual_locations_file, e)
                        });
                }
                Box::new(
                    manual_layout(locations.as_str(), &g)
                        .unwrap_or_else(|e| panic!("Could not parse node locations: {}", e)),
                )
            }
            other => panic!("Unknown layout {}", other),
        };
        render_graph(&layout, output, &Drawer, &Drawer)
            .unwrap_or_else(|e| panic!("Could not render a graph to {}: {}", output, e));
    }

    println!(
        "Successfully drawn to {}/{}",
        std::env::current_dir()
            .expect("Could not determine cwd")
            .display(),
        output,
    );
}
