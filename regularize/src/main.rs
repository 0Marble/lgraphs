use std::{fs::File, io::Read};

use lgraphs::{
    graphs::{default::DefaultBuilder, graph_trait::Graph},
    io::{reading::read, to_lgraph, writing::write},
};

// fn edge(
//     from: i32,
//     item: Option<char>,
//     index: usize,
//     open: bool,
//     to: i32,
// ) -> (i32, Item<char>, i32) {
//     (
//         from,
//         Item::new(
//             item,
//             Bracket::new(
//                 index,
//                 if open {
//                     BracketType::Open
//                 } else {
//                     BracketType::Close
//                 },
//             ),
//         ),
//         to,
//     )
// }

// fn example_1() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
//     let edges = [
//         edge(1, None, 1, true, 2),
//         edge(2, Some('a'), 2, true, 2),
//         edge(2, Some('b'), 3, true, 3),
//         edge(3, None, 3, false, 4),
//         edge(4, None, 2, false, 4),
//         edge(4, None, 1, false, 5),
//         edge(2, Some('c'), 3, true, 6),
//         edge(6, None, 3, false, 7),
//         edge(7, None, 2, false, 8),
//         edge(8, None, 2, false, 9),
//         edge(9, None, 2, false, 10),
//         edge(10, None, 1, false, 5),
//     ];
//     let mut builder = DefaultBuilder::default();
//     for (source, item, target) in edges {
//         builder.add_edge(source, item, target);
//     }
//     LGraph::new_unchecked(builder.build(1, [5]))
// }

// fn example_2() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
//     // todo!("Broken");

// let edges = [
//     edge(1, Some('a'), 1, true, 2),
//     edge(2, Some('b'), 0, true, 2),
//     edge(2, None, 0, false, 1),
//     edge(1, Some('d'), 0, true, 3),
//     edge(3, None, 0, false, 4),
//     edge(4, None, 1, false, 4),
// ];
// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [4]))
// }

// fn example_3() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
// let edges = [
//     edge(1, Some('a'), 0, true, 1),
//     edge(1, Some('b'), 0, false, 2),
// ];
// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [2]))
// }

// fn example_4() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
// let edges = [
//     edge(1, None, 1, true, 2),
//     edge(2, Some('a'), 2, true, 2),
//     edge(2, Some('b'), 3, true, 20),
//     edge(20, None, 3, false, 3),
//     edge(3, None, 2, false, 3),
//     edge(3, Some('c'), 3, true, 30),
//     edge(30, None, 3, false, 4),
//     edge(4, Some('a'), 2, true, 4),
//     edge(4, Some('d'), 3, true, 40),
//     edge(40, None, 3, false, 5),
//     edge(5, None, 2, false, 5),
//     edge(5, None, 1, false, 6),
// ];
// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [6]))
// }

// fn example_5() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
// let edges = [
//     edge(1, Some('a'), 1, true, 2),
//     edge(2, Some('b'), 0, true, 3),
//     edge(3, None, 0, false, 2),
//     edge(2, Some('c'), 2, true, 1),
//     edge(1, Some('d'), 0, true, 4),
//     edge(4, None, 0, false, 5),
//     edge(5, None, 1, false, 5),
//     edge(5, None, 2, false, 5),
// ];
// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [5]))
// }

// fn example_6() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
//     let edges = [
//         edge(1, Some('a'), 1, true, 2),
//         edge(2, Some('b'), 2, true, 3),
//         edge(3, None, 2, false, 4),
//         edge(4, None, 1, false, 5),
//     ];
//     let mut builder = DefaultBuilder::default();
//     for (source, item, target) in edges {
//         builder.add_edge(source, item, target);
//     }
//     LGraph::new_unchecked(builder.build(1, [5]))
// }

// fn example_7() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
// let edges = [
//     edge(1, None, 1, true, 2),
//     edge(2, Some('a'), 2, true, 2),
//     edge(2, None, 2, false, 3),
//     edge(3, None, 2, false, 4),
//     edge(4, None, 1, false, 5),
//     edge(5, Some('b'), 2, true, 5),
//     edge(5, Some('c'), 3, true, 6),
//     edge(6, None, 3, false, 7),
//     edge(7, None, 2, false, 7),
// ];

// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [7]))
// }

// fn example_8() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
//     let edges = [
//         edge(1, Some('a'), 1, true, 1),
//         edge(1, Some('b'), 2, true, 2),
//         edge(2, None, 2, false, 3),
//         edge(3, None, 1, false, 3),
//     ];

//     let mut builder = DefaultBuilder::default();
//     for (source, item, target) in edges {
//         builder.add_edge(source, item, target);
//     }
//     LGraph::new_unchecked(builder.build(1, [3]))
// }

// fn example_9() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
// let edges = [
//     edge(1, Some('a'), 1, true, 1),
//     edge(1, Some('b'), 0, true, 2),
//     edge(2, Some('c'), 2, true, 3),
//     edge(3, None, 2, true, 4),
//     edge(4, None, 2, true, 2),
//     edge(2, Some('d'), 2, true, 5),
//     edge(5, None, 2, false, 5),
//     edge(5, None, 0, false, 6),
//     edge(6, None, 1, false, 6),
// ];
// let mut builder = DefaultBuilder::default();
// for (source, item, target) in edges {
//     builder.add_edge(source, item, target);
// }
// LGraph::new_unchecked(builder.build(1, [6]))
// }

fn main() {
    use std::io::Write;
    // let args = std::env::args().collect::<Vec<_>>();
    let args = vec![
        "input=graphs/graph8.json".to_string(),
        "output=graphs/graph_image8.json".to_string(),
    ];
    let input = args
        .iter()
        .find_map(|arg| arg.strip_prefix("input="))
        .expect("Please specify an input file with input=[FILE | -]");
    let output = args
        .iter()
        .find_map(|arg| arg.strip_prefix("output="))
        .expect("Please specify an output file with output=[FILE]");

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

    let g = read(&src, &mut DefaultBuilder::default())
        .unwrap_or_else(|e| panic!("Could not parse lgraph: {}", e));
    let g = to_lgraph(&g, &mut DefaultBuilder::default());

    let normal = g.normal_form(&mut DefaultBuilder::default());
    let img = normal.regular_image(&mut DefaultBuilder::default());
    let no_nones = img.remove_nones(&mut DefaultBuilder::default());
    write!(&mut File::create(output).unwrap(), "{}", write(&no_nones)).unwrap();
}
