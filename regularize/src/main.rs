use std::fs::File;

use lgraphs::{
    graphs::{
        default::{DefaultBuilder, DefaultGraph},
        graph_trait::{Builder, Graph},
        lgraph::{Bracket, BracketType, Item, LGraph},
    },
    io::writing::as_json,
};

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

fn example_1() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, None, 1, true, 2),
        edge(2, Some('a'), 2, true, 2),
        edge(2, Some('b'), 3, true, 3),
        edge(3, None, 3, false, 4),
        edge(4, None, 2, false, 4),
        edge(4, None, 1, false, 5),
        edge(2, Some('c'), 3, true, 6),
        edge(6, None, 3, false, 7),
        edge(7, None, 2, false, 8),
        edge(8, None, 2, false, 9),
        edge(9, None, 2, false, 10),
        edge(10, None, 1, false, 5),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [5]))
}

fn example_2() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    // todo!("Broken");

    let edges = [
        edge(1, Some('a'), 1, true, 2),
        edge(2, Some('b'), 0, true, 2),
        edge(2, None, 0, false, 1),
        edge(1, Some('d'), 0, true, 3),
        edge(3, None, 0, false, 4),
        edge(4, None, 1, false, 4),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [4]))
}

fn example_3() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, Some('a'), 0, true, 1),
        edge(1, Some('b'), 0, false, 2),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [2]))
}

fn example_4() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, None, 1, true, 2),
        edge(2, Some('a'), 2, true, 2),
        edge(2, Some('b'), 3, true, 20),
        edge(20, None, 3, false, 3),
        edge(3, None, 2, false, 3),
        edge(3, Some('c'), 3, true, 30),
        edge(30, None, 3, false, 4),
        edge(4, Some('a'), 2, true, 4),
        edge(4, Some('d'), 3, true, 40),
        edge(40, None, 3, false, 5),
        edge(5, None, 2, false, 5),
        edge(5, None, 1, false, 6),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [6]))
}

fn example_5() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, Some('a'), 1, true, 2),
        edge(2, Some('b'), 0, true, 3),
        edge(3, None, 0, false, 2),
        edge(2, Some('c'), 2, true, 1),
        edge(1, Some('d'), 0, true, 4),
        edge(4, None, 0, false, 5),
        edge(5, None, 1, false, 5),
        edge(5, None, 2, false, 5),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [5]))
}

fn example_6() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, Some('a'), 1, true, 2),
        edge(2, Some('b'), 2, true, 3),
        edge(3, None, 2, false, 4),
        edge(4, None, 1, false, 5),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [5]))
}

fn example_7() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, None, 1, true, 2),
        edge(2, Some('a'), 2, true, 2),
        edge(2, None, 2, false, 3),
        edge(3, None, 2, false, 4),
        edge(4, None, 1, false, 5),
        edge(5, Some('b'), 2, true, 5),
        edge(5, Some('c'), 3, true, 6),
        edge(6, None, 3, false, 7),
        edge(7, None, 2, false, 7),
    ];

    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [7]))
}

fn example_8() -> LGraph<i32, char, impl Graph<i32, Item<char>>> {
    let edges = [
        edge(1, Some('a'), 1, true, 1),
        edge(1, Some('b'), 2, true, 2),
        edge(2, None, 2, false, 3),
        edge(3, None, 1, false, 3),
    ];

    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [3]))
}

fn example_9() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
    let edges = [
        edge(1, Some('a'), 1, true, 1),
        edge(1, Some('b'), 0, true, 2),
        edge(2, Some('c'), 2, true, 3),
        edge(3, None, 2, true, 4),
        edge(4, None, 2, true, 2),
        edge(2, Some('d'), 2, true, 5),
        edge(5, None, 2, false, 5),
        edge(5, None, 0, false, 6),
        edge(6, None, 1, false, 6),
    ];
    let mut builder = DefaultBuilder::default();
    for (source, item, target) in edges {
        builder.add_edge(source, item, target);
    }
    LGraph::new_unchecked(builder.build(1, [6]))
}

fn main() {
    use std::io::Write;

    let g = example_1();
    write!(
        &mut File::create("graph.json").unwrap(),
        "{}",
        as_json(&g).unwrap()
    )
    .unwrap();

    let normal = g.normal_form(&mut DefaultBuilder::default());
    write!(
        &mut File::create("graph_normal.json").unwrap(),
        "{}",
        as_json(&normal).unwrap()
    )
    .unwrap();

    let img = normal.regular_image(&mut DefaultBuilder::default());
    let no_nones = img
        .remove_nones(&mut DefaultBuilder::default())
        .rebuild_to_node_nums(&mut DefaultBuilder::default());
    write!(
        &mut File::create("graph_image.json").unwrap(),
        "{}",
        as_json(&no_nones).unwrap()
    )
    .unwrap();

    println!("Hello, world!");
}
