use std::{fs::File, io::Write};

use lgraphs::{
    graphs::{
        default::{DefaultBuilder, DefaultGraph},
        graph_trait::{Builder, Graph},
        lgraph::*,
    },
    io::writing::write,
};

fn edge(
    from: i32,
    item: Option<char>,
    index: usize,
    open: bool,
    to: i32,
) -> (i32, Item<char>, i32) {
    (from, Item::new(item, Bracket::new(index, open)), to)
}

fn example() -> LGraph<i32, char, DefaultGraph<i32, Item<char>>> {
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

fn main() {
    let g = example();
    write!(&mut File::create("graph.json").unwrap(), "{}", write(&g)).unwrap();

    // for (i, p) in g
    //     .progenetors(&mut DefaultBuilder::default())
    //     .into_iter()
    //     .enumerate()
    // {
    //     write!(
    //         &mut File::create(format!("p{i}.json")).unwrap(),
    //         "{}",
    //         as_json(&p).unwrap()
    //     )
    //     .unwrap();
    // }

    let core_paths = g.faster_core(1, 1, Some(10));
    let core = g.graph_from_paths_unchecked(core_paths.into_iter(), &mut DefaultBuilder::default());
    write!(&mut File::create("core.json").unwrap(), "{}", write(&core)).unwrap();
}
