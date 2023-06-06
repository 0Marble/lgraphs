use std::{println, str::FromStr};

use graphs::graph::{default_graph::DefaultGraph, lgraph::LGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let g: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 2->; 1-a,[1->1; 1-b->2; 2-]1->2;")?;

    println!("{}", g);

    for t in g.core(1, 1) {
        println!("{}", t);
    }

    println!("{}", g.normal_form::<DefaultGraph<_, _>>());
    println!(
        "Does the graph not have letters on paired loops? {}",
        g.core11_no_letters_on_loops()
    );

    Ok(())
}
