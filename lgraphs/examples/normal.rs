use std::{println, str::FromStr};

use graphs::graph::{default_graph::DefaultGraph, lgraph::LGraph, Graph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let g: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 2->; 1-a,[1->1; 1-b->2; 2-]1->2;")?;

    println!("{}", g.normal_form::<DefaultGraph<_, _>>(1).to_dot());

    Ok(())
}
