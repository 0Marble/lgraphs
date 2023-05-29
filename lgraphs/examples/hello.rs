use std::{println, str::FromStr};

use graphs::graph::{default_graph::DefaultGraph, lgraph::LGraph, Graph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let g: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 6->; 1-[1->2; 2-a[2->2; 2-b->3; 3-a]2->3; 3-b->4; 4-]2->4; 3-a]1->5; 5-a->5; 5-b->6; 4-]1->6;")?;

    println!("{}", g.normal_form::<DefaultGraph<_, _>>(1).to_dot());

    Ok(())
}
