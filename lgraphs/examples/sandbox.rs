use std::{println, str::FromStr};

use graphs::graph::{default_graph::DefaultGraph, lgraph::LGraph, Graph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let g: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 1-a->1; 1-b,[0->2; 2-b,[0->2; 2-c->3; 3-b,]0->3; 3->;")?;

    let g = g.normal_form::<DefaultGraph<_, _>>();

    println!("{}", g.to_dot());
    // for t in g.core(1, 1) {
    //     println!("{}", t.to_dot());
    // }

    Ok(())
}
