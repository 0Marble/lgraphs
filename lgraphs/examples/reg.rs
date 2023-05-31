use std::{println, str::FromStr};

use graphs::graph::{default_graph::DefaultGraph, lgraph::LGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let g1: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 1->2; 2->1; 2->3; 3->4; 4->1; 2->;")?;
    let g2: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 1-a->1; 1-b->2; 2-c->2; 2->;")?;
    let g3: LGraph<DefaultGraph<_, _>, usize, char> =
        LGraph::from_str("->1; 2->; 1-a,[1->1; 1-b->2; 2-]1->2;")?;

    println!("{}", g1.no_letters());
    println!("{}", g2.no_brackets());
    println!("{}", g3.core11_no_letters_on_loops());

    Ok(())
}
