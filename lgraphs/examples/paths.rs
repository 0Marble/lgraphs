use std::{println, str::FromStr};

use graphs::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let t = Path::from_str("1-[0->1->2-]0->2->3-[0->3->4-]0->4")?;

    for (l, r) in t.paired_loops() {
        println!("\nt = {} = t1 t2 t3 t4 t5", t);
        println!("t1 = {}", t.subpath(0, l.0).unwrap());
        println!("t2 = {}", t.subpath(l.0, l.1).unwrap());
        println!("t3 = {}", t.subpath(l.1, r.0).unwrap());
        println!("t1 = {}", t.subpath(r.0, r.1).unwrap());
        println!("t1 = {}", t.subpath(r.1, t.len_in_nodes() - 1).unwrap());
    }

    Ok(())
}
