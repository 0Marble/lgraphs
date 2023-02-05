use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

use crate::graphs::{
    graph_trait::Graph,
    refs::{NodeRef, Path},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Distance {
    Infinite,
    Finite(usize),
}

impl Add<Distance> for Distance {
    type Output = Distance;

    fn add(self, rhs: Distance) -> Self::Output {
        match (self, rhs) {
            (Distance::Infinite, Distance::Infinite) => Distance::Infinite,
            (Distance::Infinite, Distance::Finite(_)) => Distance::Infinite,
            (Distance::Finite(_), Distance::Infinite) => Distance::Infinite,
            (Distance::Finite(a), Distance::Finite(b)) => Distance::Finite(a + b),
        }
    }
}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Distance::Infinite, Distance::Infinite) => None,
            (Distance::Infinite, Distance::Finite(_)) => Some(std::cmp::Ordering::Greater),
            (Distance::Finite(_), Distance::Infinite) => Some(std::cmp::Ordering::Less),
            (Distance::Finite(a), Distance::Finite(b)) => Some(a.cmp(b)),
        }
    }
}

pub fn djikstra<'a, N, E, G>(graph: &'a G) -> Vec<(Path<'a, N, E>, usize, NodeRef<'a, N>)>
where
    G: Graph<N, E>,
    N: 'a,
    E: 'a,
{
    if graph.node_count() == 0 {
        return vec![];
    }

    let mut reached: HashSet<NodeRef<'a, N>> = HashSet::new();
    let mut distances = HashMap::new();
    distances.insert(graph.start_node(), (Distance::Finite(0), vec![]));
    for node in graph.nodes().filter(|n| n != &graph.start_node()) {
        distances.insert(node, (Distance::Infinite, vec![]));
    }

    while let Some((node, (dist, path))) =
        distances.iter().filter(|(n, _)| !reached.contains(n)).fold(
            distances.iter().find(|(n, _)| !reached.contains(n)),
            |acc, (node, dist_path)| match acc {
                Some((acc_node, acc_dist_path)) => {
                    let (acc_dist, _) = acc_dist_path;
                    let (dist, _) = dist_path;
                    match acc_dist.partial_cmp(dist) {
                        Some(std::cmp::Ordering::Less) => Some((acc_node, acc_dist_path)),
                        Some(_) => Some((node, dist_path)),
                        None => None,
                    }
                }
                None => None,
            },
        )
    {
        reached.insert(*node);
        let path = path.clone();
        let node = *node;
        let dist = *dist;

        for edge in graph.edges_from(node) {
            let (target_dist, _) = distances.get(&edge.target()).unwrap();
            let new_dist = dist + Distance::Finite(1);
            if *target_dist > new_dist {
                let mut path_to_target = path.clone();
                path_to_target.push(edge);
                distances.insert(edge.target(), (new_dist, path_to_target));
            }
        }
    }

    distances
        .into_iter()
        .map(|(node, (_, path))| {
            let dist = path.len();
            (Path::new(path), dist, node)
        })
        .collect()
}
