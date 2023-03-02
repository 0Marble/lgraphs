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

impl Distance {
    fn is_infinite(&self) -> bool {
        matches!(self, Self::Infinite)
    }
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
            (Distance::Finite(a), Distance::Finite(b)) => a.partial_cmp(b),
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

    loop {
        let mut min: Option<(NodeRef<_>, Distance, _)> = None;
        for (node, (dist, path)) in &distances {
            if reached.contains(node) {
                continue;
            }

            if let Some((_, min_dist, _)) = min {
                if min_dist.partial_cmp(dist) == Some(std::cmp::Ordering::Greater) {
                    min = Some((*node, *dist, path));
                }
            } else if dist != &Distance::Infinite {
                min = Some((*node, *dist, path));
            }
        }

        let Some((node, dist, path)) = min else {break;};

        reached.insert(node);
        let path = path.clone();
        let node = node;
        let dist = dist;

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
        .filter(|(_, (dist, _))| !dist.is_infinite())
        .map(|(node, (_, path))| {
            let dist = path.len();
            (Path::new(path), dist, node)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::graphs::{default::DefaultBuilder, graph_trait::Builder};

    use super::djikstra;

    #[test]
    fn shortest() {
        let edges = [('a', 'b'), ('b', 'c'), ('c', 'a'), ('a', 'd')];
        let mut builder = DefaultBuilder::default();
        for (from, to) in edges {
            builder.add_edge(from, (), to);
        }
        let graph = builder.build('a', ['d']);
        let distances = djikstra(&graph);

        dbg!(&distances);

        let find = |node_name: char| {
            distances
                .iter()
                .find(|(_, _, n)| n.contents() == &node_name)
                .unwrap()
        };
        let (_, a_dist, _) = find('a');
        let (_, b_dist, _) = find('b');
        let (_, c_dist, _) = find('c');
        let (_, d_dist, _) = find('d');

        assert_eq!(*a_dist, 0);
        assert_eq!(*b_dist, 1);
        assert_eq!(*c_dist, 2);
        assert_eq!(*d_dist, 1);
    }
}
