use std::{hash::Hash, iter::once};

#[derive(Debug)]
pub struct NodeRef<'a, N> {
    contents: &'a N,
    index: usize,
}

impl<'a, N> NodeRef<'a, N> {
    pub fn new(contents: &'a N, index: usize) -> Self {
        Self { contents, index }
    }

    pub fn contents(&self) -> &'a N {
        self.contents
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl<'a, N> Ord for NodeRef<'a, N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<'a, N> PartialOrd for NodeRef<'a, N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<'a, N> Eq for NodeRef<'a, N> {}

impl<'a, N> PartialEq for NodeRef<'a, N> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<'a, N> Hash for NodeRef<'a, N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<'a, N> Copy for NodeRef<'a, N> {}

impl<'a, N> Clone for NodeRef<'a, N> {
    fn clone(&self) -> Self {
        Self {
            contents: self.contents,
            index: self.index,
        }
    }
}

#[derive(Debug)]
pub struct EdgeRef<'a, N, E> {
    contents: &'a E,
    source: &'a N,
    target: &'a N,
    index: usize,
    source_index: usize,
    target_index: usize,
}

impl<'a, N, E> EdgeRef<'a, N, E> {
    pub fn new(
        contents: &'a E,
        source: &'a N,
        target: &'a N,
        index: usize,
        source_index: usize,
        target_index: usize,
    ) -> Self {
        Self {
            contents,
            source,
            target,
            index,
            source_index,
            target_index,
        }
    }

    pub fn target(&self) -> NodeRef<'a, N> {
        NodeRef {
            contents: self.target,
            index: self.target_index,
        }
    }

    pub fn source(&self) -> NodeRef<'a, N> {
        NodeRef {
            contents: self.source,
            index: self.source_index,
        }
    }

    pub fn contents(&self) -> &'a E {
        self.contents
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn source_index(&self) -> usize {
        self.source_index
    }

    pub fn target_index(&self) -> usize {
        self.target_index
    }
}

impl<'a, N, E> Ord for EdgeRef<'a, N, E> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<'a, N, E> PartialOrd for EdgeRef<'a, N, E> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<'a, N, E> Eq for EdgeRef<'a, N, E> {}

impl<'a, N, E> PartialEq for EdgeRef<'a, N, E> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
            && self.source_index == other.source_index
            && self.target_index == other.target_index
    }
}

impl<'a, N, E> Hash for EdgeRef<'a, N, E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.source_index.hash(state);
        self.target_index.hash(state);
    }
}

impl<'a, N, E> Copy for EdgeRef<'a, N, E> {}

impl<'a, N, E> Clone for EdgeRef<'a, N, E> {
    fn clone(&self) -> Self {
        Self {
            contents: self.contents,
            source: self.source,
            target: self.target,
            index: self.index,
            source_index: self.source_index,
            target_index: self.target_index,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path<'a, N, E> {
    edges: Vec<EdgeRef<'a, N, E>>,
}

impl<'a, N, E> Path<'a, N, E> {
    pub fn new(edges: impl Into<Vec<EdgeRef<'a, N, E>>>) -> Self {
        Self {
            edges: edges.into(),
        }
    }

    pub fn nodes<'b>(&'b self) -> impl Iterator<Item = NodeRef<'a, N>> + 'b
    where
        'a: 'b,
    {
        self.edges
            .first()
            .into_iter()
            .flat_map(|e| once(e.source()))
            .chain(self.edges.iter().map(|e| e.target()))
    }

    pub fn edges<'b>(&'b self) -> impl Iterator<Item = EdgeRef<'a, N, E>> + 'b
    where
        'a: 'b,
    {
        self.edges.iter().cloned()
    }
}
