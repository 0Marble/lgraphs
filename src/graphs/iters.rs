pub struct Subsets<I>
where
    I: Iterator,
{
    inner: I,
    items: Vec<I::Item>,
    state: Vec<bool>,
    finished: bool,
}

impl<I> Subsets<I>
where
    I: Iterator,
    I::Item: Clone,
{
    pub fn new(inner: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            inner: inner.into_iter(),
            items: vec![],
            state: vec![],
            finished: false,
        }
    }

    fn cur_subset(&self) -> Vec<I::Item> {
        self.state
            .iter()
            .enumerate()
            .filter(|(_, is_selected)| **is_selected)
            .map(|(i, _)| self.items[i].clone())
            .collect()
    }

    fn step(&mut self) {
        for c in self.state.iter_mut() {
            if *c {
                *c = false;
            } else {
                *c = true;
                return;
            }
        }

        self.state.iter_mut().for_each(|c| *c = false);
        self.state.push(true);

        let new_item = self.inner.next();
        match new_item {
            Some(item) => self.items.push(item),
            None => self.finished = true,
        }
    }
}

impl<I> Iterator for Subsets<I>
where
    I: Iterator,
    I::Item: Clone,
{
    type Item = Vec<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let res = self.cur_subset();
        self.step();
        Some(res)
    }
}

pub struct AllPairs<I1, I2>
where
    I1: Iterator,
    I2: Iterator,
{
    it1: I1,
    it2: I2,
    cache: Vec<I2::Item>,
    cur: Option<I1::Item>,
    cache_index: usize,
}

impl<I1, I2> AllPairs<I1, I2>
where
    I1: Iterator,
    I2: Iterator,
{
    pub fn new(
        it1: impl IntoIterator<IntoIter = I1>,
        it2: impl IntoIterator<IntoIter = I2>,
    ) -> Self {
        let mut it1 = it1.into_iter();
        let it2 = it2.into_iter();
        Self {
            cur: it1.next(),
            it1,
            it2,
            cache: vec![],
            cache_index: 0,
        }
    }

    fn i2_next(&mut self) -> Option<I2::Item>
    where
        I2::Item: Clone,
    {
        match self.cache.get(self.cache_index).cloned() {
            Some(item) => {
                self.cache_index += 1;
                Some(item)
            }
            None => match self.it2.next() {
                Some(new_item) => {
                    self.cache_index += 1;
                    self.cache.push(new_item.clone());
                    Some(new_item)
                }
                None => None,
            },
        }
    }
}

impl<I1, I2> Iterator for AllPairs<I1, I2>
where
    I1: Iterator,
    I2: Iterator,
    I1::Item: Clone,
    I2::Item: Clone,
{
    type Item = (I1::Item, I2::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let b = match self.i2_next() {
            Some(b) => b,
            None => {
                self.cur = self.it1.next();
                self.cache_index = 0;
                self.i2_next()?
            }
        };
        let a = self.cur.as_ref()?.clone();
        Some((a, b))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn subsets() {
        let items = [1, 2, 3, 4];
        let subsets = Subsets::new(items)
            .map(|s| {
                println!("{:?}", s);
                s
            })
            .collect::<HashSet<_>>();
        let actual_subsets = HashSet::from([
            vec![],
            vec![1],
            vec![2],
            vec![3],
            vec![4],
            vec![1, 2],
            vec![1, 3],
            vec![1, 4],
            vec![2, 3],
            vec![2, 4],
            vec![3, 4],
            vec![1, 2, 3],
            vec![1, 2, 4],
            vec![1, 3, 4],
            vec![2, 3, 4],
            vec![1, 2, 3, 4],
        ]);

        assert_eq!(subsets, actual_subsets);
    }

    #[test]
    fn all_pairs() {
        let a = [1, 2, 3, 4];
        let b = ['a', 'b', 'c'];
        let pairs = AllPairs::new(a, b)
            .map(|p| {
                println!("{:?}", p);
                p
            })
            .collect::<HashSet<_>>();
        let actual_pairs = HashSet::from([
            (1, 'a'),
            (1, 'b'),
            (1, 'c'),
            (2, 'a'),
            (2, 'b'),
            (2, 'c'),
            (3, 'a'),
            (3, 'b'),
            (3, 'c'),
            (4, 'a'),
            (4, 'b'),
            (4, 'c'),
        ]);
        assert_eq!(pairs, actual_pairs);
    }
}
