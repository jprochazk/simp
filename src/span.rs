use std::{
    fmt::Display,
    ops::{Index, Range},
};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    #[inline]
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }

    #[inline]
    pub fn start(self) -> usize {
        self.start as usize
    }

    #[inline]
    pub fn end(self) -> usize {
        self.end as usize
    }
}

impl From<std::ops::Range<usize>> for Span {
    #[inline]
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start as u32,
            end: value.end as u32,
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    #[inline]
    fn from(value: Span) -> Self {
        Self {
            start: value.start as usize,
            end: value.end as usize,
        }
    }
}

impl<T: Index<Range<usize>>> Index<Span> for [T] {
    type Output = <[T] as Index<Range<usize>>>::Output;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        self.index(Range::from(index))
    }
}

impl Index<Span> for str {
    type Output = <str as Index<Range<usize>>>::Output;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        self.index(Range::from(index))
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}
