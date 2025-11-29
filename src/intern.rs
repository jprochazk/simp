//! String interner which stores strings in a single contiguous buffer.
//!
//! Adapted from <https://github.com/CAD97/strena/blob/1ddbf48b9f639ffbb4c89b0d51cb005fc0e3a4f7/src/lib.rs>
//!
//! MIT/Apache-2.0 licensed

macro_rules! index_unchecked {
    ($place:expr, $index:expr) => {{
        debug_assert!($place.get($index).is_some());
        $place.get_unchecked($index)
    }};
}

pub mod simple;

use std::{
    hash::{BuildHasher, Hash, Hasher as _},
    ops::{Index, Range},
};

use core::{iter, slice};
use hashbrown::{HashMap, hash_map::RawEntryMut};

fn make_hash(builder: &impl BuildHasher, hashee: &(impl ?Sized + Hash)) -> u64 {
    let state = &mut builder.build_hasher();
    hashee.hash(state);
    state.finish()
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone)]
struct Opaque<T>(T);

type Idx = u32;
pub type NonZeroIdx = core::num::NonZeroU32;
const IDX_MAX: u32 = u32::MAX;

#[derive(Debug, Copy, Clone)]
pub struct Span {
    base: Idx,
    len: Idx,
}

impl Span {
    #[inline]
    fn range(self) -> Range<usize> {
        self.base as usize..self.base as usize + self.len as usize
    }
}

pub trait Intern {
    /// Convert the symbol into a `u32` index.
    ///
    /// It may be converted back to [`Self`] via [`Intern::from_index`]
    fn index(self) -> u32;

    /// Converts an index acquired from [`Intern::index`] back to [`Self`].
    ///
    /// ## Safety
    ///
    /// - The value of `v` must be the same as it was when acquired from [`Symbol::index`].
    unsafe fn from_index(v: u32) -> Self;
}

macro_rules! declare_intern_id {
    ($vis:vis $Id:ident) => {
        /// An interned string.
        ///
        /// This is an index into an [`Interner`]. Symbols from different arenas
        /// don't have a defined order.
        #[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
        $vis struct $Id {
            raw: $crate::intern::NonZeroIdx,
        }

        impl $crate::intern::Intern for $Id {
            /// Convert the symbol into a `u32` index.
            ///
            /// It may be converted back to a [`Symbol`] via [`Symbol::from_index`]
            #[inline]
            fn index(self) -> u32 {
                self.raw.get() - 1
            }

            /// Converts an index acquired from [`Symbol::index`] back to a [`Symbol`].
            ///
            /// ## Safety
            ///
            /// - The value of `v` must be the same as it was when acquired from [`Symbol::index`].
            unsafe fn from_index(v: u32) -> Self {
                Self {
                    raw: unsafe { $crate::intern::NonZeroIdx::new_unchecked(v + 1) },
                }
            }
        }
    };
}

/// This interner caches strings and yields "symbols".
///
/// Symbols are [`Copy`] and two symbols from the same arena may be used to test for
/// string identity.
///
/// Interned strings are stored in a single concatenated string.
#[derive(Clone)]
pub struct Interner<Id, S = rustc_hash::FxBuildHasher> {
    hasher: S,
    // SAFETY: symbols must be valid indexes into `symbol_to_span`.
    // `Opaque<Symbol>` is treated in the hashmap as the string it represents.
    string_to_symbol: HashMap<Opaque<Id>, (), ()>,
    // SAFETY: spans must be valid spans in `span_to_string`.
    symbol_to_span: Vec<Span>,
    span_to_string: String,
}

impl<Id: std::fmt::Debug + Intern, S: Default> Default for Interner<Id, S> {
    fn default() -> Self {
        Self::with_hasher(Default::default())
    }
}

impl<Id: std::fmt::Debug + Intern> Interner<Id> {
    /// Creates a new empty interner.
    pub fn new() -> Self {
        Default::default()
    }

    /// Creates an empty interner with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_hasher_and_capacity(Default::default(), capacity)
    }
}

impl<Id: std::fmt::Debug + Intern, S> Interner<Id, S> {
    /// Creates an empty interner which will use the given hash builder.
    pub fn with_hasher(hasher: S) -> Self {
        Interner::with_hasher_and_capacity(hasher, Default::default())
    }

    /// Creates an empty interner with the specified capacity and hash builder.
    pub fn with_hasher_and_capacity(hasher: S, capacity: usize) -> Self {
        // chosen arbitrarily, 1 string ~= 8 bytes
        let entries = capacity / 8;
        Interner {
            hasher,
            string_to_symbol: HashMap::with_capacity_and_hasher(entries, ()),
            symbol_to_span: Vec::with_capacity(entries),
            span_to_string: String::with_capacity(capacity),
        }
    }

    /// The number of uniquely interned strings.
    pub fn len(&self) -> usize {
        self.symbol_to_span.len()
    }

    /// Returns true if the interner has no elements.
    pub fn is_empty(&self) -> bool {
        self.symbol_to_span.is_empty()
    }

    /// The number of interned bytes.
    pub fn len_bytes(&self) -> usize {
        self.span_to_string.len()
    }

    /// Returns the string associated with the given symbol.
    pub fn get(&self, s: Id) -> Option<&str> {
        let span = *self.symbol_to_span.get(s.index() as usize)?;
        unsafe { Some(index_unchecked!(self.span_to_string, span.range())) }
    }

    /// Iterate all interned strings in insertion order.
    pub fn iter(&self) -> Iter<'_> {
        <&Self>::into_iter(self)
    }
}

impl<Id: std::fmt::Debug + Intern, S> Index<Id> for Interner<Id, S> {
    type Output = str;
    fn index(&self, s: Id) -> &str {
        self.get(s).expect("no entry found for symbol")
    }
}

#[inline]
fn insert_substring(string: &mut String, s: &str) -> Span {
    if string
        .len()
        .checked_add(s.len())
        .map(|end| end > IDX_MAX as usize)
        .unwrap_or(true)
    {
        panic!("interner overflowed")
    }
    let base = string.len() as Idx;
    let len = s.len() as Idx;
    string.push_str(s);
    Span { base, len }
}

impl<Id: std::fmt::Debug + Intern + Copy, S: BuildHasher> Interner<Id, S> {
    /// Gets the interned symbol for the string.
    pub fn find(&self, s: &str) -> Option<Id> {
        let hash = make_hash(&self.hasher, s);
        let entry = self
            .string_to_symbol
            .raw_entry()
            .from_hash(hash, |&Opaque(symbol)| {
                let span =
                    unsafe { index_unchecked!(self.symbol_to_span, symbol.index() as usize) };
                s == unsafe { index_unchecked!(self.span_to_string, span.range()) }
            });

        entry.map(|(&Opaque(symbol), &())| symbol)
    }

    /// Interns the given value.
    pub fn intern(&mut self, s: &str) -> Id {
        let hash = make_hash(&self.hasher, s);
        let entry = self
            .string_to_symbol
            .raw_entry_mut()
            .from_hash(hash, |&Opaque(symbol)| {
                let span =
                    unsafe { index_unchecked!(self.symbol_to_span, symbol.index() as usize) };
                s == unsafe { index_unchecked!(self.span_to_string, span.range()) }
            });

        let (&mut Opaque(symbol), &mut ()) = match entry {
            RawEntryMut::Occupied(entry) => entry.into_key_value(),
            RawEntryMut::Vacant(entry) => {
                let span = insert_substring(&mut self.span_to_string, s);
                let symbol = unsafe { Id::from_index(self.symbol_to_span.len() as u32) };
                self.symbol_to_span.push(span);

                entry.insert_with_hasher(hash, Opaque(symbol), (), |&Opaque(symbol)| {
                    let span =
                        unsafe { index_unchecked!(self.symbol_to_span, symbol.index() as usize) };
                    let s = unsafe { index_unchecked!(self.span_to_string, span.range()) };
                    make_hash(&self.hasher, s)
                })
            }
        };

        symbol
    }
}
/// An iterator over all interned strings from an [`Interner`].
#[derive(Debug, Clone)]
pub struct Iter<'a> {
    pub(super) spans: slice::Iter<'a, Span>,
    pub(super) span_to_string: &'a str,
}

impl<'a, Id, S> IntoIterator for &'a Interner<Id, S> {
    type Item = &'a str;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            spans: self.symbol_to_span.iter(),
            span_to_string: &self.span_to_string,
        }
    }
}

// The same impls as iter::Map
impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        let span = self.spans.next()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.range()) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.spans.size_hint()
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a str> {
        let span = self.spans.next_back()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.range()) })
    }
}

impl ExactSizeIterator for Iter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.spans.len()
    }
}

impl iter::FusedIterator for Iter<'_> {}
