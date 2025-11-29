//! "Simple" value interner, a kind of flat [`std::collections::HashSet`].
#![allow(dead_code)]

use std::hash::{BuildHasher, Hash};

use hashbrown::{HashMap, hash_map::RawEntryMut};

use super::{Intern, Opaque, make_hash};

#[derive(Clone)]
pub struct SimpleInterner<Id, T, S = rustc_hash::FxBuildHasher> {
    hasher: S,
    map: HashMap<Opaque<Id>, (), ()>,
    values: Vec<T>,
}

impl<Id: Intern, T, S: Default> Default for SimpleInterner<Id, T, S> {
    fn default() -> Self {
        Self::with_hasher(Default::default())
    }
}

impl<Id: Intern, T> SimpleInterner<Id, T> {
    /// Creates a new empty interner.
    pub fn new() -> Self {
        Default::default()
    }

    /// Creates an empty interner with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_hasher_and_capacity(Default::default(), capacity)
    }
}

impl<Id: Intern, T, S> SimpleInterner<Id, T, S> {
    /// Creates an empty interner which will use the given hash builder.
    pub fn with_hasher(hasher: S) -> Self {
        SimpleInterner::with_hasher_and_capacity(hasher, Default::default())
    }

    /// Creates an empty interner with the specified capacity and hash builder.
    pub fn with_hasher_and_capacity(hasher: S, capacity: usize) -> Self {
        SimpleInterner {
            hasher,
            map: HashMap::with_capacity_and_hasher(capacity, ()),
            values: Vec::with_capacity(capacity),
        }
    }

    /// The number of uniquely interned strings.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true if the interner has no elements.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Returns the string associated with the given symbol.
    pub fn get(&self, s: Id) -> Option<&T> {
        self.values.get(s.index() as usize)
    }
}

impl<Id: Intern + Copy, T: Hash + PartialEq, S: BuildHasher> SimpleInterner<Id, T, S> {
    /// Gets the interned symbol for the string.
    pub fn find(&self, v: T) -> Option<Id> {
        let hash = make_hash(&self.hasher, &v);
        let entry = self.map.raw_entry().from_hash(hash, |&Opaque(symbol)| {
            let stored = unsafe { index_unchecked!(self.values, symbol.index() as usize) };
            stored == &v
        });

        entry.map(|(&Opaque(symbol), &())| symbol)
    }

    /// Interns the given value.
    pub fn intern(&mut self, v: T) -> Id {
        let hash = make_hash(&self.hasher, &v);
        let entry = self.map.raw_entry_mut().from_hash(hash, |&Opaque(symbol)| {
            let stored = unsafe { index_unchecked!(self.values, symbol.index() as usize) };
            stored == &v
        });

        let (&mut Opaque(symbol), &mut ()) = match entry {
            RawEntryMut::Occupied(entry) => entry.into_key_value(),
            RawEntryMut::Vacant(entry) => {
                let symbol = unsafe { Id::from_index(self.values.len() as u32) };
                self.values.push(v);

                entry.insert_with_hasher(hash, Opaque(symbol), (), |&Opaque(symbol)| {
                    let stored = unsafe { index_unchecked!(self.values, symbol.index() as usize) };
                    make_hash(&self.hasher, &stored)
                })
            }
        };
        symbol
    }
}
