use std::borrow::{Borrow, Cow};
use std::fmt;
use std::fmt::Formatter;
use std::hash::Hash;
use std::iter::FusedIterator;
use std::ops::{Index, IndexMut};

use crate::{List, Value};

/// A map type with [`String`] keys and [`Value`] values.
#[derive(Clone, Default)]
pub struct Compound<S = String> {
    map: Map<S>,
}

#[cfg(not(feature = "preserve_order"))]
type Map<S> = std::collections::BTreeMap<S, Value<S>>;

#[cfg(feature = "preserve_order")]
type Map<S> = indexmap::IndexMap<S, Value<S>>;

impl<S: fmt::Debug> fmt::Debug for Compound<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.map.fmt(f)
    }
}

impl<S> PartialEq for Compound<S>
where
    S: Ord + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

#[cfg(feature = "serde")]
impl<Str> serde::Serialize for Compound<Str>
where
    Str: Ord + Hash + serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.map.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, S> serde::Deserialize<'de> for Compound<S>
where
    S: Ord + Hash + serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Map::<S>::deserialize(deserializer).map(|map| Self { map })
    }

    fn deserialize_in_place<D>(deserializer: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Map::<S>::deserialize_in_place(deserializer, &mut place.map)
    }
}

impl<S> Compound<S> {
    pub fn new() -> Self {
        Self { map: Map::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            #[cfg(not(feature = "preserve_order"))]
            map: {
                // BTreeMap does not have with_capacity.
                let _ = cap;
                Map::new()
            },
            #[cfg(feature = "preserve_order")]
            map: Map::with_capacity(cap),
        }
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }
}

macro_rules! query_funcs {
    (
        $($(#[$attr:meta])* pub fn $func_name:ident <Q>(($($self:tt)+), $key_name:ident: &Q) -> $return_type:ty
        where $string_type:ty, Q: blah
        $content:block)*
    ) => {
        $(
            $(#[$attr])* pub fn $func_name<Q>($($self)+, $key_name: &Q) -> $return_type
            where
                Q: ?Sized + AsBorrowed<$string_type>,
                <Q as AsBorrowed<$string_type>>::Borrowed: Hash + Ord,
                $string_type: Borrow<<Q as AsBorrowed<$string_type>>::Borrowed>,
            $content
        )*
    };
}

impl<S> Compound<S>
where
    S: Ord + Hash,
{
    query_funcs! {
        pub fn get<Q>((&self), k: &Q) -> Option<&Value<S>>
        where S, Q: blah
        {
            self.map.get(k.as_borrowed())
        }

        pub fn contains_key<Q>((&self), k: &Q) -> bool
        where S, Q: blah
        {
            self.map.contains_key(k.as_borrowed())
        }

        pub fn get_mut<Q>((&mut self), k: &Q) -> Option<&mut Value<S>>
        where S, Q: blah
        {
            self.map.get_mut(k.as_borrowed())
        }

        pub fn get_key_value<Q>((&self), k: &Q) -> Option<(&S, &Value<S>)>
        where S, Q: blah
        {
            self.map.get_key_value(k.as_borrowed())
        }

        pub fn remove<Q>((&mut self), k: &Q) -> Option<Value<S>>
        where S, Q: blah
        {
            #[cfg(feature = "preserve_order")]
            return self.swap_remove(k);
            #[cfg(not(feature = "preserve_order"))]
            return self.map.remove(k.as_borrowed());
        }

        #[cfg(feature = "preserve_order")]
        pub fn swap_remove<Q>((&mut self), k: &Q) -> Option<Value<S>>
        where S, Q: blah
        {
            self.map.swap_remove(k.as_borrowed())
        }

        #[cfg(feature = "preserve_order")]
        pub fn shift_remove<Q>((&mut self), k: &Q) -> Option<Value<S>>
        where S, Q: blah
        {
            self.map.shift_remove(k.as_borrowed())
        }

        pub fn get_i8<Q>((&self), k: &Q) -> Option<i8>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_i8())
        }

        pub fn get_i16<Q>((&self), k: &Q) -> Option<i16>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_i16())
        }

        pub fn get_i32<Q>((&self), k: &Q) -> Option<i32>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_i32())
        }

        pub fn get_i64<Q>((&self), k: &Q) -> Option<i64>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_i64())
        }

        pub fn get_f32<Q>((&self), k: &Q) -> Option<f32>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_f32())
        }

        pub fn get_f64<Q>((&self), k: &Q) -> Option<f64>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_f64())
        }

        pub fn get_byte_array<Q>((&self), k: &Q) -> Option<&[i8]>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_byte_array())
        }

        pub fn get_compound<Q>((&self), k: &Q) -> Option<&Compound<S>>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_compound())
        }

        pub fn get_list<Q>((&self), k: &Q) -> Option<&List<S>>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_list())
        }

        pub fn get_string<Q>((&self), k: &Q) -> Option<&S>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_string())
        }

        pub fn get_int_array<Q>((&self), k: &Q) -> Option<&[i32]>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_int_array())
        }

        pub fn get_long_array<Q>((&self), k: &Q) -> Option<&[i64]>
        where S, Q: blah
        {
            self.get(k).and_then(|o| o.as_long_array())
        }

        pub fn get_compound_list<Q>((&self), k: &Q) -> Option<&[Compound<S>]>
        where S, Q: blah
        {
            self.get_list(k).and_then(|l| l.as_compounds())
        }

        pub fn get_string_list<Q>((&self), k: &Q) -> Option<&[S]>
        where S, Q: blah
        {
            self.get_list(k).and_then(|l| l.as_strings())
        }

        pub fn get_byte_array_mut<Q>((&mut self), k: &Q) -> Option<&mut Vec<i8>>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_byte_array_mut())
        }

        pub fn get_compound_mut<Q>((&mut self), k: &Q) -> Option<&mut Compound<S>>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_compound_mut())
        }

        pub fn get_list_mut<Q>((&mut self), k: &Q) -> Option<&mut List<S>>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_list_mut())
        }

        pub fn get_string_mut<Q>((&mut self), k: &Q) -> Option<&mut S>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_string_mut())
        }

        pub fn get_int_array_mut<Q>((&mut self), k: &Q) -> Option<&mut Vec<i32>>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_int_array_mut())
        }

        pub fn get_long_array_mut<Q>((&mut self), k: &Q) -> Option<&mut Vec<i64>>
        where S, Q: blah
        {
            self.get_mut(k).and_then(|o| o.as_long_array_mut())
        }

        pub fn get_compound_list_mut<Q>((&mut self), k: &Q) -> Option<&mut [Compound<S>]>
        where S, Q: blah
        {
            self.get_list_mut(k).and_then(|l| l.as_compounds_mut())
        }

        pub fn get_string_list_mut<Q>((&mut self), k: &Q) -> Option<&mut [S]>
        where S, Q: blah
        {
            self.get_list_mut(k).and_then(|l| l.as_strings_mut())
        }

        pub fn remove_i8<Q>((&mut self), k: &Q) -> Option<i8>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_i8())
        }

        pub fn remove_i16<Q>((&mut self), k: &Q) -> Option<i16>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_i16())
        }

        pub fn remove_i32<Q>((&mut self), k: &Q) -> Option<i32>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_i32())
        }

        pub fn remove_i64<Q>((&mut self), k: &Q) -> Option<i64>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_i64())
        }

        pub fn remove_f32<Q>((&mut self), k: &Q) -> Option<f32>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_f32())
        }

        pub fn remove_f64<Q>((&mut self), k: &Q) -> Option<f64>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.as_f64())
        }

        pub fn remove_byte_array<Q>((&mut self), k: &Q) -> Option<Vec<i8>>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_byte_array())
        }

        pub fn remove_compound<Q>((&mut self), k: &Q) -> Option<Compound<S>>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_compound())
        }

        pub fn remove_list<Q>((&mut self), k: &Q) -> Option<List<S>>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_list())
        }

        pub fn remove_string<Q>((&mut self), k: &Q) -> Option<S>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_string())
        }

        pub fn remove_int_array<Q>((&mut self), k: &Q) -> Option<Vec<i32>>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_int_array())
        }

        pub fn remove_long_array<Q>((&mut self), k: &Q) -> Option<Vec<i64>>
        where S, Q: blah
        {
            self.remove(k).and_then(|o| o.into_long_array())
        }

        pub fn remove_compound_list<Q>((&mut self), k: &Q) -> Option<Vec<Compound<S>>>
        where S, Q: blah
        {
            self.remove_list(k).and_then(|l| l.into_compounds())
        }

        pub fn remove_string_list<Q>((&mut self), k: &Q) -> Option<Vec<S>>
        where S, Q: blah
        {
            self.remove_list(k).and_then(|l| l.into_strings())
        }
    }

    pub fn insert<K, V>(&mut self, k: K, v: V) -> Option<Value<S>>
    where
        K: Into<S>,
        V: Into<Value<S>>,
    {
        self.map.insert(k.into(), v.into())
    }

    pub fn remove_entry<Q>(&mut self, k: &Q) -> Option<(S, Value<S>)>
    where
        S: Borrow<Q>,
        Q: ?Sized + Ord + Hash,
    {
        #[cfg(feature = "preserve_order")]
        return self.swap_remove_entry(k);
        #[cfg(not(feature = "preserve_order"))]
        return self.map.remove_entry(k);
    }

    #[cfg(feature = "preserve_order")]
    pub fn swap_remove_entry<Q>(&mut self, k: &Q) -> Option<(S, Value<S>)>
    where
        S: Borrow<Q>,
        Q: ?Sized + Ord + Hash,
    {
        self.map.swap_remove_entry(k)
    }

    #[cfg(feature = "preserve_order")]
    pub fn shift_remove_entry<Q>(&mut self, k: &Q) -> Option<(S, Value<S>)>
    where
        S: Borrow<Q>,
        Q: ?Sized + Ord + Hash,
    {
        self.map.shift_remove_entry(k)
    }

    pub fn append(&mut self, other: &mut Self) {
        #[cfg(not(feature = "preserve_order"))]
        self.map.append(&mut other.map);

        #[cfg(feature = "preserve_order")]
        for (k, v) in std::mem::take(&mut other.map) {
            self.map.insert(k, v);
        }
    }

    pub fn entry<K>(&mut self, k: K) -> Entry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => Entry::Vacant(VacantEntry { entry: ve }),
            EntryImpl::Occupied(oe) => Entry::Occupied(OccupiedEntry { entry: oe }),
        }
    }

    pub fn compound_entry<K>(&mut self, k: K) -> CompoundEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => CompoundEntry::Vacant(VacantCompoundEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_compound().is_some() => {
                // SAFETY: we just verified that the entry is a compound
                CompoundEntry::Occupied(unsafe { OccupiedCompoundEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => CompoundEntry::WrongType(WrongTypeCompoundEntry::new(oe)),
        }
    }

    pub fn list_entry<K>(&mut self, k: K) -> ListEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => ListEntry::Vacant(VacantListEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_list().is_some() => {
                // SAFETY: we just verified that the entry is a list
                ListEntry::Occupied(unsafe { OccupiedListEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => ListEntry::WrongType(WrongTypeListEntry::new(oe)),
        }
    }

    pub fn string_entry<K>(&mut self, k: K) -> StringEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => StringEntry::Vacant(VacantStringEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_string().is_some() => {
                // SAFETY: we just verified that the entry is a string
                StringEntry::Occupied(unsafe { OccupiedStringEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => StringEntry::WrongType(WrongTypeStringEntry::new(oe)),
        }
    }

    pub fn byte_array_entry<K>(&mut self, k: K) -> ByteArrayEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => ByteArrayEntry::Vacant(VacantByteArrayEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_byte_array().is_some() => {
                // SAFETY: we just verified that the entry is a byte array
                ByteArrayEntry::Occupied(unsafe { OccupiedByteArrayEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => ByteArrayEntry::WrongType(WrongTypeByteArrayEntry::new(oe)),
        }
    }

    pub fn int_array_entry<K>(&mut self, k: K) -> IntArrayEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => IntArrayEntry::Vacant(VacantIntArrayEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_int_array().is_some() => {
                // SAFETY: we just verified that the entry is an int array
                IntArrayEntry::Occupied(unsafe { OccupiedIntArrayEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => IntArrayEntry::WrongType(WrongTypeIntArrayEntry::new(oe)),
        }
    }

    pub fn long_array_entry<K>(&mut self, k: K) -> LongArrayEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => LongArrayEntry::Vacant(VacantLongArrayEntry::new(ve)),
            EntryImpl::Occupied(oe) if oe.get().as_long_array().is_some() => {
                // SAFETY: we just verified that the entry is a long array
                LongArrayEntry::Occupied(unsafe { OccupiedLongArrayEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => LongArrayEntry::WrongType(WrongTypeLongArrayEntry::new(oe)),
        }
    }

    pub fn compound_list_entry<K>(&mut self, k: K) -> CompoundListEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => CompoundListEntry::Vacant(VacantCompoundListEntry::new(ve)),
            EntryImpl::Occupied(oe) if matches!(oe.get(), Value::List(List::Compound(_))) => {
                // SAFETY: we just verified that the entry is a compound list (and not List::End)
                CompoundListEntry::Occupied(unsafe { OccupiedCompoundListEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => {
                CompoundListEntry::WrongType(WrongTypeCompoundListEntry::new(oe))
            }
        }
    }

    pub fn string_list_entry<K>(&mut self, k: K) -> StringListEntry<S>
    where
        K: Into<S>,
    {
        #[cfg(not(feature = "preserve_order"))]
        use std::collections::btree_map::Entry as EntryImpl;

        #[cfg(feature = "preserve_order")]
        use indexmap::map::Entry as EntryImpl;

        match self.map.entry(k.into()) {
            EntryImpl::Vacant(ve) => StringListEntry::Vacant(VacantStringListEntry::new(ve)),
            EntryImpl::Occupied(oe) if matches!(oe.get(), Value::List(List::String(_))) => {
                // SAFETY: we just verified that the entry is a string list (and not List::End)
                StringListEntry::Occupied(unsafe { OccupiedStringListEntry::new(oe) })
            }
            EntryImpl::Occupied(oe) => {
                StringListEntry::WrongType(WrongTypeStringListEntry::new(oe))
            }
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn iter(&self) -> Iter<S> {
        Iter {
            iter: self.map.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<S> {
        IterMut {
            iter: self.map.iter_mut(),
        }
    }

    pub fn keys(&self) -> Keys<S> {
        Keys {
            iter: self.map.keys(),
        }
    }

    pub fn values(&self) -> Values<S> {
        Values {
            iter: self.map.values(),
        }
    }

    pub fn values_mut(&mut self) -> ValuesMut<S> {
        ValuesMut {
            iter: self.map.values_mut(),
        }
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&S, &mut Value<S>) -> bool,
    {
        self.map.retain(f)
    }

    /// Inserts all items from `other` into `self` recursively.
    ///
    /// # Example
    ///
    /// ```
    /// use valence_nbt::compound;
    ///
    /// let mut this = compound! {
    ///     "foo" => 10,
    ///     "bar" => compound! {
    ///         "baz" => 20,
    ///     }
    /// };
    ///
    /// let other = compound! {
    ///     "foo" => 15,
    ///     "bar" => compound! {
    ///         "quux" => "hello",
    ///     }
    /// };
    ///
    /// this.merge(other);
    ///
    /// assert_eq!(
    ///     this,
    ///     compound! {
    ///         "foo" => 15,
    ///         "bar" => compound! {
    ///             "baz" => 20,
    ///             "quux" => "hello",
    ///         }
    ///     }
    /// );
    /// ```
    pub fn merge(&mut self, other: Compound<S>) {
        for (k, v) in other {
            match (self.entry(k), v) {
                (Entry::Occupied(mut oe), Value::Compound(other)) => {
                    if let Value::Compound(this) = oe.get_mut() {
                        // Insert compound recursively.
                        this.merge(other);
                    }
                }
                (Entry::Occupied(mut oe), value) => {
                    oe.insert(value);
                }
                (Entry::Vacant(ve), value) => {
                    ve.insert(value);
                }
            }
        }
    }
}

/// Trait that can be used as a key to query a compound. Basically something
/// that can be converted to a type `B` such that `S: Borrow<B>`.
pub trait AsBorrowed<S> {
    type Borrowed: ?Sized;

    fn as_borrowed(&self) -> &Self::Borrowed;
}

impl<Q: ?Sized> AsBorrowed<String> for Q
where
    String: Borrow<Q>,
{
    type Borrowed = Q;

    #[inline]
    fn as_borrowed(&self) -> &Q {
        self
    }
}

impl<'a, Q: ?Sized> AsBorrowed<Cow<'a, str>> for Q
where
    Cow<'a, str>: Borrow<Q>,
{
    type Borrowed = Q;

    #[inline]
    fn as_borrowed(&self) -> &Q {
        self
    }
}

#[cfg(feature = "java_string")]
impl<Q: ?Sized> AsBorrowed<java_string::JavaString> for Q
where
    for<'a> &'a Q: Into<&'a java_string::JavaStr>,
{
    type Borrowed = java_string::JavaStr;

    fn as_borrowed(&self) -> &Self::Borrowed {
        self.into()
    }
}

#[cfg(feature = "java_string")]
impl<Q: ?Sized> AsBorrowed<Cow<'_, java_string::JavaStr>> for Q
where
    for<'a> &'a Q: Into<&'a java_string::JavaStr>,
{
    type Borrowed = java_string::JavaStr;

    fn as_borrowed(&self) -> &Self::Borrowed {
        self.into()
    }
}

impl<S> Extend<(S, Value<S>)> for Compound<S>
where
    S: Ord + Hash,
{
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (S, Value<S>)>,
    {
        self.map.extend(iter)
    }
}

impl<S> FromIterator<(S, Value<S>)> for Compound<S>
where
    S: Ord + Hash,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (S, Value<S>)>,
    {
        Self {
            map: Map::from_iter(iter),
        }
    }
}

pub enum Entry<'a, S = String> {
    Vacant(VacantEntry<'a, S>),
    Occupied(OccupiedEntry<'a, S>),
}

impl<'a, S> Entry<'a, S>
where
    S: Hash + Ord,
{
    pub fn key(&self) -> &S {
        match self {
            Entry::Vacant(ve) => ve.key(),
            Entry::Occupied(oe) => oe.key(),
        }
    }

    pub fn or_insert<V: Into<Value<S>>>(self, default: V) -> &'a mut Value<S> {
        match self {
            Entry::Vacant(ve) => ve.insert(default),
            Entry::Occupied(oe) => oe.into_mut(),
        }
    }

    pub fn or_insert_with<F, V>(self, default: F) -> &'a mut Value<S>
    where
        F: FnOnce() -> V,
        V: Into<Value<S>>,
    {
        match self {
            Entry::Vacant(ve) => ve.insert(default()),
            Entry::Occupied(oe) => oe.into_mut(),
        }
    }

    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut Value<S>),
    {
        match self {
            Entry::Vacant(ve) => Entry::Vacant(ve),
            Entry::Occupied(mut oe) => {
                f(oe.get_mut());
                Entry::Occupied(oe)
            }
        }
    }
}

impl<S> fmt::Debug for Entry<'_, S>
where
    S: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Vacant(entry) => f.debug_tuple("Vacant").field(entry).finish(),
            Self::Occupied(entry) => f.debug_tuple("Occupied").field(entry).finish(),
        }
    }
}

pub struct VacantEntry<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    entry: std::collections::btree_map::VacantEntry<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    entry: indexmap::map::VacantEntry<'a, S, Value<S>>,
}

impl<'a, S> VacantEntry<'a, S>
where
    S: Ord + Hash,
{
    pub fn key(&self) -> &S {
        self.entry.key()
    }

    pub fn insert<V: Into<Value<S>>>(self, v: V) -> &'a mut Value<S> {
        self.entry.insert(v.into())
    }
}

impl<S> fmt::Debug for VacantEntry<'_, S>
where
    S: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VacantEntry")
            .field("entry", &self.entry)
            .finish()
    }
}

pub struct OccupiedEntry<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    entry: std::collections::btree_map::OccupiedEntry<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    entry: indexmap::map::OccupiedEntry<'a, S, Value<S>>,
}

impl<'a, S> OccupiedEntry<'a, S>
where
    S: Hash + Ord,
{
    pub fn key(&self) -> &S {
        self.entry.key()
    }

    pub fn get(&self) -> &Value<S> {
        self.entry.get()
    }

    pub fn get_mut(&mut self) -> &mut Value<S> {
        self.entry.get_mut()
    }

    pub fn into_mut(self) -> &'a mut Value<S> {
        self.entry.into_mut()
    }

    pub fn insert<V: Into<Value<S>>>(&mut self, v: V) -> Value<S> {
        self.entry.insert(v.into())
    }

    pub fn remove(self) -> Value<S> {
        #[cfg(feature = "preserve_order")]
        return self.swap_remove();
        #[cfg(not(feature = "preserve_order"))]
        return self.entry.remove();
    }

    #[cfg(feature = "preserve_order")]
    pub fn swap_remove(self) -> Value<S> {
        self.entry.swap_remove()
    }

    #[cfg(feature = "preserve_order")]
    pub fn shift_remove(self) -> Value<S> {
        self.entry.shift_remove()
    }
}

impl<S> fmt::Debug for OccupiedEntry<'_, S>
where
    S: fmt::Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedEntry")
            .field("entry", &self.entry)
            .finish()
    }
}

macro_rules! typed_entry {
    (
        $name:ident,
        $vacant_name:ident,
        $wrong_type_name:ident,
        $occupied_name:ident,
        ($($expected_type:tt)+),
        ($($expected_type_immutable:tt)+),
        $downcast:expr,
        $downcast_mut:expr,
        $downcast_owned:expr,
        $upcast:expr,
    ) => {
        pub enum $name<'a, S = String> {
            Vacant($vacant_name<'a, S>),
            WrongType($wrong_type_name<'a, S>),
            Occupied($occupied_name<'a, S>),
        }

        impl<'a, S> $name<'a, S>
        where
            S: Hash + Ord,
        {
            pub fn key(&self) -> &S {
                match self {
                    $name::Vacant(ve) => ve.key(),
                    $name::WrongType(wte) => wte.key(),
                    $name::Occupied(oe) => oe.key(),
                }
            }

            pub fn or_insert<V: Into<$($expected_type)+>>(self, default: V) -> &'a mut $($expected_type)+ {
                match self {
                    $name::Vacant(ve) => ve.insert(default),
                    $name::WrongType(wte) => wte.insert(default),
                    $name::Occupied(oe) => oe.into_mut(),
                }
            }

            pub fn or_insert_with<F, V>(self, default: F) -> &'a mut $($expected_type)+
            where
                F: FnOnce() -> V,
                V: Into<$($expected_type)+>,
            {
                match self {
                    $name::Vacant(ve) => ve.insert(default()),
                    $name::WrongType(wte) => wte.insert(default()),
                    $name::Occupied(oe) => oe.into_mut(),
                }
            }

            pub fn or_default(self) -> &'a mut $($expected_type)+
            where
                S: Default
            {
                self.or_insert_with::<_, $($expected_type)+>(Default::default)
            }

            pub fn and_modify<F>(self, f: F) -> Self
            where
                F: FnOnce(&mut $($expected_type)+),
            {
                match self {
                    $name::Occupied(mut oe) => {
                        f(oe.get_mut());
                        $name::Occupied(oe)
                    }
                    _ => self,
                }
            }
        }

        impl<S> fmt::Debug for $name<'_, S>
        where
            S: fmt::Debug + Ord,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::Vacant(entry) => f.debug_tuple("Vacant").field(entry).finish(),
                    Self::WrongType(entry) => f.debug_tuple("WrongType").field(entry).finish(),
                    Self::Occupied(entry) => f.debug_tuple("Occupied").field(entry).finish(),
                }
            }
        }

        pub struct $vacant_name<'a, S = String> {
            #[cfg(not(feature = "preserve_order"))]
            entry: std::collections::btree_map::VacantEntry<'a, S, Value<S>>,
            #[cfg(feature = "preserve_order")]
            entry: indexmap::map::VacantEntry<'a, S, Value<S>>,
        }

        impl<'a, S> $vacant_name<'a, S>
        where
            S: Ord + Hash,
        {
            #[cfg(not(feature = "preserve_order"))]
            fn new(entry: std::collections::btree_map::VacantEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            #[cfg(feature = "preserve_order")]
            fn new(entry: indexmap::map::VacantEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            pub fn key(&self) -> &S {
                self.entry.key()
            }

            pub fn insert<V: Into<$($expected_type)+>>(self, v: V) -> &'a mut $($expected_type)+{
                let downcasted = $downcast_mut(self.entry.insert($upcast(v.into())));
                // SAFETY: we just inserted a value of the corresponding type
                unsafe { downcasted.unwrap_unchecked() }
            }
        }

        impl<S> fmt::Debug for $vacant_name<'_, S>
        where
            S: fmt::Debug + Ord,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($vacant_name))
                    .field("entry", &self.entry)
                    .finish()
            }
        }

        pub struct $wrong_type_name<'a, S = String> {
            #[cfg(not(feature = "preserve_order"))]
            entry: std::collections::btree_map::OccupiedEntry<'a, S, Value<S>>,
            #[cfg(feature = "preserve_order")]
            entry: indexmap::map::OccupiedEntry<'a, S, Value<S>>,
        }

        impl<'a, S> $wrong_type_name<'a, S>
        where
            S: Ord + Hash,
        {
            #[cfg(not(feature = "preserve_order"))]
            fn new(entry: std::collections::btree_map::OccupiedEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            #[cfg(feature = "preserve_order")]
            fn new(entry: indexmap::map::OccupiedEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            pub fn key(&self) -> &S {
                self.entry.key()
            }

            pub fn get(&self) -> &Value<S> {
                self.entry.get()
            }

            pub fn get_mut(&mut self) -> &mut Value<S> {
                self.entry.get_mut()
            }

            pub fn insert_value<V: Into<Value<S>>>(&mut self, v: V) -> Value<S> {
                self.entry.insert(v.into())
            }

            pub fn insert<V: Into<$($expected_type)+>>(mut self, v: V) -> &'a mut $($expected_type)+ {
                self.entry.insert($upcast(v.into()));
                let downcasted = $downcast_mut(self.entry.into_mut());
                // SAFETY: we just inserted a value of the corresponding type
                unsafe { downcasted.unwrap_unchecked() }
            }

            pub fn remove(self) -> Value<S> {
                #[cfg(feature = "preserve_order")]
                return self.entry.swap_remove();
                #[cfg(not(feature = "preserve_order"))]
                return self.entry.remove();
            }

            #[cfg(feature = "preserve_order")]
            pub fn swap_remove(self) -> Value<S> {
                self.entry.swap_remove()
            }

            #[cfg(feature = "preserve_order")]
            pub fn shift_remove(self) -> Value<S> {
                self.entry.shift_remove()
            }
        }

        impl<S> fmt::Debug for $wrong_type_name<'_, S>
        where
            S: fmt::Debug + Ord
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($wrong_type_name))
                    .field("entry", &self.entry)
                    .finish()
            }
        }

        pub struct $occupied_name<'a, S = String> {
            #[cfg(not(feature = "preserve_order"))]
            entry: std::collections::btree_map::OccupiedEntry<'a, S, Value<S>>,
            #[cfg(feature = "preserve_order")]
            entry: indexmap::map::OccupiedEntry<'a, S, Value<S>>,
        }

        impl<'a, S> $occupied_name<'a, S>
        where
            S: Hash + Ord,
        {
            /// # Safety
            /// The input entry must contain the expected type of this struct
            #[cfg(not(feature = "preserve_order"))]
            unsafe fn new(entry: std::collections::btree_map::OccupiedEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            /// # Safety
            /// The input entry must contain the expected type of this struct
            #[cfg(feature = "preserve_order")]
            unsafe fn new(entry: indexmap::map::OccupiedEntry<'a, S, Value<S>>) -> Self {
                Self {
                    entry,
                }
            }

            pub fn key(&self) -> &S {
                self.entry.key()
            }

            pub fn get(&self) -> &$($expected_type_immutable)+ {
                let downcasted = $downcast(self.entry.get());
                // SAFETY: invariant of this struct is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            pub fn get_mut(&mut self) -> &mut $($expected_type)+ {
                let downcasted = $downcast_mut(self.entry.get_mut());
                // SAFETY: invariant of this struct is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            pub fn into_mut(self) -> &'a mut $($expected_type)+ {
                let downcasted = $downcast_mut(self.entry.into_mut());
                // SAFETY: invariant of this struct is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            pub fn insert<V: Into<$($expected_type)+>>(&mut self, v: V) -> $($expected_type)+ {
                let downcasted = $downcast_owned(self.entry.insert($upcast(v.into())));
                // SAFETY: invariant of this type is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            pub fn remove(self) -> $($expected_type)+ {
                #[cfg(feature = "preserve_order")]
                let downcasted = $downcast_owned(self.entry.swap_remove());
                #[cfg(not(feature = "preserve_order"))]
                let downcasted = $downcast_owned(self.entry.remove());
                // SAFETY: invariant of this type is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            #[cfg(feature = "preserve_order")]
            pub fn swap_remove(self) -> $($expected_type)+ {
                let downcasted = $downcast_owned(self.entry.swap_remove());
                // SAFETY: invariant of this type is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }

            #[cfg(feature = "preserve_order")]
            pub fn shift_remove(self) -> $($expected_type)+ {
                let downcasted = $downcast_owned(self.entry.shift_remove());
                // SAFETY: invariant of this type is that the value is of the expected type
                unsafe { downcasted.unwrap_unchecked() }
            }
        }

        impl<S> fmt::Debug for $occupied_name<'_, S>
        where
            S: fmt::Debug + Ord,
        {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($occupied_name))
                    .field("entry", &self.entry)
                    .finish()
            }
        }
    };
}

typed_entry!(
    CompoundEntry,
    VacantCompoundEntry,
    WrongTypeCompoundEntry,
    OccupiedCompoundEntry,
    (Compound<S>),
    (Compound<S>),
    Value::as_compound,
    Value::as_compound_mut,
    Value::into_compound,
    Value::Compound,
);
typed_entry!(
    ListEntry,
    VacantListEntry,
    WrongTypeListEntry,
    OccupiedListEntry,
    (List<S>),
    (List<S>),
    Value::as_list,
    Value::as_list_mut,
    Value::into_list,
    Value::List,
);
typed_entry!(
    StringEntry,
    VacantStringEntry,
    WrongTypeStringEntry,
    OccupiedStringEntry,
    (S),
    (S),
    Value::as_string,
    Value::as_string_mut,
    Value::into_string,
    Value::String,
);
typed_entry!(
    ByteArrayEntry,
    VacantByteArrayEntry,
    WrongTypeByteArrayEntry,
    OccupiedByteArrayEntry,
    (Vec<i8>),
    ([i8]),
    Value::as_byte_array,
    Value::as_byte_array_mut,
    Value::into_byte_array,
    Value::ByteArray,
);
typed_entry!(
    IntArrayEntry,
    VacantIntArrayEntry,
    WrongTypeIntArrayEntry,
    OccupiedIntArrayEntry,
    (Vec<i32>),
    ([i32]),
    Value::as_int_array,
    Value::as_int_array_mut,
    Value::into_int_array,
    Value::IntArray,
);
typed_entry!(
    LongArrayEntry,
    VacantLongArrayEntry,
    WrongTypeLongArrayEntry,
    OccupiedLongArrayEntry,
    (Vec<i64>),
    ([i64]),
    Value::as_long_array,
    Value::as_long_array_mut,
    Value::into_long_array,
    Value::LongArray,
);

fn downcast_compound_list<S>(v: &Value<S>) -> Option<&[Compound<S>]> {
    v.as_list().and_then(|o| o.as_compounds())
}
fn downcast_compound_list_mut<S>(v: &mut Value<S>) -> Option<&mut Vec<Compound<S>>> {
    match v {
        Value::List(List::Compound(list)) => Some(list),
        _ => None,
    }
}
typed_entry!(
    CompoundListEntry,
    VacantCompoundListEntry,
    WrongTypeCompoundListEntry,
    OccupiedCompoundListEntry,
    (Vec<Compound<S>>),
    ([Compound<S>]),
    downcast_compound_list,
    downcast_compound_list_mut,
    |v: Value<S>| v.into_list().and_then(|o| o.into_compounds()),
    |v| Value::List(List::Compound(v)),
);

fn downcast_string_list<S>(v: &Value<S>) -> Option<&[S]> {
    v.as_list().and_then(|o| o.as_strings())
}
fn downcast_string_list_mut<S>(v: &mut Value<S>) -> Option<&mut Vec<S>> {
    match v {
        Value::List(List::String(list)) => Some(list),
        _ => None,
    }
}
typed_entry!(
    StringListEntry,
    VacantStringListEntry,
    WrongTypeStringListEntry,
    OccupiedStringListEntry,
    (Vec<S>),
    ([S]),
    downcast_string_list,
    downcast_string_list_mut,
    |v: Value<S>| v.into_list().and_then(|o| o.into_strings()),
    |v| Value::List(List::String(v)),
);

impl<S, Q> Index<&'_ Q> for Compound<S>
where
    S: Borrow<Q> + Ord + Hash,
    Q: ?Sized + Ord + Hash,
{
    type Output = Value<S>;

    fn index(&self, index: &Q) -> &Self::Output {
        self.map.index(index)
    }
}

impl<S, Q> IndexMut<&'_ Q> for Compound<S>
where
    S: Borrow<Q> + Hash + Ord,
    Q: ?Sized + Ord + Hash,
{
    fn index_mut(&mut self, index: &Q) -> &mut Self::Output {
        self.map.get_mut(index).expect("no entry found for key")
    }
}

macro_rules! impl_iterator_traits {
    (($name:ident $($generics:tt)*) => $item:ty) => {
        impl $($generics)* Iterator for $name $($generics)* {
            type Item = $item;
            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next()
            }
            #[inline]
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }
        }

        #[cfg(feature = "preserve_order")]
        impl $($generics)* DoubleEndedIterator for $name $($generics)* {
            #[inline]
            fn next_back(&mut self) -> Option<Self::Item> {
                self.iter.next_back()
            }
        }

        impl $($generics)* ExactSizeIterator for $name $($generics)* {
            #[inline]
            fn len(&self) -> usize {
                self.iter.len()
            }
        }

        impl $($generics)* FusedIterator for $name $($generics)* {}
    }
}

impl<'a, S> IntoIterator for &'a Compound<S> {
    type Item = (&'a S, &'a Value<S>);
    type IntoIter = Iter<'a, S>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            iter: self.map.iter(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Iter<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::Iter<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::Iter<'a, S, Value<S>>,
}

impl_iterator_traits!((Iter<'a, S>) => (&'a S, &'a Value<S>));

impl<'a, S> IntoIterator for &'a mut Compound<S> {
    type Item = (&'a S, &'a mut Value<S>);
    type IntoIter = IterMut<'a, S>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            iter: self.map.iter_mut(),
        }
    }
}

#[derive(Debug)]
pub struct IterMut<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::IterMut<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::IterMut<'a, S, Value<S>>,
}

impl_iterator_traits!((IterMut<'a, S>) => (&'a S, &'a mut Value<S>));

impl<S> IntoIterator for Compound<S> {
    type Item = (S, Value<S>);
    type IntoIter = IntoIter<S>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            iter: self.map.into_iter(),
        }
    }
}

#[derive(Debug)]
pub struct IntoIter<S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::IntoIter<S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::IntoIter<S, Value<S>>,
}

impl_iterator_traits!((IntoIter<S>) => (S, Value<S>));

#[derive(Clone, Debug)]
pub struct Keys<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::Keys<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::Keys<'a, S, Value<S>>,
}

impl_iterator_traits!((Keys<'a, S>) => &'a S);

#[derive(Clone, Debug)]
pub struct Values<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::Values<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::Values<'a, S, Value<S>>,
}

impl_iterator_traits!((Values<'a, S>) => &'a Value<S>);

#[derive(Debug)]
pub struct ValuesMut<'a, S = String> {
    #[cfg(not(feature = "preserve_order"))]
    iter: std::collections::btree_map::ValuesMut<'a, S, Value<S>>,
    #[cfg(feature = "preserve_order")]
    iter: indexmap::map::ValuesMut<'a, S, Value<S>>,
}

impl_iterator_traits!((ValuesMut<'a, S>) => &'a mut Value<S>);

#[cfg(test)]
mod tests {
    #[cfg(feature = "preserve_order")]
    #[test]
    fn compound_preserves_order() {
        use super::*;

        let letters = ["g", "b", "d", "e", "h", "z", "m", "a", "q"];

        let mut c = Compound::<String>::new();
        for l in letters {
            c.insert(l, 0_i8);
        }

        for (k, l) in c.keys().zip(letters) {
            assert_eq!(k, l);
        }
    }
}
