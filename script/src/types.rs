// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

//! Arrays and Dictionaries are implemented as ScriptTypes as mixing 16 byte with 8 byte `Rc<T>`s
//! has a significant impact on `clone` performance.
//! It may be better to have some sort of custom `Array` type that is unsized, but AFAICT that isn't
//! possible (yet?).

use crate::{CallError, CallResult, Environment, ScriptType, VariantType};
use core::cell::{Ref, RefCell};
use core::{fmt, mem};
#[cfg(feature = "fast-dictionary")]
use rustc_hash::FxHashMap;
#[cfg(not(feature = "fast-dictionary"))]
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Array<V>(Rc<RefCell<Vec<V>>>)
where
	V: VariantType;

#[derive(Clone, Debug)]
#[cfg(not(feature = "fast-dictionary"))]
pub struct Dictionary<V>(Rc<RefCell<HashMap<VariantOrd, V>>>)
where
	V: VariantType;
#[cfg(feature = "fast-dictionary")]
pub struct Dictionary<V>(Rc<RefCell<FxHashMap<VariantOrd, V>>>)
where
	V: VariantType;

/// A Variant type with only types that implement Ord and are not interiorly mutable
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum VariantOrd {
	Bool(bool),
	Integer(isize),
	String(Rc<str>),
}

/// An iterator that holds a [`Ref`](core::cell::Ref)
// DO NOT REORDER THE FIELDS: the drop order is important!
struct ArrayIter<'a, V>
where
	V: VariantType,
{
	iter: core::slice::Iter<'a, V>,
	_borrow: Ref<'a, Vec<V>>,
	_array: Array<V>,
}

/// An iterator that holds a [`Ref`](core::cell::Ref)
// DO NOT REORDER THE FIELDS: the drop order is important!
struct DictionaryIter<'a, V>
where
	V: VariantType,
{
	iter: std::collections::hash_map::Keys<'a, VariantOrd, V>,
	_borrow: Ref<'a, HashMap<VariantOrd, V>>,
	_dictionary: Dictionary<V>,
}

macro_rules! borrow {
	($self:ident) => {
		$self
			.0
			.try_borrow()
			.map_err(|_| CallError::AlreadyBorrowed)?
	};
	(mut $self:ident) => {
		$self
			.0
			.try_borrow_mut()
			.map_err(|_| CallError::AlreadyBorrowed)?
	};
}

macro_rules! check_arg_count {
	($args:ident, $count:expr) => {
		if $args.len() != $count {
			return Err(CallError::BadArgumentCount);
		}
	};
}

impl<V> Array<V>
where
	V: VariantType,
{
	pub fn new() -> Self {
		Self(Rc::new(RefCell::new(Vec::new())))
	}

	pub fn with_capacity(n: usize) -> Self {
		Self(Rc::new(RefCell::new(Vec::with_capacity(n))))
	}

	pub(crate) fn with_len(n: usize) -> Self {
		let mut v = Vec::new();
		v.resize(n, V::default());
		Self(Rc::new(RefCell::new(v)))
	}
}

impl<V> ScriptType<V> for Array<V>
where
	V: VariantType,
{
	fn call(&self, function: &str, args: &[V], _: &Environment<V>) -> CallResult<V> {
		match function {
			"len" => {
				check_arg_count!(args, 0);
				Ok(V::new_integer(borrow!(self).len() as isize))
			}
			"push" => {
				check_arg_count!(args, 1);
				borrow!(mut self).push(args[0].clone());
				Ok(V::default())
			}
			// TODO is it fine to default to None?
			"pop" => {
				check_arg_count!(args, 0);
				Ok(borrow!(mut self).pop().unwrap_or(V::default()))
			}
			_ => Err(CallError::UndefinedFunction),
		}
	}

	#[inline]
	fn index(&self, index: &V) -> CallResult<V> {
		if let Ok(v) = index.as_integer() {
			borrow!(self)
				.get(v as usize)
				.cloned()
				.ok_or(CallError::BadArgument)
		} else {
			Err(CallError::BadArgument)
		}
	}

	#[inline]
	fn set_index(&self, index: &V, value: V) -> CallResult<()> {
		if let Ok(v) = index.as_integer() {
			borrow!(mut self)
				.get_mut(v as usize)
				.map(|v| *v = value)
				.ok_or(CallError::BadArgument)
		} else {
			Err(CallError::BadArgument)
		}
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = V>>> {
		let iter = ArrayIter::new(self.clone())?;
		Ok(Box::new(iter))
	}

	#[inline]
	fn to_string(&self) -> String {
		let mut s = "[".to_string();
		let a = self.0.borrow();
		if let Some((last, a)) = a.split_last() {
			for a in a.iter() {
				s.extend(format!("{}, ", a).chars());
			}
			s.extend(format!("{}", last).chars());
		}
		s.push(']');
		s
	}
}

impl<V> Dictionary<V>
where
	V: VariantType,
{
	pub fn new() -> Self {
		Self(Rc::new(RefCell::new(HashMap::with_hasher(
			Default::default(),
		))))
	}

	pub fn with_capacity(n: usize) -> Self {
		Self(Rc::new(RefCell::new(HashMap::with_capacity_and_hasher(
			n,
			Default::default(),
		))))
	}
}

impl<V> ScriptType<V> for Dictionary<V>
where
	V: VariantType,
{
	fn call(&self, function: &str, args: &[V], _: &Environment<V>) -> CallResult<V> {
		match function {
			"len" => {
				check_arg_count!(args, 0);
				Ok(V::new_integer(borrow!(self).len() as isize))
			}
			"insert" => {
				check_arg_count!(args, 2);
				let key = VariantOrd::from_variant(args[0].clone())?;
				let value = args[1].clone();
				Ok(borrow!(mut self).insert(key, value).unwrap_or(V::default()))
			}
			// TODO is it fine to default to None?
			"remove" => {
				check_arg_count!(args, 1);
				let key = VariantOrd::from_variant(args[0].clone())?;
				Ok(borrow!(mut self).remove(&key).unwrap_or(V::default()))
			}
			_ => Err(CallError::UndefinedFunction),
		}
	}

	#[inline]
	fn index(&self, index: &V) -> CallResult<V> {
		// TODO avoid clone somehow
		let key = VariantOrd::from_variant(index.clone())?;
		borrow!(self)
			.get(&key)
			.cloned()
			.ok_or(CallError::BadArgument)
	}

	#[inline]
	fn set_index(&self, index: &V, value: V) -> CallResult<()> {
		let key = VariantOrd::from_variant(index.clone())?;
		borrow!(mut self).insert(key, value);
		Ok(())
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = V>>> {
		let iter = DictionaryIter::new(self.clone())?;
		Ok(Box::new(iter))
	}

	#[inline]
	fn to_string(&self) -> String {
		let mut s = "{".to_string();
		let d = self.0.borrow();
		let last = d.len() - 1;
		for (i, (k, v)) in d.iter().enumerate() {
			s.extend(format!("{:?}: {:?}", k, v).chars());
			if i != last {
				s.extend(", ".chars());
			}
		}
		s.push('}');
		s
	}
}

impl<V> ArrayIter<'_, V>
where
	V: VariantType,
{
	fn new(array: Array<V>) -> CallResult<Self> {
		// SAFETY:
		// The borrow is valid as long as the array isn't dropped
		// The iterator is valid as long as the borrow isn't dropped
		unsafe {
			let borrow = borrow!(array);
			let borrow: Ref<'_, Vec<V>> = mem::transmute(borrow);
			let iter = borrow.iter();
			let iter = mem::transmute(iter);
			Ok(Self {
				_array: array,
				_borrow: borrow,
				iter,
			})
		}
	}
}

impl<V> Iterator for ArrayIter<'_, V>
where
	V: VariantType,
{
	type Item = V;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().map(V::clone)
	}
}

impl<V> DictionaryIter<'_, V>
where
	V: VariantType,
{
	fn new(dictionary: Dictionary<V>) -> CallResult<Self> {
		// SAFETY:
		// The borrow is valid as long as the array isn't dropped
		// The iterator is valid as long as the borrow isn't dropped
		unsafe {
			let borrow = borrow!(dictionary);
			let borrow: Ref<'_, HashMap<VariantOrd, V>> = mem::transmute(borrow);
			let iter = borrow.keys();
			let iter = mem::transmute(iter);
			Ok(Self {
				_dictionary: dictionary,
				_borrow: borrow,
				iter,
			})
		}
	}
}

impl<V> Iterator for DictionaryIter<'_, V>
where
	V: VariantType,
{
	type Item = V;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().cloned().map(VariantOrd::as_variant)
	}
}

impl VariantOrd {
	fn from_variant<V>(var: V) -> Result<Self, CallError>
	where
		V: VariantType,
	{
		Ok(match var.as_bool() {
			Ok(v) => Self::Bool(v),
			Err(v) => match v.as_integer() {
				Ok(v) => Self::Integer(v),
				Err(_) => match var.as_string() {
					Ok(v) => Self::String(v),
					Err(_) => return Err(CallError::IncompatibleType),
				},
			},
		})
	}

	fn as_variant<V>(self) -> V
	where
		V: VariantType,
	{
		match self {
			Self::Bool(b) => V::new_bool(b),
			Self::Integer(i) => V::new_integer(i),
			Self::String(s) => V::new_string(s),
		}
	}
}

impl fmt::Debug for VariantOrd {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Bool(b) => write!(f, "{}", b),
			Self::Integer(b) => write!(f, "{}", b),
			Self::String(b) => write!(f, "{}", b),
		}
	}
}
