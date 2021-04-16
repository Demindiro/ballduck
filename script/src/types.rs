//! Arrays and Dictionaries are implemented as ScriptTypes as mixing 16 byte with 8 byte `Rc<T>`s
//! has a significant impact on `clone` performance.
//! It may be better to have some sort of custom `Array` type that is unsized, but AFAICT that isn't
//! possible (yet?).

use crate::{CallError, CallResult, Environment, ScriptType, Variant};
use core::cell::{Ref, RefCell};
use core::convert::{TryFrom, TryInto};
use core::{fmt, mem};
#[cfg(feature = "fast-dictionary")]
use rustc_hash::FxHashMap;
#[cfg(not(feature = "fast-dictionary"))]
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Array(Rc<RefCell<Vec<Variant>>>);

#[derive(Clone, Debug)]
#[cfg(not(feature = "fast-dictionary"))]
pub struct Dictionary(Rc<RefCell<HashMap<VariantOrd, Variant>>>);
#[cfg(feature = "fast-dictionary")]
pub struct Dictionary(Rc<RefCell<FxHashMap<VariantOrd, Variant>>>);

/// A Variant type with only types that implement Ord and are not interiorly mutable
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum VariantOrd {
	Bool(bool),
	Integer(isize),
	String(Rc<str>),
}

/// An iterator that holds a [`Ref`](core::cell::Ref)
// DO NOT REORDER THE FIELDS: the drop order is important!
struct ArrayIter<'a> {
	iter: core::slice::Iter<'a, Variant>,
	_borrow: Ref<'a, Vec<Variant>>,
	_array: Array,
}

/// An iterator that holds a [`Ref`](core::cell::Ref)
// DO NOT REORDER THE FIELDS: the drop order is important!
struct DictionaryIter<'a> {
	iter: std::collections::hash_map::Keys<'a, VariantOrd, Variant>,
	_borrow: Ref<'a, HashMap<VariantOrd, Variant>>,
	_dictionary: Dictionary,
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

impl Array {
	pub fn new() -> Self {
		Self(Rc::new(RefCell::new(Vec::new())))
	}

	pub fn with_capacity(n: usize) -> Self {
		Self(Rc::new(RefCell::new(Vec::with_capacity(n))))
	}

	pub(crate) fn with_len(n: usize) -> Self {
		let mut v = Vec::new();
		v.resize(n, Variant::default());
		Self(Rc::new(RefCell::new(v)))
	}
}

impl ScriptType for Array {
	fn call(&self, function: &str, args: &[Variant], _: &Environment) -> CallResult {
		match function {
			"len" => {
				check_arg_count!(args, 0);
				Ok(Variant::Integer(borrow!(self).len() as isize))
			}
			"push" => {
				check_arg_count!(args, 1);
				borrow!(mut self).push(args[0].clone());
				Ok(Variant::None)
			}
			// TODO is it fine to default to None?
			"pop" => {
				check_arg_count!(args, 0);
				Ok(borrow!(mut self).pop().unwrap_or(Variant::None))
			}
			_ => Err(CallError::UndefinedFunction),
		}
	}

	#[inline]
	fn index(&self, index: &Variant) -> CallResult {
		if let Variant::Integer(index) = index {
			borrow!(self)
				.get(*index as usize)
				.cloned()
				.ok_or(CallError::BadArgument)
		} else {
			Err(CallError::BadArgument)
		}
	}

	#[inline]
	fn set_index(&self, index: &Variant, value: Variant) -> CallResult<()> {
		if let Variant::Integer(index) = index {
			borrow!(mut self)
				.get_mut(*index as usize)
				.map(|v| *v = value)
				.ok_or(CallError::BadArgument)
		} else {
			Err(CallError::BadArgument)
		}
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Variant>>> {
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

impl Dictionary {
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

impl ScriptType for Dictionary {
	fn call(&self, function: &str, args: &[Variant], _: &Environment) -> CallResult {
		match function {
			"len" => {
				check_arg_count!(args, 0);
				Ok(Variant::Integer(borrow!(self).len() as isize))
			}
			"insert" => {
				check_arg_count!(args, 2);
				let key = args[0].clone().try_into()?;
				let value = args[1].clone();
				Ok(borrow!(mut self)
					.insert(key, value)
					.unwrap_or(Variant::None))
			}
			// TODO is it fine to default to None?
			"remove" => {
				check_arg_count!(args, 1);
				let key = args[0].clone().try_into()?;
				Ok(borrow!(mut self).remove(&key).unwrap_or(Variant::None))
			}
			_ => Err(CallError::UndefinedFunction),
		}
	}

	#[inline]
	fn index(&self, index: &Variant) -> CallResult {
		let key = index.clone().try_into()?;
		borrow!(self)
			.get(&key)
			.cloned()
			.ok_or(CallError::BadArgument)
	}

	#[inline]
	fn set_index(&self, index: &Variant, value: Variant) -> CallResult<()> {
		let key = index.clone().try_into()?;
		borrow!(mut self).insert(key, value);
		Ok(())
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Variant>>> {
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

impl TryFrom<Variant> for VariantOrd {
	type Error = CallError;

	fn try_from(var: Variant) -> Result<Self, Self::Error> {
		Ok(match var {
			Variant::Bool(b) => Self::Bool(b),
			Variant::Integer(i) => Self::Integer(i),
			Variant::String(s) => Self::String(s),
			_ => return Err(CallError::IncompatibleType),
		})
	}
}

impl From<VariantOrd> for Variant {
	fn from(var: VariantOrd) -> Self {
		match var {
			VariantOrd::Bool(b) => Self::Bool(b),
			VariantOrd::Integer(i) => Self::Integer(i),
			VariantOrd::String(s) => Self::String(s),
		}
	}
}

impl ArrayIter<'_> {
	fn new(array: Array) -> CallResult<Self> {
		// SAFETY:
		// The borrow is valid as long as the array isn't dropped
		// The iterator is valid as long as the borrow isn't dropped
		unsafe {
			let borrow = borrow!(array);
			let borrow: Ref<'_, Vec<Variant>> = mem::transmute(borrow);
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

impl Iterator for ArrayIter<'_> {
	type Item = Variant;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().map(Variant::clone)
	}
}

impl DictionaryIter<'_> {
	fn new(dictionary: Dictionary) -> CallResult<Self> {
		// SAFETY:
		// The borrow is valid as long as the array isn't dropped
		// The iterator is valid as long as the borrow isn't dropped
		unsafe {
			let borrow = borrow!(dictionary);
			let borrow: Ref<'_, HashMap<VariantOrd, Variant>> = mem::transmute(borrow);
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

impl Iterator for DictionaryIter<'_> {
	type Item = Variant;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().cloned().map(Into::into)
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
