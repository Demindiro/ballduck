//! Arrays and Dictionaries are implemented as ScriptTypes as mixing 16 byte with 8 byte `Rc<T>`s
//! has a significant impact on `clone` performance.
//! It may be better to have some sort of custom `Array` type that is unsized, but AFAICT that isn't
//! possible (yet?).

use crate::{CallError, CallResult, Environment, ScriptType, Variant};
use core::cell::RefCell;
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
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum VariantOrd {
	None,
	Bool(bool),
	Integer(isize),
	String(Rc<str>),
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

	fn set_index(&self, index: &Variant, value: Variant) -> Result<(), CallError> {
		if let Variant::Integer(index) = index {
			borrow!(mut self)
				.get_mut(*index as usize)
				.map(|v| *v = value)
				.ok_or(CallError::BadArgument)
		} else {
			Err(CallError::BadArgument)
		}
	}

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
