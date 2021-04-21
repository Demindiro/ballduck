// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

pub mod specialized;

#[cfg(not(feature = "std"))]
use crate::std_types::*;
use crate::{CallError, CallResult, Environment, Rc, ScriptObject};
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};
use core::{cmp, fmt};

/// This trait must be implemented on custom Variant types.
/// A custom variant is useful if you have a type that is common and needs to
/// be able to be processed relatively quickly.
pub trait VariantType
where
	Self:
		Clone + fmt::Debug + fmt::Display + Default + From<bool> + PartialEq + PartialOrd + 'static,
{
	fn new_bool(value: bool) -> Self;

	fn new_integer(value: isize) -> Self;

	fn new_real(value: f64) -> Self;

	fn new_string(value: Rc<str>) -> Self;

	fn new_object(value: ScriptObject<Self>) -> Self;

	fn as_bool(&self) -> Result<bool, &Self>;

	fn as_integer(&self) -> Result<isize, &Self>;

	fn as_real(&self) -> Result<f64, &Self>;

	fn as_string(self) -> Result<Rc<str>, Self>;

	fn as_object(self) -> Result<ScriptObject<Self>, Self>;

	fn call(&self, function: &str, args: &[&Self], env: &Environment<Self>) -> CallResult<Self>;

	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Self>>>;

	fn index(&self, index: &Self) -> CallResult<Self>;

	fn set_index(&self, index: &Self, value: Self) -> CallResult<()>;

	// TODO this is stupid as hell but I'm out of ideas
	fn add(&self, rhs: &Self) -> CallResult<Self>;
	fn sub(&self, rhs: &Self) -> CallResult<Self>;
	fn mul(&self, rhs: &Self) -> CallResult<Self>;
	fn div(&self, rhs: &Self) -> CallResult<Self>;
	fn rem(&self, rhs: &Self) -> CallResult<Self>;
	fn bitand(&self, rhs: &Self) -> CallResult<Self>;
	fn bitor(&self, rhs: &Self) -> CallResult<Self>;
	fn bitxor(&self, rhs: &Self) -> CallResult<Self>;
	fn lhs(&self, rhs: &Self) -> CallResult<Self>;
	fn rhs(&self, rhs: &Self) -> CallResult<Self>;
	fn neg(&self) -> CallResult<Self>;
	fn not(&self) -> CallResult<Self>;
}

#[derive(Clone)]
/// Variant type that encodes a few common types. Having the common types
/// not be hidden behind a dyn trait improves performance greatly
pub enum Variant {
	None,
	Bool(bool),
	Real(f64),
	Integer(isize),
	Char(char),
	String(Rc<str>),
	Object(ScriptObject<Self>),
}

impl Default for Variant {
	fn default() -> Self {
		Variant::None
	}
}

#[macro_export]
macro_rules! check_arg_count {
	($args:ident, $count:expr) => {
		if $args.len() != $count {
			return Err(CallError::BadArgumentCount);
		}
	};
}

#[macro_export]
macro_rules! gen_op {
	(
		$variant:ident, $trait:ident, $fn:ident
		[$left:ident, $right:ident]
		$([$lhs:ident, $rhs:ident] => $out:ident $code:block)*
	) => {
		impl<'a> $trait<&'a $variant> for &'a $variant {
			type Output = CallResult<$variant>;

			#[inline]
			fn $fn(self, rhs: &'a $variant) -> Self::Output {
				Ok(match (self, rhs) {
					$((Variant::$lhs($left), Variant::$rhs($right)) => Variant::$out($code),)*
					_ => return Err(CallError::IncompatibleType),
				})
			}
		}
	};
}

gen_op!(
	Variant, Add, add
	[rhs, lhs]
	[Real, Real] => Real { rhs + lhs }
	[Real, Integer] => Real { rhs + *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 + lhs }
	[Integer, Integer] => Integer { rhs + lhs }
	[String, String] => String {
		let mut out = rhs.to_string();
		out.extend(lhs.chars());
		out.into()
	}
);

gen_op!(
	Variant, Sub, sub
	[rhs, lhs]
	[Real, Real] => Real { rhs - lhs }
	[Real, Integer] => Real { rhs - *lhs as f64 }
	[Integer, Integer] => Integer { rhs - lhs }
);

gen_op!(
	Variant, Mul, mul
	[rhs, lhs]
	[Real, Real] => Real { rhs * lhs }
	[Real, Integer] => Real { rhs * *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 * lhs }
	[Integer, Integer] => Integer { rhs * lhs }
);

gen_op!(
	Variant, Div, div
	[rhs, lhs]
	[Real, Real] => Real { rhs / lhs }
	[Real, Integer] => Real { rhs / *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 / lhs }
	[Integer, Integer] => Integer { rhs / lhs }
);

gen_op!(
	Variant, Rem, rem
	[rhs, lhs]
	[Real, Real] => Real { rhs % lhs }
	[Real, Integer] => Real { rhs % *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 % lhs }
	[Integer, Integer] => Integer { rhs % lhs }
);

gen_op!(
	Variant, BitAnd, bitand
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs & lhs }
	[Integer, Integer] => Integer { rhs & lhs }
);

gen_op!(
	Variant, BitOr, bitor
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs | lhs }
	[Integer, Integer] => Integer { rhs | lhs }
);

gen_op!(
	Variant, BitXor, bitxor
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs ^ lhs }
	[Integer, Integer] => Integer { rhs ^ lhs }
);

gen_op!(
	Variant, Shl, shl
	[rhs, lhs]
	[Integer, Integer] => Integer { rhs << lhs }
);

gen_op!(
	Variant, Shr, shr
	[rhs, lhs]
	[Integer, Integer] => Integer { rhs >> lhs }
);

impl core::ops::Neg for &Variant {
	type Output = CallResult<Variant>;

	#[inline]
	fn neg(self) -> Self::Output {
		use Variant::*;
		Ok(match self {
			Integer(i) => Integer(-i),
			Real(r) => Real(-r),
			_ => return Err(CallError::IncompatibleType),
		})
	}
}

impl core::ops::Not for &Variant {
	type Output = CallResult<Variant>;

	#[inline]
	fn not(self) -> Self::Output {
		use Variant::*;
		Ok(match self {
			Bool(b) => Bool(!b),
			Integer(i) => Integer(!i),
			_ => return Err(CallError::IncompatibleType),
		})
	}
}

impl PartialEq<Self> for Variant {
	#[inline]
	// FIXME should we return bool or should we implement a custom form
	// of PartialEq that returns a Result?
	fn eq(&self, rhs: &Self) -> bool {
		use Variant::*;
		match (self, rhs) {
			(Bool(a), Bool(b)) => a == b,
			(Real(a), Real(b)) => a == b,
			(Real(a), Integer(b)) => *a == *b as f64,
			(Integer(a), Real(b)) => *a as f64 == *b,
			(Integer(a), Integer(b)) => a == b,
			(String(a), String(b)) => a == b,
			(Char(a), Char(b)) => a == b,
			_ => false,
		}
	}
}

impl PartialOrd<Self> for Variant {
	#[inline]
	// FIXME ditto, pretty much
	fn partial_cmp(&self, rhs: &Self) -> Option<cmp::Ordering> {
		use Variant::*;
		match (self, rhs) {
			(Bool(a), Bool(b)) => a.partial_cmp(b),
			(Real(a), Real(b)) => a.partial_cmp(b),
			(Real(a), Integer(b)) => a.partial_cmp(&(*b as f64)),
			(Integer(a), Real(b)) => (*a as f64).partial_cmp(b),
			(Integer(a), Integer(b)) => a.partial_cmp(b),
			(String(a), String(b)) => a.partial_cmp(b),
			(Char(a), Char(b)) => a.partial_cmp(b),
			_ => Option::None,
		}
	}
}

impl VariantType for Variant {
	#[inline]
	fn new_bool(value: bool) -> Self {
		Self::Bool(value)
	}

	#[inline]
	fn new_integer(value: isize) -> Self {
		Self::Integer(value)
	}

	#[inline]
	fn new_real(value: f64) -> Self {
		Self::Real(value)
	}

	#[inline]
	fn new_string(value: Rc<str>) -> Self {
		Self::String(value)
	}

	#[inline]
	fn new_object(value: ScriptObject<Self>) -> Self {
		Self::Object(value)
	}

	#[inline]
	fn as_bool(&self) -> Result<bool, &Self> {
		if let Self::Bool(b) = self {
			Ok(*b)
		} else {
			Err(self)
		}
	}

	#[inline]
	fn as_integer(&self) -> Result<isize, &Self> {
		if let Self::Integer(b) = self {
			Ok(*b)
		} else {
			Err(self)
		}
	}

	#[inline]
	fn as_real(&self) -> Result<f64, &Self> {
		if let Self::Real(b) = self {
			Ok(*b)
		} else {
			Err(self)
		}
	}

	#[inline]
	fn as_string(self) -> Result<Rc<str>, Self> {
		if let Self::String(b) = self {
			Ok(b)
		} else {
			Err(self)
		}
	}

	#[inline]
	fn as_object(self) -> Result<ScriptObject<Self>, Self> {
		if let Self::Object(b) = self {
			Ok(b)
		} else {
			Err(self)
		}
	}

	fn call(&self, function: &str, args: &[&Self], env: &Environment<Self>) -> CallResult<Self> {
		Ok(match self {
			Self::None => return Err(CallError::IsEmpty),
			Self::Real(r) => match function {
				"abs" => {
					check_arg_count!(args, 0);
					Self::Real(r.abs())
				}
				"sqrt" => {
					check_arg_count!(args, 0);
					Self::Real(r.sqrt())
				}
				_ => return Err(CallError::UndefinedFunction),
			},
			Self::Integer(i) => match function {
				"abs" => {
					check_arg_count!(args, 0);
					Self::Integer(i.abs())
				}
				_ => return Err(CallError::UndefinedFunction),
			},
			Self::String(s) => match function {
				"len" => {
					check_arg_count!(args, 0);
					Variant::Integer(s.len() as isize)
				}
				_ => return Err(CallError::UndefinedFunction),
			},
			Self::Object(o) => return o.call(function, args, env),
			_ => return Err(CallError::UndefinedFunction),
		})
	}

	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Self>>> {
		match self {
			Variant::None => Err(CallError::IsEmpty),
			&Variant::Integer(i) => {
				if i < 0 {
					Ok(Box::new((i + 1..=0).rev().map(|i| Variant::Integer(i))))
				} else {
					Ok(Box::new((0..i).map(|i| Variant::Integer(i))))
				}
			}
			Variant::String(s) => Ok(Box::new(StringIter::new(s.clone()))),
			Variant::Object(o) => o.iter(),
			_ => Err(CallError::IncompatibleType),
		}
	}

	#[inline]
	fn index(&self, index: &Self) -> CallResult<Self> {
		match self {
			Self::Object(obj) => obj.index(index),
			_ => Err(CallError::IncompatibleType),
		}
	}

	#[inline]
	fn set_index(&self, index: &Self, value: Self) -> CallResult<()> {
		match self {
			Self::Object(obj) => obj.set_index(index, value),
			_ => Err(CallError::IncompatibleType),
		}
	}

	#[inline]
	fn add(&self, rhs: &Self) -> CallResult<Self> {
		self + rhs
	}
	#[inline]
	fn sub(&self, rhs: &Self) -> CallResult<Self> {
		self - rhs
	}
	#[inline]
	fn mul(&self, rhs: &Self) -> CallResult<Self> {
		self * rhs
	}
	#[inline]
	fn div(&self, rhs: &Self) -> CallResult<Self> {
		self / rhs
	}
	#[inline]
	fn rem(&self, rhs: &Self) -> CallResult<Self> {
		self % rhs
	}
	#[inline]
	fn bitand(&self, rhs: &Self) -> CallResult<Self> {
		self & rhs
	}
	#[inline]
	fn bitor(&self, rhs: &Self) -> CallResult<Self> {
		self | rhs
	}
	#[inline]
	fn bitxor(&self, rhs: &Self) -> CallResult<Self> {
		self ^ rhs
	}
	#[inline]
	fn lhs(&self, rhs: &Self) -> CallResult<Self> {
		self << rhs
	}
	#[inline]
	fn rhs(&self, rhs: &Self) -> CallResult<Self> {
		self >> rhs
	}
	#[inline]
	fn neg(&self) -> CallResult<Self> {
		-self
	}
	#[inline]
	fn not(&self) -> CallResult<Self> {
		!self
	}
}

impl fmt::Debug for Variant {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use core::fmt::Write;
		match self {
			Variant::None => f.write_str("none"),
			Variant::Bool(false) => f.write_str("false"),
			Variant::Bool(true) => f.write_str("true"),
			Variant::Real(n) => f.write_str(n.to_string().as_str()),
			Variant::Integer(n) => f.write_str(n.to_string().as_str()),
			Variant::Char(n) => f.write_char(*n),
			Variant::String(n) => {
				f.write_char('"')?;
				f.write_str(n)?;
				f.write_char('"')
			}
			Variant::Object(n) => f.write_str(n.to_string().as_str()),
		}
	}
}

impl fmt::Display for Variant {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use core::fmt::Write;
		match self {
			Variant::None => f.write_str("none"),
			Variant::Bool(false) => f.write_str("false"),
			Variant::Bool(true) => f.write_str("true"),
			Variant::Real(n) => f.write_str(n.to_string().as_str()),
			Variant::Integer(n) => f.write_str(n.to_string().as_str()),
			Variant::Char(n) => f.write_char(*n),
			Variant::String(n) => f.write_str(n),
			Variant::Object(n) => f.write_str(n.to_string().as_str()),
		}
	}
}

impl From<bool> for Variant {
	fn from(var: bool) -> Self {
		Variant::Bool(var)
	}
}

// DO NOT REORDER THE FIELDS: the drop order is important!
struct StringIter<'a> {
	iter: core::str::Chars<'a>,
	_string: Rc<str>,
}

impl<'a> StringIter<'a> {
	fn new(string: Rc<str>) -> Self {
		let iter = string.chars();
		// SAFETY: the reference held by `iter` is valid as long as `string´ isn't dropped
		unsafe {
			let iter = core::mem::transmute(iter);
			Self {
				_string: string,
				iter,
			}
		}
	}
}

impl Iterator for StringIter<'_> {
	type Item = Variant;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().map(Variant::Char)
	}
}
