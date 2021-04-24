// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See script/LICENSE for details.

#[cfg(not(feature = "std"))]
use crate::std_types::*;
use crate::{CallError, CallResult, Environment, Rc, ScriptObject};
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};
use core::{cmp, fmt};

use super::{check_arg_count, gen_op, VariantType};

#[derive(Clone, Copy)]
/// This is a special variant type that implements only types that implement Copy.
/// This means it is _much_ faster to process in the interpreter loop, as there is
/// no need to drop any values.
pub enum Variant {
	None,
	Bool(bool),
	Real(f64),
	Integer(isize),
	Char(char),
}

impl Default for Variant {
	fn default() -> Self {
		Variant::None
	}
}

gen_op!(
	Variant, Add, add
	[rhs, lhs]
	[Real, Real] => Real { rhs + lhs }
	[Real, Integer] => Real { rhs + *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 + lhs }
	[Integer, Integer] => Integer { rhs + lhs }
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

	fn neg(self) -> Self::Output {
		use Variant::*;
		Ok(match self {
			Integer(i) => Integer(-i),
			Real(r) => Real(-r),
			_ => return Err(CallError::incompatible_type()),
		})
	}
}

impl core::ops::Not for &Variant {
	type Output = CallResult<Variant>;

	fn not(self) -> Self::Output {
		use Variant::*;
		Ok(match self {
			Bool(b) => Bool(!b),
			Integer(i) => Integer(!i),
			_ => return Err(CallError::incompatible_type()),
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
	fn new_string(_: Rc<str>) -> Self {
		unimplemented!()
	}

	#[inline]
	fn new_object(_: ScriptObject<Self>) -> Self {
		unimplemented!()
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
	fn into_string(self) -> Result<Rc<str>, Self> {
		Err(self)
	}

	#[inline]
	fn into_object(self) -> Result<ScriptObject<Self>, Self> {
		Err(self)
	}

	fn call(&self, function: &str, args: &[&Self], _: &Environment<Self>) -> CallResult<Self> {
		Ok(match self {
			Self::None => return Err(CallError::empty()),
			Self::Real(r) => match function {
				"abs" => {
					check_arg_count!(args, 0);
					Self::Real(r.abs())
				}
				"sqrt" => {
					check_arg_count!(args, 0);
					Self::Real(r.sqrt())
				}
				_ => return Err(CallError::undefined_function()),
			},
			Self::Integer(i) => match function {
				"abs" => {
					check_arg_count!(args, 0);
					Self::Integer(i.abs())
				}
				_ => return Err(CallError::undefined_function()),
			},
			_ => return Err(CallError::undefined_function()),
		})
	}

	#[inline]
	fn iter(&self) -> CallResult<Box<dyn Iterator<Item = Self>>> {
		match self {
			Variant::None => Err(CallError::empty()),
			&Variant::Integer(i) => {
				if i < 0 {
					Ok(Box::new((i + 1..=0).rev().map(Variant::Integer)))
				} else {
					Ok(Box::new((0..i).map(Variant::Integer)))
				}
			}
			_ => Err(CallError::incompatible_type()),
		}
	}

	#[inline]
	fn index(&self, _: &Self) -> CallResult<Self> {
		Err(CallError::incompatible_type())
	}

	#[inline]
	fn set_index(&self, _: &Self, _: Self) -> CallResult<()> {
		Err(CallError::incompatible_type())
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
		}
	}
}

impl From<bool> for Variant {
	fn from(var: bool) -> Self {
		Variant::Bool(var)
	}
}
