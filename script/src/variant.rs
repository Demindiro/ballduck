use crate::bytecode::CallResult;
use crate::{CallError, ScriptObject};
use core::cmp;
use core::fmt::Debug;
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

#[derive(Clone, Debug)]
/// Variant type that encodes a few common types. Having the common types
/// not be hidden behind a dyn trait improves performance greatly
pub enum Variant {
	None,
	Bool(bool),
	Real(f64),
	Integer(isize),
	Object(ScriptObject),
}

impl Default for Variant {
	fn default() -> Self {
		Variant::None
	}
}

macro_rules! check_arg_count {
	($args:ident, $count:expr) => {
		if $args.len() != $count {
			return Err(CallError::BadArgumentCount);
		}
	};
}

macro_rules! gen_op {
	(
		$trait:ident, $fn:ident
		[$left:ident, $right:ident]
		$([$lhs:ident, $rhs:ident] => $out:ident { $code:expr })*
	) => {
		impl $trait<Self> for &Variant {
			type Output = CallResult;

			#[inline]
			fn $fn(self, rhs: Self) -> Self::Output {
				Ok(match (self, rhs) {
					$((Variant::$lhs($left), Variant::$rhs($right)) => Variant::$out({ $code }),)*
					_ => return Err(CallError::IncompatibleType),
				})
			}
		}
	};
}

gen_op!(
	Add, add
	[rhs, lhs]
	[Real, Real] => Real { rhs + lhs }
	[Real, Integer] => Real { rhs + *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 + lhs }
	[Integer, Integer] => Integer { rhs + lhs }
);

gen_op!(
	Sub, sub
	[rhs, lhs]
	[Real, Real] => Real { rhs - lhs }
	[Real, Integer] => Real { rhs - *lhs as f64 }
	[Integer, Integer] => Integer { rhs - lhs }
);

gen_op!(
	Mul, mul
	[rhs, lhs]
	[Real, Real] => Real { rhs * lhs }
	[Real, Integer] => Real { rhs * *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 * lhs }
	[Integer, Integer] => Integer { rhs * lhs }
);

gen_op!(
	Div, div
	[rhs, lhs]
	[Real, Real] => Real { rhs / lhs }
	[Real, Integer] => Real { rhs / *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 / lhs }
	[Integer, Integer] => Integer { rhs / lhs }
);

gen_op!(
	Rem, rem
	[rhs, lhs]
	[Real, Real] => Real { rhs % lhs }
	[Real, Integer] => Real { rhs % *lhs as f64 }
	[Integer, Real] => Real { *rhs as f64 % lhs }
	[Integer, Integer] => Integer { rhs % lhs }
);

gen_op!(
	BitAnd, bitand
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs & lhs }
	[Integer, Integer] => Integer { rhs & lhs }
);

gen_op!(
	BitOr, bitor
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs | lhs }
	[Integer, Integer] => Integer { rhs | lhs }
);

gen_op!(
	BitXor, bitxor
	[rhs, lhs]
	[Bool, Bool] => Bool { rhs ^ lhs }
	[Integer, Integer] => Integer { rhs ^ lhs }
);

gen_op!(
	Shl, shl
	[rhs, lhs]
	[Integer, Integer] => Integer { rhs << lhs }
);

gen_op!(
	Shr, shr
	[rhs, lhs]
	[Integer, Integer] => Integer { rhs >> lhs }
);

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

impl Variant {
	pub fn call(&self, function: &str, args: &[Variant]) -> Result<Variant, CallError> {
		match self {
			Variant::None => Err(CallError::IsEmpty),
			Variant::Bool(_) => Err(CallError::UndefinedFunction),
			Variant::Real(r) => match function {
				"sqrt" => {
					check_arg_count!(args, 0);
					Ok(Variant::Real(r.sqrt()))
				}
				_ => Err(CallError::UndefinedFunction),
			},
			Variant::Integer(_) => Err(CallError::UndefinedFunction),
			Variant::Object(o) => o.call(function, args),
		}
	}
}
