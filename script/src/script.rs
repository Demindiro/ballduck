use core::ops::{Add, Mul};
use core::any::Any;
use crate::bytecode::{ByteCode, CallResult, Environment, RunError};
use core::fmt::Debug;
use std::sync::Arc;
use rustc_hash::FxHashMap;

pub struct Class(Arc<Script>);

#[derive(Debug)]
pub(crate) struct Script {
    pub(crate) functions: FxHashMap<Box<str>, ByteCode>,
    pub(crate) locals: FxHashMap<Box<str>, u16>,
}

#[derive(Debug)]
pub struct Instance {
    script: Arc<Script>,
    variables: Box<[Box<dyn ScriptType>]>,
}

#[derive(Debug)]
pub enum CallError {
    UndefinedFunction,
    BadArgument,
    BadArgumentCount,
    RunError(RunError),
    /// This is specifically intended for operations on `()` AKA "null"
    IsEmpty,
	InvalidOperator,
	IncompatibleType,
}

pub trait ScriptType: Debug + Any {
    fn call(
        &mut self,
        function: &str,
        args: &[&dyn ScriptType],
    ) -> CallResult<CallError>;

	fn dup(&self) -> Box<dyn ScriptType>;

	fn mul(&self, rhs: &Box<dyn ScriptType>) -> CallResult<CallError> {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	fn add(&self, rhs: &Box<dyn ScriptType>) -> CallResult<CallError> {
		let _ = rhs;
		Err(CallError::InvalidOperator)
	}

	// rust pls
	fn as_any(&self) -> Box<dyn Any>;
}

impl Mul<Self> for &Box<dyn ScriptType> {
	type Output = CallResult<CallError>;

	fn mul(self, rhs: Self) -> Self::Output {
		self.as_ref().mul(rhs)
	}
}

impl Add<Self> for &Box<dyn ScriptType> {
	type Output = CallResult<CallError>;

	fn add(self, rhs: Self) -> Self::Output {
		self.as_ref().add(rhs)
	}
}

/// Regular ol' clone doesn't work because of [`Sized`] stuff, so this exists
macro_rules! impl_dup {
	() => {
		fn dup(&self) -> Box<dyn ScriptType> {
			Box::new(self.clone()) as Box<dyn ScriptType>
		}

		fn as_any(&self) -> Box<dyn Any> {
			// FIXME avoid clone
			Box::new(self.clone())
		}
	};
}

pub trait ScriptIter: Debug {
    fn iter(&self) -> ScriptIterator;
}

pub type ScriptIterator<'a> = Box<dyn Iterator<Item = Box<dyn ScriptType>> + 'a>;

impl Script {
    pub(crate) fn new(locals: FxHashMap<Box<str>, u16>) -> Self {
        Self {
            functions: FxHashMap::with_hasher(Default::default()),
            locals,
        }
    }

    pub(crate) fn shrink(&mut self) {
        self.locals.shrink_to_fit();
        self.functions.shrink_to_fit();
    }

    fn call(
        &self,
        function: &str,
        locals: &mut [Box<dyn ScriptType>],
        args: &[&dyn ScriptType],
    ) -> Result<Box<dyn ScriptType>, CallError> {
        let mut env = Environment::new();
        env.add_function(
            "print".into(),
            Box::new(|a: &[&_]| {
                a.iter().for_each(|a| println!("{:?}", a));
                Ok(Box::new(()))
            }),
        )
        .unwrap();

        if let Some(function) = self.functions.get(function) {
            function.run(&self.functions, locals, args, &env).map_err(CallError::RunError)
        } else {
            Err(CallError::UndefinedFunction)
        }
    }
}

impl Class {
    pub fn instance(&self) -> Instance {
        Instance {
            script: self.0.clone(),
            variables: Box::new([]),
        }
    }
}

impl From<Script> for Class {
	fn from(script: Script) -> Self {
		Self(Arc::new(script))
	}
}

impl ScriptType for Instance {
    fn call(&mut self, function: &str, args: &[&dyn ScriptType]) -> CallResult<CallError> {
        self.script.call(function, &mut self.variables, args)
    }

	fn dup(&self) -> Box<dyn ScriptType> {
		todo!();
	}

	fn as_any(&self) -> Box<dyn Any> {
		todo!();
	}
}

impl ScriptType for () {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        Err(CallError::IsEmpty)
    }

	impl_dup!();
}

impl ScriptType for isize {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }

	impl_dup!();
}

impl ScriptType for f64 {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }

	impl_dup!();

	fn mul(&self, rhs: &Box<dyn ScriptType>) -> CallResult<CallError> {
		rhs
			.as_any()
			.downcast_ref::<Self>()
			.map(|rhs| Box::new(self * rhs) as Box<dyn ScriptType>)
			.ok_or(CallError::IncompatibleType)
	}

	fn add(&self, rhs: &Box<dyn ScriptType>) -> CallResult<CallError> {
		rhs
			.as_any()
			.downcast_ref::<Self>()
			.map(|rhs| Box::new(self + rhs) as Box<dyn ScriptType>)
			.ok_or(CallError::IncompatibleType)
	}
}

impl ScriptType for Box<str> {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }

	impl_dup!();
}

impl ScriptType for char {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }

	impl_dup!();
}

impl ScriptType for String {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }

	impl_dup!();
}

impl ScriptIter for isize {
    fn iter(&self) -> ScriptIterator {
        if *self < 0 {
            Box::new(
                ((-self + 1)..=0)
                    .rev()
                    .map(|i| Box::new(i) as Box<dyn ScriptType>),
            )
        } else {
            Box::new((0..*self).map(|i| Box::new(i) as Box<dyn ScriptType>))
        }
    }
}

impl ScriptIter for Box<str> {
    fn iter(&self) -> ScriptIterator {
        Box::new(self.chars().map(|c| Box::new(c) as Box<dyn ScriptType>))
    }
}

impl ScriptIter for String {
    fn iter(&self) -> ScriptIterator {
        Box::new(self.chars().map(|c| Box::new(c) as Box<dyn ScriptType>))
    }
}
