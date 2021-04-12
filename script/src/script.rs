use crate::bytecode::{ByteCode, CallResult, Environment, RunError};
use core::any::Any;
use core::fmt::Debug;
use core::ops::{Add, Mul};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

pub struct Class(Arc<Script>);

#[derive(Debug)]
pub(crate) struct Script {
    pub(crate) functions: FxHashMap<Box<str>, ByteCode>,
    pub(crate) locals: FxHashMap<Box<str>, u16>,
}

#[derive(Debug)]
pub struct Instance {
    script: Arc<Script>,
    // TODO we have 3 options to solve the `A.foo() -> B.bar() -> A.foo()` problem:
    // * We use `Cell<Box<[_]>>`. This *should* have relatively little overhead but is
    //   very unintuitive, and the second `A.foo()` call will only see old variable names.
    //   It is also likely still less efficient than `RefCell` due to an extra alloc.
    // * We use `RefCell<Box<[_]>>`. This is arguably the most "Rusty" way in terms of
    //   "don't alias stuff", but is not very intuitive compared to other languages
    //   and may also end up being impractical.
    // * We use `Box<[Cell<_>]>`. This will allow mimicking other scripting languages in
    //   terms of (expected) behaviour but may be less efficient than `RefCell`.
    // The second and third option should be measured for performance. If the latter is
    // fast enough (or perhaps even faster) we should use that.
    // For now, the second option is chosen as the third can't be undone without being a
    // massive breaking change.
    variables: RefCell<Box<[Rc<dyn ScriptType>]>>,
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
    AlreadyBorrowed,
}

pub trait ScriptType: Debug + Any {
    fn call(&self, function: &str, args: &[Rc<dyn ScriptType>]) -> CallResult<CallError>;

    fn dup(&self) -> Rc<dyn ScriptType>;

    fn mul(&self, rhs: &Rc<dyn ScriptType>) -> CallResult<CallError> {
        let _ = rhs;
        Err(CallError::InvalidOperator)
    }

    fn add(&self, rhs: &Rc<dyn ScriptType>) -> CallResult<CallError> {
        let _ = rhs;
        Err(CallError::InvalidOperator)
    }

    // rust pls
    fn as_any(&self) -> Box<dyn Any>;
}

/*
impl Mul<Self> for &Rc<dyn ScriptType> {
    type Output = CallResult<CallError>;

    fn mul(self, rhs: Self) -> Self::Output {
        self.as_ref().mul(rhs)
    }
}

impl Add<Self> for &Rc<dyn ScriptType> {
    type Output = CallResult<CallError>;

    fn add(self, rhs: Self) -> Self::Output {
        self.as_ref().add(rhs)
    }
}
*/

/// Regular ol' clone doesn't work because of [`Sized`] stuff, so this exists
macro_rules! impl_dup {
    () => {
        fn dup(&self) -> Rc<dyn ScriptType> {
            Rc::new(self.clone()) as Rc<dyn ScriptType>
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

pub type ScriptIterator<'a> = Box<dyn Iterator<Item = Rc<dyn ScriptType>> + 'a>;

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
        locals: &mut [Rc<dyn ScriptType>],
        args: &[Rc<dyn ScriptType>],
    ) -> Result<Rc<dyn ScriptType>, CallError> {
        let mut env = Environment::new();
        env.add_function(
            "print".into(),
            Box::new(|a: &[_]| {
                a.iter().for_each(|a| println!("{:?}", a));
                Ok(Rc::new(()))
            }),
        )
        .unwrap();

        if let Some(function) = self.functions.get(function) {
            function
                .run(&self.functions, locals, args, &env)
                .map_err(CallError::RunError)
        } else {
            Err(CallError::UndefinedFunction)
        }
    }
}

impl Class {
    pub fn instance(&self) -> Instance {
        Instance {
            script: self.0.clone(),
            variables: RefCell::new(Box::new([])),
        }
    }
}

impl From<Script> for Class {
    fn from(script: Script) -> Self {
        Self(Arc::new(script))
    }
}

impl ScriptType for Instance {
    fn call(&self, function: &str, args: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        if let Ok(mut vars) = self.variables.try_borrow_mut() {
            self.script.call(function, &mut vars, args)
        } else {
            Err(CallError::AlreadyBorrowed)
        }
    }

    fn dup(&self) -> Rc<dyn ScriptType> {
        todo!();
    }

    fn as_any(&self) -> Box<dyn Any> {
        todo!();
    }
}

impl ScriptType for () {
    fn call(&self, _: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        Err(CallError::IsEmpty)
    }

    impl_dup!();
}

impl ScriptType for isize {
    fn call(&self, _: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        todo!()
    }

    impl_dup!();
}

impl ScriptType for f64 {
    fn call(&self, func: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        Ok(Rc::new(match func {
            "sqrt" => self.sqrt(),
            _ => return Err(CallError::UndefinedFunction),
        }))
    }

    impl_dup!();

    fn mul(&self, rhs: &Rc<dyn ScriptType>) -> CallResult<CallError> {
        rhs.as_any()
            .downcast_ref::<Self>()
            .map(|rhs| Rc::new(self * rhs) as Rc<dyn ScriptType>)
            .ok_or(CallError::IncompatibleType)
    }

    fn add(&self, rhs: &Rc<dyn ScriptType>) -> CallResult<CallError> {
        rhs.as_any()
            .downcast_ref::<Self>()
            .map(|rhs| Rc::new(self + rhs) as Rc<dyn ScriptType>)
            .ok_or(CallError::IncompatibleType)
    }
}

impl ScriptType for Box<str> {
    fn call(&self, _: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        todo!()
    }

    impl_dup!();
}

impl ScriptType for char {
    fn call(&self, _: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
        todo!()
    }

    impl_dup!();
}

impl ScriptType for String {
    fn call(&self, _: &str, _: &[Rc<dyn ScriptType>]) -> CallResult<CallError> {
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
                    .map(|i| Rc::new(i) as Rc<dyn ScriptType>),
            )
        } else {
            Box::new((0..*self).map(|i| Rc::new(i) as Rc<dyn ScriptType>))
        }
    }
}

impl ScriptIter for Box<str> {
    fn iter(&self) -> ScriptIterator {
        Box::new(self.chars().map(|c| Rc::new(c) as Rc<dyn ScriptType>))
    }
}

impl ScriptIter for String {
    fn iter(&self) -> ScriptIterator {
        Box::new(self.chars().map(|c| Rc::new(c) as Rc<dyn ScriptType>))
    }
}
