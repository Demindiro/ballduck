use crate::bytecode::{ByteCode, CallResult, Environment};
use core::fmt::Debug;
use rustc_hash::FxHashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Script {
    pub(crate) functions: FxHashMap<Box<str>, ByteCode>,
    pub(crate) locals: FxHashMap<Box<str>, u16>,
}

#[derive(Debug)]
pub struct Instance<'asm> {
    script: &'asm Script,
    variables: Box<[Box<dyn ScriptType>]>,
}

#[derive(Debug)]
pub enum CallError {
    UndefinedFunction,
    BadArgument,
    BadArgumentCount,
    RunError,
}

pub trait ScriptType: Debug {
    fn call(
        &mut self,
        function: &str,
        args: &[&dyn ScriptType],
    ) -> Result<Option<Box<dyn ScriptType>>, CallError>;
}

pub trait ScriptIter {
    fn iter(&self) -> dyn Iterator<Item = Box<dyn ScriptType>>;
}

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

    pub fn instance(&self) -> Instance {
        Instance {
            script: self,
            variables: Box::new([]),
        }
    }

    fn call(
        &self,
        function: &str,
        locals: &mut [Box<dyn ScriptType>],
        args: &[&dyn ScriptType],
    ) -> Result<Option<Box<dyn ScriptType>>, CallError> {
        let mut env = Environment::new();
        env.add_function(
            "print".into(),
            Box::new(|a: &[&_]| {
                a.iter().for_each(|a| println!("{:?}", a));
                Ok(None)
            }),
        )
        .unwrap();

        if let Some(function) = self.functions.get(function) {
            match function.run(locals, &self.locals, args, &env) {
                Ok(ret) => Ok(ret),
                Err(_) => Err(CallError::RunError),
            }
        } else {
            Err(CallError::UndefinedFunction)
        }
    }
}

impl ScriptType for Instance<'_> {
    fn call(&mut self, function: &str, args: &[&dyn ScriptType]) -> CallResult<CallError> {
        self.script.call(function, &mut self.variables, args)
    }
}

impl ScriptType for isize {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }
}

impl ScriptType for f64 {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }
}

impl ScriptType for String {
    fn call(&mut self, _: &str, _: &[&dyn ScriptType]) -> CallResult<CallError> {
        todo!()
    }
}
