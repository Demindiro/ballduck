use crate::ast::{Atom, Expression, Function, Statement};
use crate::ScriptType;
use core::convert::TryInto;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub enum Instruction {
    // TODO avoid box
    Call(Box<(u16, Box<str>)>),
    CallSelf(Box<str>),
    CallGlobal(Box<str>),
    //Iter(Box<dyn Iterator<Item = Box<dyn ScriptType>>>),
    Jmp(u32),
    JmpIf(u16, u32),
    Ret(Option<u16>),

    AndJmp(u16, u16, u32),
    OrJmp(u16, u16, u32),
    Xor(u16, u16, u32),
    Eq,
    Neq,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    AddConst,
    SubConst,
    MulConst,
    DivConst,
    RemConst,

    PushArg(u16),
    PushConstArg(Box<dyn ScriptType>),
}

#[derive(Debug)]
pub enum ByteCodeError {
    DuplicateLocal,
}

#[derive(Debug)]
pub(crate) struct ByteCode {
    code: Vec<Instruction>,
    var_count: u16,
}

#[derive(Debug)]
pub enum RunError {
    IpOutOfBounds,
    VariableOutOfBounds,
}

pub struct Environment {
    functions: FxHashMap<Box<str>, EnvironmentFunction>,
}

pub type EnvironmentFunction = Box<dyn Fn(&[&dyn ScriptType]) -> CallResult<RunError>>;
pub type CallResult<E> = Result<Option<Box<dyn ScriptType>>, E>;

#[derive(Debug)]
pub enum EnvironmentError {
    FunctionAlreadyExists,
    UndefinedFunction,
}

impl ByteCode {
    pub(crate) fn parse(
        function: Function,
        locals: &FxHashMap<Box<str>, u16>,
    ) -> Result<Self, ByteCodeError> {
        //let mut vars = FxHashMap::with_hasher(Default::default());
        let mut instr = Vec::new();
        for line in function.lines {
            match line {
                Statement::Call { func, args } => {
                    for a in args {
                        match a {
                            Expression::Atom(a) => {
                                instr.push(match a {
                                    Atom::String(a) => {
                                        Instruction::PushConstArg(Box::new(a.to_string()))
                                    }
                                    Atom::Integer(a) => Instruction::PushConstArg(Box::new(a)),
                                    Atom::Real(a) => Instruction::PushConstArg(Box::new(a)),
                                    Atom::Name(a) => todo!(),
                                });
                            }
                            _ => todo!(),
                        }
                    }
                    instr.push(Instruction::CallGlobal(func.into()));
                }
                _ => todo!(),
            }
        }
        instr.push(Instruction::Ret(None));
        Ok(Self {
            code: instr,
            var_count: 0,
        })
    }

    pub(crate) fn run(
        &self,
        locals: &mut [Box<dyn ScriptType>],
        local_map: &FxHashMap<Box<str>, u16>,
        args: &[&dyn ScriptType],
        env: &Environment,
    ) -> Result<Option<Box<dyn ScriptType>>, RunError> {
        let mut ip = 0;
        let mut call_args = Vec::new();
        loop {
            if let Some(instr) = self.code.get(ip) {
                ip += 1;
                use Instruction::*;
                match instr {
                    CallGlobal(func) => {
                        env.call(func, &call_args[..]);
                        call_args.clear();
                    }
                    PushConstArg(c) => {
                        call_args.push(c.as_ref());
                    }
                    Ret(ret) => {
                        break if let Some(ret) = ret {
                            dbg!(ret);
                            todo!()
                        } else {
                            Ok(None)
                        }
                    }
                    _ => {
                        dbg!(instr);
                        todo!()
                    }
                }
            } else {
                break Err(RunError::IpOutOfBounds);
            }
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            functions: FxHashMap::with_hasher(Default::default()),
        }
    }

    pub fn add_function(
        &mut self,
        name: String,
        f: EnvironmentFunction,
    ) -> Result<(), EnvironmentError> {
        match self.functions.entry(name.into_boxed_str()) {
            Entry::Vacant(e) => {
                e.insert(f);
                Ok(())
            }
            Entry::Occupied(_) => Err(EnvironmentError::FunctionAlreadyExists),
        }
    }

    pub fn call(&self, func: &str, args: &[&dyn ScriptType]) -> CallResult<EnvironmentError> {
        Ok(self.functions[func](args).unwrap())
    }
}
