use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::{ScriptIter, ScriptType};
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

    // TODO avoid box
    IterConst(Box<(u16, u32, Box<dyn ScriptIter>)>),
    IterJmp(u16, u32),

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
    RegisterOutOfBounds,
	NoIterator,
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
        let mut instr = Vec::new();
        let mut vars = FxHashMap::with_hasher(Default::default());
        let vars = Self::parse_block(&function.lines, locals, &mut instr, &mut vars, 0)?;
        if let Some(Instruction::Ret(_)) = instr.last() {
        } else {
            instr.push(Instruction::Ret(None));
        }
        Ok(Self {
            code: instr,
            var_count: vars,
        })
    }

    fn parse_block<'a>(
        lines: &Lines<'a>,
        locals: &FxHashMap<Box<str>, u16>,
        instr: &mut Vec<Instruction>,
        vars: &mut FxHashMap<&'a str, u16>,
        mut min_var_count: u16,
    ) -> Result<u16, ByteCodeError> {
        let mut frame_vars = Vec::new();
        for line in lines {
            match line {
                Statement::Call { func, args } => {
                    for a in args {
                        match a {
                            Expression::Atom(a) => {
                                instr.push(match a {
                                    Atom::String(a) => Instruction::PushConstArg(Box::new(
                                        a.to_string().into_boxed_str(),
                                    )),
                                    Atom::Integer(a) => Instruction::PushConstArg(Box::new(*a)),
                                    Atom::Real(a) => Instruction::PushConstArg(Box::new(*a)),
                                    Atom::Name(a) => todo!("call {:?}", a),
                                });
                            }
                            _ => todo!(),
                        }
                    }
                    instr.push(Instruction::CallGlobal((*func).into()));
                }
                Statement::For { var, expr, lines } => {
                    let reg = vars.len().try_into().expect("Too many variables");
                    vars.insert(var, reg).unwrap_none();
                    frame_vars.push(var);
                    match expr {
                        Expression::Atom(a) => {
                            instr.push(match a {
                                Atom::String(a) => Instruction::IterConst(Box::new((
                                    reg,
                                    u32::MAX,
                                    Box::new(a.to_string().into_boxed_str()),
                                ))),
                                Atom::Integer(a) => {
                                    Instruction::IterConst(Box::new((reg, u32::MAX, Box::new(*a))))
                                }
                                //Atom::Real(a) => Instruction::IterConst(Box::new(a)),
                                Atom::Real(a) => todo!("for Real({})", a),
                                Atom::Name(a) => todo!("for {:?}", a),
                            })
                        }
                        _ => todo!(),
                    }
                    let ic = instr.len() - 1;
                    let ip = instr.len() as u32;
                    min_var_count = Self::parse_block(lines, locals, instr, vars, min_var_count)?;
                    instr.push(Instruction::IterJmp(reg, ip));
					let ip = instr.len() as u32;
					if let Some(Instruction::IterConst(ic)) = instr.get_mut(ic) {
						ic.1 = ip;
					} else {
						unreachable!();
					}
                }
                _ => todo!("{:?}", line),
            }
        }
        min_var_count = min_var_count.max(vars.len() as u16);
        for fv in frame_vars {
            vars.remove(fv).unwrap();
        }
        Ok(min_var_count)
    }

    pub(crate) fn run(
        &self,
        locals: &mut [Box<dyn ScriptType>],
        local_map: &FxHashMap<Box<str>, u16>,
        args: &[&dyn ScriptType],
        env: &Environment,
    ) -> Result<Option<Box<dyn ScriptType>>, RunError> {
        let mut vars = Vec::new();
        vars.resize_with(self.var_count as usize, || {
            Box::new(()) as Box<dyn ScriptType>
        });
        let mut ip = 0;
        let mut call_args = Vec::new();
        let mut iterators = Vec::new();
        loop {
            if let Some(instr) = self.code.get(ip as usize) {
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
                    IterConst(box (reg, jmp_ip, iter)) => {
                        let mut iter = iter.iter();
                        if let Some(e) = iter.next() {
                            *vars
                                .get_mut(*reg as usize)
                                .ok_or(RunError::RegisterOutOfBounds)? = e;
                            iterators.push(iter);
                        } else {
                            ip = *jmp_ip;
                        }
                    }
                    IterJmp(reg, jmp_ip) => {
						if let Some(iter) = iterators.last_mut() {
							if let Some(e) = iter.next() {
								*vars
									.get_mut(*reg as usize)
									.ok_or(RunError::RegisterOutOfBounds)? = e;
								ip = *jmp_ip;
							}
						} else {
							return Err(RunError::NoIterator);
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
