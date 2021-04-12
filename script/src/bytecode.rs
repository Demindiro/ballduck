use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::script::CallError;
use crate::tokenizer::Op;
use crate::{ScriptIter, ScriptType};
use core::convert::TryInto;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub(crate) struct CallArgs {
    store_in: Option<u16>,
    func: Box<str>,
    args: Box<[u16]>,
}

#[derive(Debug)]
pub(crate) enum Instruction {
    Call(Box<(u16, CallArgs)>),
    CallSelf(Box<CallArgs>),
    CallGlobal(Box<CallArgs>),
    //Iter(Box<dyn Iterator<Item = Box<dyn ScriptType>>>),
    Jmp(u32),
    JmpIf(u16, u32),

    RetSome,
    RetNone,

    // TODO avoid box
    IterConst(Box<(u16, u32, Box<dyn ScriptIter>)>),
    IterJmp(u16, u32),

    AndJmp(u16, u16, u32),
    OrJmp(u16, u16, u32),
    Xor(u16, u16, u32),
    Eq,
    Neq,

    Add(u16, u16, u16),
    Sub(u16, u16, u16),
    Mul(u16, u16, u16),
    Div(u16, u16, u16),
    Rem(u16, u16, u16),

    Move(u16, u16),
    DupConst(u16, Box<dyn ScriptType>),

    AddConst,
    SubConst,
    MulConst,
    DivConst,
    RemConst,
}

#[derive(Debug)]
pub enum ByteCodeError {
    DuplicateLocal,
    DuplicateParameter,
    UndefinedVariable,
}

#[derive(Debug)]
pub(crate) struct ByteCode {
    code: Vec<Instruction>,
    param_count: u16,
    var_count: u16,
}

#[derive(Debug)]
pub enum RunError {
    IpOutOfBounds,
    RegisterOutOfBounds,
    NoIterator,
    EnvironmentError(EnvironmentError),
    UndefinedFunction,
    CallError(Box<CallError>),
    IncorrectArgumentCount,
}

pub struct Environment {
    functions: FxHashMap<Box<str>, EnvironmentFunction>,
}

pub type EnvironmentFunction = Box<dyn Fn(&[&dyn ScriptType]) -> CallResult<RunError>>;
pub type CallResult<E> = Result<Box<dyn ScriptType>, E>;

#[derive(Debug)]
pub enum EnvironmentError {
    FunctionAlreadyExists,
    UndefinedFunction,
}

/// The parser
impl ByteCode {
    pub(crate) fn parse(
        function: Function,
        locals: &FxHashMap<Box<str>, u16>,
    ) -> Result<Self, ByteCodeError> {
        let mut instr = Vec::new();
        let mut vars = FxHashMap::with_hasher(Default::default());
        let param_count = function.parameters.len() as u16;
        for p in function.parameters {
            if vars.insert(p, vars.len() as u16).is_some() {
                return Err(ByteCodeError::DuplicateParameter);
            }
        }
        let mut var_count = vars.len() as u16;
        let vars = Self::parse_block(
            &function.lines,
            locals,
            &mut instr,
            &mut vars,
            &mut var_count,
            0,
        )?;
        match instr.last() {
            Some(Instruction::RetSome) | Some(Instruction::RetNone) => (),
            _ => instr.push(Instruction::RetNone),
        }
        Ok(Self {
            code: instr,
            var_count: vars,
            param_count,
        })
    }

    fn parse_block<'a>(
        lines: &Lines<'a>,
        locals: &FxHashMap<Box<str>, u16>,
        instr: &mut Vec<Instruction>,
        vars: &mut FxHashMap<&'a str, u16>,
        curr_var_count: &mut u16,
        mut min_var_count: u16,
    ) -> Result<u16, ByteCodeError> {
        let mut frame_vars = Vec::new();
        for line in lines {
            match line {
                Statement::Call {
                    func,
                    args: arguments,
                } => {
                    let og_cvc = *curr_var_count;
                    let mut args = Vec::with_capacity(arguments.len());
                    // TODO move this to `parse_expression`
                    for a in arguments {
                        args.push(match a {
                            Expression::Atom(a) => match a {
                                Atom::String(a) => {
                                    let v = Box::new(a.to_string().into_boxed_str());
                                    let v = Instruction::DupConst(*curr_var_count, v);
                                    instr.push(v);
                                    *curr_var_count += 1;
                                    *curr_var_count - 1
                                }
                                Atom::Integer(a) => {
                                    let v = Box::new(*a);
                                    let v = Instruction::DupConst(*curr_var_count, v);
                                    instr.push(v);
                                    *curr_var_count += 1;
                                    *curr_var_count - 1
                                }
                                Atom::Real(a) => {
                                    let v = Box::new(*a);
                                    let v = Instruction::DupConst(*curr_var_count, v);
                                    instr.push(v);
                                    *curr_var_count += 1;
                                    *curr_var_count - 1
                                }
                                Atom::Name(a) => todo!("call {:?}", a),
                            },
                            Expression::Function { name, arguments, expr } => {
                                let mut args = Vec::with_capacity(arguments.len());
                                let store_in = *curr_var_count;
                                *curr_var_count += 1;
                                let og_cvc = *curr_var_count;
                                for a in arguments {
                                    match a {
                                        Expression::Atom(a) => {
                                            args.push(match a {
                                                Atom::String(a) => {
                                                    let v =
                                                        Box::new(a.to_string().into_boxed_str());
                                                    let v =
                                                        Instruction::DupConst(*curr_var_count, v);
                                                    instr.push(v);
                                                    *curr_var_count += 1;
                                                    *curr_var_count - 1
                                                }
                                                Atom::Integer(a) => {
                                                    let v = Box::new(*a);
                                                    let v =
                                                        Instruction::DupConst(*curr_var_count, v);
                                                    instr.push(v);
                                                    *curr_var_count += 1;
                                                    *curr_var_count - 1
                                                }
                                                Atom::Real(a) => {
                                                    let v = Box::new(*a);
                                                    let v =
                                                        Instruction::DupConst(*curr_var_count, v);
                                                    instr.push(v);
                                                    *curr_var_count += 1;
                                                    *curr_var_count - 1
                                                }
                                                Atom::Name(a) => todo!("call {:?}", a),
                                            });
                                        }
                                        e => todo!("{:?}", e),
                                    }
                                }
                                min_var_count = min_var_count.max(*curr_var_count);
                                *curr_var_count = og_cvc;
                                let args = CallArgs {
                                    store_in: Some(store_in),
                                    func: (*name).into(),
                                    args: args.into_boxed_slice(),
                                };
                                instr.push(Instruction::CallSelf(Box::new(args)));
                                store_in
                            }
                            e => todo!("{:?}", e),
                        });
                    }
                    min_var_count = min_var_count.max(*curr_var_count);
                    *curr_var_count = og_cvc;
                    let args = CallArgs {
                        store_in: None,
                        func: (*func).into(),
                        args: args.into_boxed_slice(),
                    };
                    instr.push(Instruction::CallGlobal(Box::new(args)));
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
                    min_var_count = Self::parse_block(
                        lines,
                        locals,
                        instr,
                        vars,
                        curr_var_count,
                        min_var_count,
                    )?;
                    instr.push(Instruction::IterJmp(reg, ip));
                    let ip = instr.len() as u32;
                    if let Some(Instruction::IterConst(ic)) = instr.get_mut(ic) {
                        ic.1 = ip;
                    } else {
                        unreachable!();
                    }
                }
                Statement::Return { expr } => {
                    if let Some(expr) = expr {
                        let r = Self::parse_expression(
                            0,
                            expr,
                            locals,
                            instr,
                            vars,
                            &mut min_var_count,
                            &mut (vars.len() as u16),
                        )?;
                        if let Some(r) = r {
                            instr.push(Instruction::Move(0, r));
                        }
                        instr.push(Instruction::RetSome);
                    } else {
                        instr.push(Instruction::RetNone);
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

    fn parse_expression(
        store: u16,
        expr: &Expression,
        locals: &FxHashMap<Box<str>, u16>,
        instr: &mut Vec<Instruction>,
        vars: &FxHashMap<&str, u16>,
        min_var_count: &mut u16,
        curr_var_count: &mut u16,
    ) -> Result<Option<u16>, ByteCodeError> {
        match expr {
            Expression::Operation { left, op, right } => {
                let og_cvc = *curr_var_count;
                let (r_left, r_right) = (*curr_var_count, *curr_var_count + 1);
                *curr_var_count += 2;
                *min_var_count = *min_var_count.max(curr_var_count);
                let or_left = Self::parse_expression(
                    r_left,
                    left,
                    locals,
                    instr,
                    vars,
                    min_var_count,
                    curr_var_count,
                )?;
                let left = if let Some(l) = or_left {
                    l
                } else {
                    *curr_var_count -= 1;
                    r_left
                };
                let or_right = Self::parse_expression(
                    r_right,
                    right,
                    locals,
                    instr,
                    vars,
                    min_var_count,
                    curr_var_count,
                )?;
                let right = if let Some(r) = or_right { r } else { r_right };
                instr.push(match op {
                    Op::Add => Instruction::Add(store, left, right),
                    Op::Mul => Instruction::Mul(store, left, right),
                    o => todo!("{:?}", o),
                });
                *curr_var_count = og_cvc;
                Ok(None)
            }
            Expression::Atom(a) => match a {
                &Atom::Name(name) => {
                    if let Some(&reg) = vars.get(name).or_else(|| locals.get(name)) {
                        Ok(Some(reg))
                    } else {
                        return Err(ByteCodeError::UndefinedVariable);
                    }
                }
                a => todo!("{:?}", a),
            },
			Expression::Function { expr, name, arguments } => {
				let expr = if let Some(e) = expr { e } else { todo!("none expr in fn") };
				let og_cvc = *curr_var_count;
				let r = *curr_var_count;
				*curr_var_count += 1;
				let e = Self::parse_expression(r, expr, locals, instr, vars, min_var_count, curr_var_count)?;
				let expr = if let Some(e) = e { e } else { r };
				let mut args = Vec::with_capacity(arguments.len());
				for a in arguments {
					let r = *curr_var_count;
					*curr_var_count += 1;
					let e = Self::parse_expression(
						r,
						a,
						locals,
						instr,
						vars,
						min_var_count,
						curr_var_count,
					)?;
					if let Some(e) = e {
						args.push(e);
						*curr_var_count -= 1;
					} else {
						args.push(r);
					}
				}
				let ca = CallArgs {
					store_in: Some(store),
					func: (*name).into(),
					args: args.into_boxed_slice(),
				};
				instr.push(Instruction::Call(Box::new((expr, ca))));
				*curr_var_count = og_cvc;
				Ok(None)
			}
            e => todo!("{:#?}", e),
        }
    }
}

/// The interpreter
impl ByteCode {
    pub(crate) fn run(
        &self,
        functions: &FxHashMap<Box<str>, Self>,
        locals: &mut [Box<dyn ScriptType>],
        args: &[&dyn ScriptType],
        env: &Environment,
    ) -> CallResult<RunError> {
        if args.len() != self.param_count as usize {
            return Err(RunError::IncorrectArgumentCount);
        }
        let mut vars = Vec::with_capacity(self.var_count as usize);
        for a in args.iter() {
            vars.push(a.dup());
        }
        vars.resize_with(self.var_count as usize, || {
            Box::new(()) as Box<dyn ScriptType>
        });
        let mut ip = 0;
        let mut iterators = Vec::new();
        loop {
            let err_roob = || RunError::RegisterOutOfBounds;
            let err_uf = || RunError::UndefinedFunction;
            let err_env = |e| RunError::EnvironmentError(e);
            let err_call = |e| RunError::CallError(Box::new(e));
            if let Some(instr) = self.code.get(ip as usize) {
                ip += 1;
                use Instruction::*;
                match instr {
					Call(box (reg, CallArgs { store_in, func, args })) => {
                        let r = {
                            let mut ca = Vec::with_capacity(args.len());
                            for &a in args.iter() {
                                ca.push(vars.get(a as usize).ok_or(err_roob())?.as_ref());
                            }
							let obj = vars.get(*reg as usize).ok_or(err_roob())?.as_ref();
							obj.call(func, &ca[..]).map_err(err_call)?
                        };
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
					}
                    CallGlobal(box CallArgs {
                        store_in,
                        func,
                        args,
                    }) => {
                        let r = {
                            let mut ca = Vec::with_capacity(args.len());
                            for &a in args.iter() {
                                ca.push(vars.get(a as usize).ok_or(err_roob())?.as_ref());
                            }
                            env.call(func, &ca[..]).map_err(err_env)?
                        };
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
                    }
                    CallSelf(box CallArgs {
                        store_in,
                        func,
                        args,
                    }) => {
                        let r = {
                            let mut ca = Vec::with_capacity(args.len());
                            for &a in args.iter() {
                                ca.push(vars.get(a as usize).ok_or(err_roob())?.as_ref());
                            }
                            let r = functions.get(func).ok_or(RunError::UndefinedFunction)?;
                            r.run(functions, locals, &ca[..], env)?
                        };
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
                    }
                    RetSome => break Ok(vars.first().ok_or(RunError::RegisterOutOfBounds)?.dup()),
                    RetNone => break Ok(Box::new(())),
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
                    Mul(r, a, b) => {
                        let a = vars.get(*a as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let b = vars.get(*b as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let e = (a * b).map_err(|e| RunError::CallError(Box::new(e)))?;
                        *vars
                            .get_mut(*r as usize)
                            .ok_or(RunError::RegisterOutOfBounds)? = e;
                    }
                    Add(r, a, b) => {
                        let a = vars.get(*a as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let b = vars.get(*b as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let e = (a + b).map_err(|e| RunError::CallError(Box::new(e)))?;
                        *vars
                            .get_mut(*r as usize)
                            .ok_or(RunError::RegisterOutOfBounds)? = e;
                    }
                    DupConst(r, c) => {
                        *vars.get_mut(*r as usize).ok_or(err_roob())? = c.dup();
                    }
                    _ => todo!("{:?}", instr),
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
