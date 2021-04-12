use crate::ast::{Atom, Expression, Function, Lines, Statement};
use crate::script::CallError;
use crate::tokenizer::Op;
use crate::{ScriptIter, ScriptType};
use core::convert::TryInto;
use core::fmt::{self, Debug, Formatter};
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

pub(crate) struct CallArgs {
    store_in: Option<u16>,
    func: Box<str>,
    args: Box<[u16]>,
}

pub(crate) enum Instruction {
    Call(Box<(u16, CallArgs)>),
    CallSelf(Box<CallArgs>),
    CallGlobal(Box<CallArgs>),
    //Iter(Box<dyn Iterator<Item = Rc<dyn ScriptType>>>),
    //Jmp(u32),
    //JmpIf(u16, u32),
    RetSome,
    RetNone,

    IterConst(Box<(u16, u32, Box<dyn ScriptIter>)>),
    IterJmp(u16, u32),

    /*
    AndJmp(u16, u16, u32),
    OrJmp(u16, u16, u32),
    Xor(u16, u16, u32),
    Eq,
    Neq,
    */
    Add(u16, u16, u16),
    //Sub(u16, u16, u16),
    Mul(u16, u16, u16),
    //Div(u16, u16, u16),
    //Rem(u16, u16, u16),
    Move(u16, u16),
    /*
    AddConst,
    SubConst,
    MulConst,
    DivConst,
    RemConst,
    */
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
    consts: Vec<Rc<dyn ScriptType>>,
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

pub type EnvironmentFunction = Box<dyn Fn(&[Rc<dyn ScriptType>]) -> CallResult<RunError>>;
pub type CallResult<E> = Result<Rc<dyn ScriptType>, E>;

#[derive(Debug)]
pub enum EnvironmentError {
    FunctionAlreadyExists,
    UndefinedFunction,
}

/// The parser
impl ByteCode {
    pub(crate) fn parse(
        function: Function,
        methods: &FxHashMap<&str, ()>,
        locals: &FxHashMap<Box<str>, u16>,
    ) -> Result<Self, ByteCodeError> {
        let mut instr = Vec::new();
        let mut vars = FxHashMap::with_hasher(Default::default());
        let mut consts = Vec::new();
        let param_count = function.parameters.len() as u16;
        for p in function.parameters {
            if vars.insert(p, vars.len() as u16).is_some() {
                return Err(ByteCodeError::DuplicateParameter);
            }
        }
        let mut var_count = vars.len() as u16;
        let vars = Self::parse_block(
            &function.lines,
            methods,
            locals,
            &mut instr,
            &mut vars,
            &mut consts,
            &mut var_count,
            0,
        )?;
        match instr.last() {
            Some(Instruction::RetSome) | Some(Instruction::RetNone) => (),
            _ => instr.push(Instruction::RetNone),
        }

        if consts.len() > 0 {
            // All consts are using the upper-most registers, move them downwards
            let offset = (u16::MAX - consts.len() as u16).wrapping_add(1);
            for i in instr.iter_mut() {
                use Instruction::*;
                let conv = |c: &mut u16| {
                    if *c >= offset {
                        *c = u16::MAX - *c + vars
                    }
                };
                match i {
                    Call(box (_, ca)) | CallSelf(box ca) | CallGlobal(box ca) => {
                        for a in ca.args.iter_mut() {
                            conv(a);
                        }
                    }
                    Move(_, a) => conv(a),
                    Add(_, a, b) | Mul(_, a, b) => {
                        conv(a);
                        conv(b);
                    }
                    IterConst(_) | IterJmp(_, _) | RetSome | RetNone => (),
                }
            }
        }

        Ok(Self {
            code: instr,
            var_count: vars,
            param_count,
            consts,
        })
    }

    fn parse_block<'a>(
        lines: &Lines<'a>,
        methods: &FxHashMap<&str, ()>,
        locals: &FxHashMap<Box<str>, u16>,
        instr: &mut Vec<Instruction>,
        vars: &mut FxHashMap<&'a str, u16>,
        consts: &mut Vec<Rc<dyn ScriptType>>,
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
                        let mut add_const = |v| {
                            consts.push(v);
                            u16::MAX - consts.len() as u16 + 1
                        };
                        args.push(match a {
                            Expression::Atom(a) => match a {
                                Atom::String(a) => {
                                    add_const(Rc::new(a.to_string().into_boxed_str()))
                                }
                                Atom::Integer(a) => add_const(Rc::new(*a)),
                                Atom::Real(a) => add_const(Rc::new(*a)),
                                Atom::Name(a) => todo!("call {:?}", a),
                            },
                            Expression::Function {
                                name,
                                arguments,
                                expr,
                            } => {
                                let mut args = Vec::with_capacity(arguments.len());
                                let store_in = *curr_var_count;
                                *curr_var_count += 1;
                                let og_cvc = *curr_var_count;
                                for a in arguments {
                                    match a {
                                        Expression::Atom(a) => {
                                            args.push(match a {
                                                Atom::String(a) => add_const(Rc::new(
                                                    a.to_string().into_boxed_str(),
                                                )),
                                                Atom::Integer(a) => add_const(Rc::new(*a)),
                                                Atom::Real(a) => add_const(Rc::new(*a)),
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
                    let args = Box::new(CallArgs {
                        store_in: None,
                        func: (*func).into(),
                        args: args.into_boxed_slice(),
                    });
                    instr.push(if methods.contains_key(func) {
                        Instruction::CallSelf(args)
                    } else {
                        Instruction::CallGlobal(args)
                    });
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
                        methods,
                        locals,
                        instr,
                        vars,
                        consts,
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
            Expression::Function {
                expr,
                name,
                arguments,
            } => {
                let expr = if let Some(e) = expr {
                    e
                } else {
                    todo!("none expr in fn")
                };
                let og_cvc = *curr_var_count;
                let r = *curr_var_count;
                *curr_var_count += 1;
                let e = Self::parse_expression(
                    r,
                    expr,
                    locals,
                    instr,
                    vars,
                    min_var_count,
                    curr_var_count,
                )?;
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
        locals: &mut [Rc<dyn ScriptType>],
        args: &[Rc<dyn ScriptType>],
        env: &Environment,
    ) -> CallResult<RunError> {
        if args.len() != self.param_count as usize {
            return Err(RunError::IncorrectArgumentCount);
        }
        let mut vars = Vec::with_capacity(self.var_count as usize + self.consts.len());
        for a in args.iter() {
            vars.push(a.clone());
        }
        vars.resize_with(self.var_count as usize, || {
            Rc::new(()) as Rc<dyn ScriptType>
        });
        vars.extend(self.consts.iter().cloned());
        let mut ip = 0;
        let mut iterators = Vec::new();
        let mut call_args = Vec::new();
        loop {
            let err_roob = || RunError::RegisterOutOfBounds;
            let err_uf = || RunError::UndefinedFunction;
            let err_env = |e| RunError::EnvironmentError(e);
            let err_call = |e| RunError::CallError(Box::new(e));
            if let Some(instr) = self.code.get(ip as usize) {
                ip += 1;
                use Instruction::*;
                match instr {
                    Call(box (
                        reg,
                        CallArgs {
                            store_in,
                            func,
                            args,
                        },
                    )) => {
                        for &a in args.iter() {
                            call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
                        }
                        let obj = vars.get(*reg as usize).ok_or(err_roob())?.as_ref();
                        let r = obj.call(func, &call_args[..]).map_err(err_call)?;
                        call_args.clear();
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
                    }
                    CallGlobal(box CallArgs {
                        store_in,
                        func,
                        args,
                    }) => {
                        for &a in args.iter() {
                            call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
                        }
                        let r = env.call(func, &call_args[..]).map_err(err_env)?;
                        call_args.clear();
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
                    }
                    CallSelf(box CallArgs {
                        store_in,
                        func,
                        args,
                    }) => {
                        for &a in args.iter() {
                            call_args.push(vars.get(a as usize).ok_or(err_roob())?.clone());
                        }
                        let r = functions.get(func).ok_or(RunError::UndefinedFunction)?;
                        let r = r.run(functions, locals, &call_args[..], env)?;
                        call_args.clear();
                        // SAFETY: ditto
                        if let Some(reg) = store_in {
                            *vars.get_mut(*reg as usize).ok_or(err_roob())? = r;
                        }
                    }
                    RetSome => {
                        break Ok(vars.first().ok_or(RunError::RegisterOutOfBounds)?.clone())
                    }
                    RetNone => break Ok(Rc::new(())),
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
                        let e = a.mul(b).map_err(|e| RunError::CallError(Box::new(e)))?;
                        *vars
                            .get_mut(*r as usize)
                            .ok_or(RunError::RegisterOutOfBounds)? = e;
                    }
                    Add(r, a, b) => {
                        let a = vars.get(*a as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let b = vars.get(*b as usize).ok_or(RunError::RegisterOutOfBounds)?;
                        let e = a.add(b).map_err(|e| RunError::CallError(Box::new(e)))?;
                        *vars
                            .get_mut(*r as usize)
                            .ok_or(RunError::RegisterOutOfBounds)? = e;
                    }
                    /*
                    DupConst(r, c) => {
                        *vars.get_mut(*r as usize).ok_or(err_roob())? = c.clone();
                    }
                    */
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

    pub fn call(&self, func: &str, args: &[Rc<dyn ScriptType>]) -> CallResult<EnvironmentError> {
        Ok(self
            .functions
            .get(func)
            .ok_or(EnvironmentError::UndefinedFunction)?(args)
        .unwrap())
    }
}

/// This returns each instruction on oneline instead of 5+ with the default Debug
impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            Call(box (r, a)) => write!(f, "call    {}, {:?}", r, a),
            CallSelf(a) => write!(f, "call    self, {:?}", a),
            CallGlobal(a) => write!(f, "call    env, {:?}", a),
            Move(a, b) => write!(f, "move    {:?}, {:?}", a, b),
            RetSome => write!(f, "ret     0"),
            RetNone => write!(f, "ret     none"),
            IterConst(box (r, p, i)) => write!(f, "iter    {}, {}, {:?}", r, p, i),
            IterJmp(r, p) => write!(f, "iterjmp {}, {}", r, p),
            Add(r, a, b) => write!(f, "add     {}, {}, {}", r, a, b),
            Mul(r, a, b) => write!(f, "mul     {}, {}, {}", r, a, b),
        }
    }
}

impl Debug for CallArgs {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		if let Some(n) = self.store_in {
			write!(f, "{}, \"{}\", {:?}", n, self.func, self.args)
		} else {
			write!(f, "none, \"{}\", {:?}", self.func, self.args)
		}
    }
}
