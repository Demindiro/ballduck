// Copyright (C) 2021  David Hoppenbrouwers
//
// This file is licensed under the MIT license. See LICENSE for details.

use super::{ByteCode, Instruction, RunState};
use crate::std_types::Rc;
use crate::VariantType;
use std::error::Error;

/// This trait is used to inspect the execution of bytecode. It can trace every
/// instruction that is being executed & follow calls.
pub trait Tracer<V>: Clone + 'static
where
	V: VariantType,
{
	/// Called before the execution of a single instruction
	fn instruction_pre(&self, bytecode: &ByteCode<V>, ip: u32, instruction: &Instruction);

	/// Called after the execution of a single instruction
	fn instruction_post(&self, bytecode: &ByteCode<V>, ip: u32, instruction: &Instruction);

	/// Called right before a function call
	fn call_pre(&self, bytecode: &ByteCode<V>, function: &Rc<str>);

	/// Called right after a function call
	fn call_post(&self, bytecode: &ByteCode<V>, function: &Rc<str>);

	/// Called right before a function call
	fn call_self_pre(&self, bytecode: &ByteCode<V>, function: u16);

	/// Called right after a function call
	fn call_self_post(&self, bytecode: &ByteCode<V>, function: u16);

	/// Called at the start of a bytecode run
	fn run_pre(&self, bytecode: &ByteCode<V>);

	/// Called at the end of a bytecode run
	fn run_post(&self, bytecode: &ByteCode<V>);

	/// Called every iteration to allow reading and modifying the [`RunState`](super::RunState)
	/// It is called right after instruction_pre but before the instruction is actually called.
	fn peek(&self, bytecode: &ByteCode<V>, state: &mut RunState<V>);

	/// Called whenever an error occurs in the inner loop
	fn error(&self, bytecode: &ByteCode<V>, state: &mut RunState<V>, error: &dyn Error);
}

/// Default implementation that does nothing and thus has no performance impact.
impl<V> Tracer<V> for ()
where
	V: VariantType,
{
	#[inline(always)]
	fn instruction_pre(&self, _: &ByteCode<V>, _: u32, _: &Instruction) {}

	#[inline(always)]
	fn instruction_post(&self, _: &ByteCode<V>, _: u32, _: &Instruction) {}

	#[inline(always)]
	fn call_pre(&self, _: &ByteCode<V>, _: &Rc<str>) {}

	#[inline(always)]
	fn call_post(&self, _: &ByteCode<V>, _: &Rc<str>) {}

	#[inline(always)]
	fn call_self_pre(&self, _: &ByteCode<V>, _: u16) {}

	#[inline(always)]
	fn call_self_post(&self, _: &ByteCode<V>, _: u16) {}

	#[inline(always)]
	fn run_pre(&self, _: &ByteCode<V>) {}

	#[inline(always)]
	fn run_post(&self, _: &ByteCode<V>) {}

	#[inline(always)]
	fn peek(&self, _: &ByteCode<V>, _: &mut RunState<V>) {}

	#[inline(always)]
	fn error(&self, _: &ByteCode<V>, _: &mut RunState<V>, _: &dyn Error) {}
}

/// Starts tracing a run and automatically calls `run_pos` when it is dropped.
/// Useful in conjuction with the `?` syntax and panics.
pub(super) struct TraceRun<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	bytecode: &'a ByteCode<V>,
	tracer: &'a T,
}

impl<'a, V, T> TraceRun<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	pub fn new(tracer: &'a T, bytecode: &'a ByteCode<V>) -> Self {
		tracer.run_pre(bytecode);
		Self { bytecode, tracer }
	}
}

impl<V, T> Drop for TraceRun<'_, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	fn drop(&mut self) {
		self.tracer.run_post(self.bytecode);
	}
}

/// Starts tracing a call and automatically calls `call_pos` when it is dropped.
/// Useful in conjuction with the `?` syntax and panics.
pub(super) struct TraceCall<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	bytecode: &'a ByteCode<V>,
	tracer: &'a T,
	function: &'a Rc<str>,
}

impl<'a, V, T> TraceCall<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	pub fn new(tracer: &'a T, bytecode: &'a ByteCode<V>, function: &'a Rc<str>) -> Self {
		tracer.call_pre(bytecode, function);
		Self {
			bytecode,
			tracer,
			function,
		}
	}
}

impl<V, T> Drop for TraceCall<'_, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	fn drop(&mut self) {
		self.tracer.call_post(self.bytecode, self.function);
	}
}

/// Starts tracing a local call and automatically calls `call_pos` when it is dropped.
/// Useful in conjuction with the `?` syntax and panics.
pub(super) struct TraceSelfCall<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	bytecode: &'a ByteCode<V>,
	tracer: &'a T,
	function: u16,
}

impl<'a, V, T> TraceSelfCall<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	pub fn new(tracer: &'a T, bytecode: &'a ByteCode<V>, function: u16) -> Self {
		tracer.call_self_pre(bytecode, function);
		Self {
			bytecode,
			tracer,
			function,
		}
	}
}

impl<V, T> Drop for TraceSelfCall<'_, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	fn drop(&mut self) {
		self.tracer.call_self_post(self.bytecode, self.function);
	}
}

/// Starts tracing a instruction and automatically calls `instruction_pos` when it is dropped.
/// Useful in conjuction with the `?` syntax and panics.
pub(super) struct TraceInstruction<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	bytecode: &'a ByteCode<V>,
	tracer: &'a T,
	ip: u32,
	instruction: &'a Instruction,
}

impl<'a, V, T> TraceInstruction<'a, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	pub fn new(
		tracer: &'a T,
		bytecode: &'a ByteCode<V>,
		ip: u32,
		instruction: &'a Instruction,
	) -> Self {
		tracer.instruction_pre(bytecode, ip, instruction);
		Self {
			bytecode,
			tracer,
			ip,
			instruction,
		}
	}
}

impl<V, T> Drop for TraceInstruction<'_, V, T>
where
	V: VariantType,
	T: Tracer<V>,
{
	#[inline(always)]
	fn drop(&mut self) {
		self.tracer
			.instruction_post(self.bytecode, self.ip, self.instruction);
	}
}
