use core::ops::Deref;
use std::rc::Rc;

/// A string that is either a slice or a [`String`](std::str::String)
#[derive(Clone, Debug)]
pub(crate) enum Str<'a> {
	Slice(&'a str),
	Alloc(Rc<str>),
}

impl<'a> From<&'a str> for Str<'a> {
	fn from(from: &'a str) -> Self {
		Str::Slice(from)
	}
}

impl From<Rc<str>> for Str<'_> {
	fn from(from: Rc<str>) -> Self {
		Str::Alloc(from)
	}
}

impl From<Str<'_>> for Rc<str> {
	fn from(from: Str) -> Self {
		(&*from).into()
	}
}

impl Deref for Str<'_> {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		match self {
			Str::Slice(s) => s,
			Str::Alloc(s) => s,
		}
	}
}

impl PartialEq for Str<'_> {
	fn eq(&self, rhs: &Self) -> bool {
		self == rhs
	}
}
