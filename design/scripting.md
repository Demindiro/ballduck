# Scripting in Ballduck

Ballduck offers a simple scripting language optimized for prototyping and
overall development speed. It is useful as a "glue" between components that
would be too tedious to implement in Rust.

To attach scripts to a Node, it needs a `ScriptComponent`. This Component can
hold any number of scripts.


## Syntax

Ballscript resembles a very simplified form of `Python`.

The indentation **must** be tabs. Using spaces will result in an error.

### Functions

A function can be declared with:

```ballscript
fn foo(bar, baz)
	pass
fn bar(qux,)
	pass
```

A function can be called with:

```ballscript
foo(13, "thirty-seven")
a = bar("quux")
```

Function names may **not** conflict with variable names.

### Variables

A variable can be declared with:

```ballscript
var qux
var quux = 42
```

A variable can be assigned to with:

```ballscript
qux = "( ^-^)"
```

Variable names may **not** conflict with function names.

### Comments

```ballscript
# This is a comment
```


## Types

All types implement the `ScriptType` component. To use your own types you
must implement `ScriptType` for said type.

### Built-in types

There are a few built-in types with a custom syntax

### Integers

Integers can be defined as

```ballscript
232
16_777_216
80
443
0xdeadbeef
0b101010101
```

By default, they use the native platform size (i.e. 64 bit on x86\_64,
32 bit on x86).


### Floating point numbers

Floating point numbers can be defined as

```ballscript
23.2
16_777.216
80
443
0xdeadb.eef
0b10101.0101
```

By default, they use the native platform size (i.e. 64 bit on x86\_64,
32 bit on x86).


### Arrays

Arrays are internally defined as `Vec<Box<dyn ScriptType>>`, which means
they can hold any type.

Arrays can be created using:

```ballscript
[]
[1, 2, 3, "Chicken!"]
["blep", 0.0,]
```


### Dictionaries

Dictionaries are internally defined as `HashMap<Box<dyn ScriptType + Hash + Eq>, Box<dyn ScriptType>>`.
Again, this means that they can hold any type. The keys mush implement `Hash`
and `Eq` though.

Dictionaries can be created using:

```ballscript
{}
{"a": 0, 5: 3.14}
{a: b,}
```

Note that floating point numbers do not implement `Eq` and hence cannot be
used as keys.


### Boolean

This type can be either `true` or `false`


## Control structures

### `if`

If the expression following the `if` statement evaluates to a boolean `true`,
the code block follwing it will be executed. If it is `false` it will skip
the code block. If it evaluates to a non-boolean type it will throw an error.

### `else`

This statement can be put behind an `if` block. If the expression in the `if`
evaluates to `false`, the block following the `else` will be executed.
Otherwise, it will be skipped.

### `elif`

This is a combination of `if` and `else`: if the previous `if` or `elif`
statement evaluates to `false`, the expression in the current `elif`
block will be evaluates as if it were an `if`. Otherwise, it is skipped.

### `while`

This is a variant of the `if` instruction that will repeatedly execute
a code block until the expression evaluates to `false`.

### `for`

This structure will iterate types that implement `ScriptIter`. If the type
does not implement `ScriptIter`, an error will be thrown.

```ballscript
for i in 10:
	...
for v in [42, "foo"]:
	...
for k in {"bar": "qux", 13: 37}:
	...
```

## Creating custom types in Rust

