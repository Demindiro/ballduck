# Ballscript - embeddable & sandbox-able scripting language for Rust

[Quickstart guide](#quickstart-guide)

[Integrating into existing projects](#integrating-into-existing-projects)

## What is Ballscript?

While Rust is great for creating stable and fast programs, the extensive
compile time checks hinder efficient prototyping or cases where you just
need to "get it done". Ballscript is intended to plug this gap by being
easy to integrate and having a simple, short syntax.

## Quickstart guide

This section briefly describes the Ballscript syntax and how to integrate
it in an existing project. For full details, check the documentation (TODO).
It is also recommended to check out the `examples/` directory.

| All indentation **must be tabs**. Using spaces will result in a parser error! |
| --- |

### "Hello, world!"

As every other language guide, we'll start with a "hello-world" example.

Create a file named `hello.bs` with the following contents:

```bs
fn main()
	print("Hello, world!")
```

You can then run it with `bs hello.bs`, which should then print out
`Hello, world!`.

### Declaring functions

Code can only be executed inside functions. A function can be declared
with the `fn` keyword. It is possible to specify one or more parameters,
which can then be accessed as variables.

```bs
fn main()
	print(length(0.5, 0.7))

fn length(x, y)
	return (x * x + y * y).sqrt()
```

### Declaring variables

Variables can declared using the `var` keyword. There are two types of local
variables:

#### Block-local variables

These variables can only be accessed within the same block or sub-blocks they
are declared in.

```bs
fn main()
	var x = "duck"
	if x == "duck"
		# x can be accessed within sub-blocks
		var y = "quack"
		print(x, " says ", y)
	# y is no longer accessible. Uncommenting the statement below will
	# cause a parser error
	#print(x, " can't say ", y)
```

#### Instance-local variables

These variables are shared between all functions in a script. The values
of each of these are unique per script instance.

```bs
var x

fn main()
	quack()
	x = "duck"
	quack()

fn quack()
	print(x, " says woof")
```

It is not possible to assign an initial value to instance variables. This may
change in the future.

#### Variable types

There are a couple of built-in types with a dedicated syntax:

##### None

The `none` type is the default value of all variables. Performing an operation
on on it will almost always cause an error. A `none` can be explicitly declared
using the `none` keyword.

##### Integer

An integer is internally represented as an `isize` and can be declared as follows:

```bs
42
13_37
0xbaff
0b1011
```

Note that multiplying an integer with a real number will return another real number.

```bs
# This expression evaluates to 561.54
13.37 * 42 
```

##### Real

A real number is internally represented as a `f64` and can be declared as follows:

```bs
4.2
13_37.01_101
0xba.ff
0b10.11
```

##### Booleans

A boolean can be created using either `true` or `false`. Comparison operators also
produce boolean values.

```bs
true
false
1 < 2 # true
2 > 3 # false
```

##### Strings

Strings can be created using two double quotes (`"`).

```bs
"This is a string"
```

##### Arrays

Arrays can be created using square brackets (`[]`). They can hold any type.

Elements of an array can be accessed using the index operator (also square
brackets).

```bs
[]
[1, 2, "beep"]
arr[1]
```

##### Dictionaries

Dictionaries can be created using curly brackets (`{}`). The values can be
any type, but keys are currently limited to strings, integers and booleans
to ensure the key is always valid. This restriction may be lifted in the
future for object types.

Elements of a dictionary can be accessed using the index operator.

```bs
{}
{1: 2, "foo": "bar"}
dict["foo"]
```

The keys in a dictionary are **not** guaranteed to be in any particular order.
The expressions used when instantiating a dictionary are evaluated in
declaration order however.

### Expressions

It is possible to do math:

```bs
fn main()
	var x = 2
	var pi = 3.14
	(pi * 2).sqrt()
```

#### Operator precedence

Operators at the top of the table will be evaluated before operators at the
bottom.

| Operator             | Description                                    |
| -------------------- | ---------------------------------------------- |
| `.`                  | Accesses a named element                       |
| `[x]`                | Indexes with the value `x`                     |
| `!`                  | Negates a value                                |
| `*`, `/`, `%`        | Multiplies, divides or takes the remainder     |
| `+`, `-`             | Adds or substacts                              |
| `<<`, `>>`           | Shifts a value to the left or right            |
| `&`                  | Performs a bitwise `and`                       |
| `^`                  | Performs a bitwise `xor`                       |
| `\|`                 | Performs a bitwise `or`                        |
| `<`, `>`, `<≃`, `>=` | Checks the relative order of two values        |
| `==`, `!=`           | Checks if two values are equivalent            |
| `&&`, `\|\|`         | Performs a short-circuit boolean `and` or `or` |

### Control flow

There are a number of statements to skip or repeat blocks of code.

### `if`, `else`, `elif`

To execute a block only if a certain condition is met, an `if` statement can
be used. This can be chained by `elif` statements which are evaluated if the
previous `if`/`elif` expression evaluated to false. An optional `else`
statement can be added at the end, which will be evaluated if all previous
expressions evaluated to false.

```bs
if cond
	print("cond is true")
elif other_cond
	print("cond is false, but other_cond is true")
else
	print("Both cond and other_cond are false")
```

The expressions must evaluate to a boolean value. Any other value will result
in an error.

### `while`

A `while` statement is much like an `if` statement, except it repeats the block
as long as the expression evaluates to `true`. You **cannot** put `elif` or
`else` statements behind it.

```bs
while cond
	print("cond is still true")
```

A `while` loop can be prematurely terminated using a `break` statement. When
nesting loops, it is possible to break out of multiple at once by specifying
an integer argument

```bs
while a: # 1
	while b: # 0
		break 1 # breaks out of loop 1
```

It is possible to skip to the end of a `while` loop with the `continue`
statement, which will cause the expression to be evaluated immediately.
Like with the `break` statement, it is possible to specify an integer
argument to break out of multiple loops.

### `for`

A `for` statement will evaluate an expression **once**. If the resulting value
can be iterated, the block following the `for` will be evaluated for each value
the iterator returns.

Integers can be used as iterators. It will return all values from 0 up to the
integer, excluding the integer itself.

```bs
for x in 4
	print(x) # 0, 1, 2, 3

for x in -4
	print(x) # 0, -1, -2, -3

for c in "abcde"
	print(c) # 'a', 'b', 'c', 'd', 'e'

for v in [1, "duck", [3]]
	print(v) # 1, "duck", [3]

for k in {1: 2, "duck": "meow"}
	print(k) # 1, "duck"
```

The `break` and `continue` statements can also be used and have the same rules as
with the `while` loop.

## Integrating into existing projects

A script can be parsed using `ballscript::parse`. This will return a
`Class` which can be used to create `Instance`s.

To call a function, you need to pass a slice with `Variant`s as argument
and an `Environment`, which defines globally accessible functions such
as `print`

```rust
let source = "\
fn vulkan_lives()\
	print(\"*stomp stomp*\")
";

let mut environment = ballscript::Environment::new();
environment
	.add_function("print".into(), Box::new(|a: &[_]| println!("{:?}", a)))
	.unwrap();

let class = ballscript::parse(source).unwrap();

let script = class.instance();

script.call("vulkan_lives", &[], &environment);
```

### The `Environment` structure

The `Environment` structure is the primary way to allow and limit what a script
can do. If you need a script to be able to interact with the filesystem, you
can expose methods such as `file_read` or `file_write`. Similary, if you do
not want the script to be able to access the filesystem, you don't expose any
methods for it at all.

| Be careful with passing objects! It is possible to define an object in Rust that accesses anything (e.g. `File` object) |
| --- |

```rust
let mut environment = ballscript::Environment::new();
environment
	.add_function("print".into(), Box::new(|a: &[_]| println!("{:?}", a)))
	.unwrap();
environment
	.add_function("explode".into(), Box::new(|_: &[_]| panic!("KABOOM")))
	.unwrap();
```

### Exposing Rust objects

To expose a Rust "object" to Ballscript, it must implement the `ScriptType`
trait. This object must then be wrapped in a `Variant` to pass it to a
script.

```rust
struct MyStruct;

impl<V> ballscript::ScriptType<V> for MyStruct
where
	V: ballscript::VariantType
{
	...
}
```

### Custom `Variant` type

It may be desireable to "extend" the default `Variant` type (e.g. a game engine
may want to add `Vector3`, `Quaternion`, ...). To do so, a type that implements
`VariantType` must be implemented and passed as a generic argument:

```rust
enum MyVariant {
	...
}

impl ballscript::VariantType for MyVariant {
	...
}

fn main() {
	...
	let class = ballscript::parse::<MyVariant>(source).unwrap();
	...
}
```
