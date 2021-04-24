# Integrating into existing projects

## Parsing scripts

A script can be parsed using `ballscript::parse`. This will return a
`Class` which can be used to create `Instance`s.

To call a function, you need to pass a slice with `Variant`s as argument
and an `Environment`, which defines globally accessible functions such
as `print`

```rust
let source = "\
fn vulkan_lives()\
	env.print(\"*stomp stomp*\")
";

let mut environment = ballscript::Environment::new();
environment
	.add_function("print".into(), Box::new(|a: &[_]| println!("{:?}", a)))
	.unwrap();

let class = ballscript::parse(source, ()).unwrap();

let script = class.instance();

script.call("vulkan_lives", &[], &environment);
```

## The `Environment` structure

The `Environment` structure is the primary way to allow and limit what a script
can do. If you need a script to be able to interact with the filesystem, you
can expose methods such as `file_read` or `file_write`. Similary, if you do
not want the script to be able to access the filesystem, you don't expose any
methods for it at all.

```rust
let mut environment = ballscript::Environment::new();
environment
	.add_function("print".into(), Box::new(|a: &[_]| println!("{:?}", a)))
	.unwrap();
environment
	.add_function("explode".into(), Box::new(|_: &[_]| panic!("KABOOM")))
	.unwrap();
```

## Exposing Rust objects

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

| Be careful with passing objects! It is possible to define an object in |
| Rust that accesses anything (e.g. `File` object), which may bypass any |
| `Environment` sandboxing.                                              |
| ---------------------------------------------------------------------- |

## Tracing / profiling

It may be desireable to inspect (or even modify) the interpreter loop to
debug issues. To do so, a struct implementing `Tracer` can be passed to
`ballscript::parse`. Any time a script with a `Tracer` attached is executed
the tracer will receive callbacks.

An example tracer that logs all instructions being executed could look like this:

```rust
#[derive(Clone)]
struct Logger;

impl ballscript::Tracer<ballscript::Variant> for Logger {
		fn instruction_pre(
			&self,
			bytecode: &bs::ByteCode<V>,
			ip: u32,
			instruction: &bs::Instruction,
		) {
			print!("[{}] {:>4} | {:?}", bytecode.name(), ip, instruction);
		}

		// other required method impls skipped for brevity
}
```

The default interpreter has a feature `print-instructions` that can be enabled
to print every instruction the interpreter loop executes, as well as when a
call is performed and the values of the registers that are being operated on.

## Custom `Variant` type

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
