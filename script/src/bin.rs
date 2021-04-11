use std::env;
use std::fs;
use std::io;

pub fn main() -> Result<(), io::Error> {
    let mut args = env::args();
    let exec = args.next().unwrap_or_else(|| String::from("ballscript"));
    if let Some(file) = args.next() {
        match fs::read_to_string(file) {
            Ok(source) => {
                let script = ballscript::parse(&source);
                dbg!(script);
                Ok(())
            }
            Err(e) => Err(e),
        }
    } else {
        eprintln!("Usage: {} <file>", exec);
        Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No script file specified",
        ))
    }
}
