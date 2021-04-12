use ballscript::ScriptType;
use std::env;
use std::fs;
use std::io;

pub fn main() -> Result<(), io::Error> {
    let mut args = env::args();
    let exec = args.next().unwrap_or_else(|| String::from("ballscript"));
    if let Some(file) = args.next() {
        match fs::read_to_string(file) {
            Ok(source) => match ballscript::parse(&source) {
                Ok(script) => {
                    let mut script = script.instance();
                    match script.call("main", &[]) {
                        Ok(_) => Ok(()),
                        Err(e) => todo!("{:?}", e),
                    }
                }
                Err(e) => Err(e).unwrap(),
            },
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
