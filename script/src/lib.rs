mod ast;
mod tokenizer;

use tokenizer::TokenStream;

pub fn parse(source: &str) {
    let tks = TokenStream::parse(source).unwrap();
    let ast = ast::Script::parse(tks).unwrap();
	dbg!(ast);
}
