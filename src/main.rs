/// This file will eventually be changed to lib.rs when a public API is ready

mod error;
mod expression;
mod parser;
mod tokenizer;
mod value;

fn main() {
    let mut tokenizer = tokenizer::Tokenizer::new("(meme.isCool && meme.isRecent) || user.memePass == 'VIP'");
    let mut tokens = Vec::new();
    while let Some(result) = tokenizer.next_token() {
        match result {
            Ok(t) => tokens.push(t),
            Err(e) => {
                eprint!("error: {:?}", e);
                return
            },
        }
    }

    match parser::parse(&tokens) {
        Ok(_) => println!("OK"),
        Err(e) => eprint!("error: {:?}", e),
    }
}
