/// This file will eventually be changed to lib.rs when a public API is ready

mod error;
mod tokenizer;

fn main() {
    let mut tokenizer = tokenizer::Tokenizer::new("(meme.isCool && meme.isRecent) || user.memePass == 'VIP'");
    while let Some(token) = tokenizer.next_token() {
        match token {
            Ok(t) => println!("{:?}", t),
            Err(e) => { eprintln!("{}", e); break },
        }
    }
}
