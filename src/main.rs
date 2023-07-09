use environment::{Environment, AddFunction};

/// This file will eventually be changed to lib.rs when a public API is ready

mod environment;
mod error;
mod expression;
mod parser;
mod tokenizer;
mod vm;

fn all(a: f64, b: f64) -> f64 {
    if a == 1. && b == 1. {
        1.
    } else {
        0.
    }
}

fn main() {
    let mut tokenizer = tokenizer::Tokenizer::new("all(memeIsCool, memeIsRecent)");
    let mut tokens = Vec::new();
    while let Some(result) = tokenizer.next_token() {
        match result {
            Ok(t) => tokens.push(t),
            Err(e) => {
                eprintln!("error: {:?}", e);
                return
            },
        }
    }

    let expression = match parser::parse(&tokens) {
        Ok(ex) => ex,
        Err(e) => {
            eprintln!("error: {:?}", e);
            return
        },
    };

    let mut bc = Vec::with_capacity(64);
    let mut env = Environment::new();
    env.add_local("memeIsCool");
    env.add_local("memeIsRecent");
    env.add_function("all", all);

    if let Err(e) = expression.emit_bytecode(&env, &mut bc) {
        eprintln!("compiler error: {:?}", e);
        return
    }

    let input = env.to_input(&[1.0, 0.5]);
    match vm::eval(&bc, &input) {
        Ok(f) => println!("= {}", f),
        Err(e) => eprintln!("{:?}", e),
    }
}
