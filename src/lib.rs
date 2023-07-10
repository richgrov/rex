mod environment;
mod error;
mod expression;
mod parser;
mod tokenizer;
mod vm;

pub use environment::{Environment, Function};
pub use error::Error;

pub struct Expression<'a> {
    bc: Vec<expression::ByteCode>,
    env: &'a Environment,
}

impl<'a> Expression<'a> {
    pub fn eval(&self, input: &[f64]) -> Result<f64, Error> {
        vm::eval(&self.bc, input, self.env)
    }
}

pub fn compile<'a>(src: &str, env: &'a Environment) -> Result<Expression<'a>, Error> {
    let tokens = tokenizer::tokenize(src)?;
    let expr = parser::parse(&tokens)?;

    let mut bc = Vec::with_capacity(64);
    expr.emit_bytecode(env, &mut bc)?;

    Ok(Expression {
        bc,
        env,
    })
}
