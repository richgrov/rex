mod environment;
mod error;
mod expression;
mod parser;
mod tokenizer;
mod vm;

pub use environment::{Environment, Function, OptimizationLevel};
pub use error::Error;

pub struct Expression<'a> {
    bc: Vec<expression::ByteCode>,
    env: &'a Environment,
}

impl<'a> Expression<'a> {
    pub fn eval(&self, input: &[f64]) -> Result<f64, Error> {
        vm::eval(&self.bc, input, self.env)
    }

    pub fn code_dump(&self) -> String {
        use std::fmt::Write;
        let mut str = String::new();

        for bc in &self.bc {
            write!(&mut str, "{}\n", bc).unwrap();
        }
        str
    }
}

pub fn compile<'a>(src: &str, env: &'a Environment) -> Result<Expression<'a>, Error> {
    let tokens = tokenizer::tokenize(src)?;
    let mut expr = parser::parse(&tokens)?;

    if env.optimization_level >= OptimizationLevel::Basic {
        if let Some(e) = expr.fold(env) {
            expr = e;
        }
    }

    let mut bc = Vec::with_capacity(64);
    expr.emit_bytecode(env, &mut bc)?;

    Ok(Expression {
        bc,
        env,
    })
}
