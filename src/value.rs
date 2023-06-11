use crate::{tokenizer::Token, error::Error};

pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}

impl TryFrom<&Token> for Value {
    type Error = Error;
    
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Null => Ok(Value::Null),
            Token::Bool(b) => Ok(Value::Bool(*b)),
            Token::Int(i) => Ok(Value::Int(*i)),
            Token::Uint(u) => Ok(Value::Uint(*u)),
            Token::Float(f) => Ok(Value::Float(*f)),
            Token::String(s) => Ok(Value::String(s.to_owned())),
            Token::Bytes(b) => Ok(Value::Bytes(b.to_owned())),
            _ => Err(Error::new(0, 0, "cannot convert to value")),
        }
    }
}
