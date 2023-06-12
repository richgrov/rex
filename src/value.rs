use crate::tokenizer::TokenType;
use crate::error::Error;

pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
}

impl TryFrom<&TokenType> for Value {
    type Error = Error;
    
    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Null => Ok(Value::Null),
            TokenType::Bool(b) => Ok(Value::Bool(*b)),
            TokenType::Int(i) => Ok(Value::Int(*i)),
            TokenType::Uint(u) => Ok(Value::Uint(*u)),
            TokenType::Float(f) => Ok(Value::Float(*f)),
            TokenType::String(s) => Ok(Value::String(s.to_owned())),
            TokenType::Bytes(b) => Ok(Value::Bytes(b.to_owned())),
            _ => Err(Error::new(0, 0, "cannot convert to value")),
        }
    }
}

pub struct EvaluationContext {
}
