use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Error {
    line: usize,
    column: usize,
    message: String,
}

impl Error {
    pub fn new(line: usize, column: usize, message: &str) -> Error {
        Error {
            line,
            column,
            message: message.to_owned(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.message)
    }
}
