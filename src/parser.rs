use crate::error::{Error, Result};
use crate::expression::*;
use crate::tokenizer::{Token, TokenType};
use crate::value::Value;

pub(crate) fn parse(tokens: &[Token]) -> Result<BoxedExpr> {
    let mut parser = Parser {
        tokens,
        read_index: 0,
        line: 1,
        column: 1,
    };

    let expr = parser.parse_expr()?;

    if parser.read_index < parser.tokens.len() {
        return parser.error("unexpected code after expression")
    }

    Ok(expr)
}

struct Parser<'a> {
    tokens: &'a[Token],
    read_index: usize,
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_or()?;
        let line = self.line;
        let column = self.column;

        while self.consume_if(TokenType::Question) {
            let when_true = self.parse_or()?;
            if !self.consume_if(TokenType::Colon) {
                return self.error("expected colon following terenary")
            }
            let when_false = self.parse_expr()?;

            expr = Box::new(ConditionalExpr {
                line,
                column,
                condition: expr,
                when_true,
                when_false,
            });
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_and()?;

        while self.consume_if(TokenType::Or) {
            expr = Box::new(BinaryExpr {
                line: self.line,
                column: self.column,
                left: expr,
                operator: BinaryOperator::Or,
                right: self.parse_and()?,
            });
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_relative()?;

        while self.consume_if(TokenType::And) {
            expr = Box::new(BinaryExpr {
                line: self.line,
                column: self.column,
                left: expr,
                operator: BinaryOperator::And,
                right: self.parse_relative()?,
            });
        }

        Ok(expr)
    }

    fn parse_relative(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_addition()?;

        loop {
            match self.peek() {
                Some(t) => {
                    let operator = match t {
                        TokenType::GreaterThan => BinaryOperator::GreaterThan,
                        TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                        TokenType::LessEqual => BinaryOperator::LessEqual,
                        TokenType::LessThan => BinaryOperator::LessThan,
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        TokenType::NotEqual => BinaryOperator::NotEqual,
                        TokenType::In => BinaryOperator::In,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
                        line: self.line,
                        column: self.column,
                        left: expr,
                        operator,
                        right: self.parse_addition()?,
                    });
                },
                None => break,
            }
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_multiplication()?;

        loop {
            match self.peek() {
                Some(t) => {
                    let operator = match t {
                        TokenType::Plus => BinaryOperator::Add,
                        TokenType::Minus => BinaryOperator::Sub,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
                        line: self.line,
                        column: self.column,
                        left: expr,
                        operator,
                        right: self.parse_multiplication()?,
                    });
                },
                None => break,
            }
        }

        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_unary()?;

        loop {
            match self.peek() {
                Some(t) => {
                    let operator = match t {
                        TokenType::Star => BinaryOperator::Multiply,
                        TokenType::Slash => BinaryOperator::Divide,
                        TokenType::Percent => BinaryOperator::Remainder,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
                        line: self.line,
                        column: self.column,
                        left: expr,
                        operator,
                        right: self.parse_unary()?,
                    });
                },
                None => break,
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<BoxedExpr> {
        let operator = match self.peek() {
            Some(TokenType::Not) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Negate,
            _ => return self.parse_member()
        };
        self.consume();

        Ok(Box::new(UnaryExpr {
            line: self.line,
            column: self.column,
            operator,
            expr: self.parse_member()?,
        }))
    }

    fn parse_member(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.consume_if(TokenType::Dot) {
                let line = self.line;
                let column = self.column;

                let ident = match self.consume() {
                    Some(TokenType::Identifier(s)) => s,
                    _ => return self.error("expected identifier following '.'"),
                };

                expr = Box::new(PropertyExpr {
                    line,
                    column,
                    expr,
                    property: ident.to_owned(),
                });
            } else if self.consume_if(TokenType::LParen) {
                let line = self.line;
                let column = self.column;

                expr = Box::new(CallExpr {
                    line,
                    column,
                    expr,
                    arguments: self.parse_function_args()?,
                });
            } else {
                break
            }
        }

        Ok(expr)
    }

    /// Expects opening '(' to already be consumed
    fn parse_function_args(&mut self) -> Result<Vec<BoxedExpr>> {
        let mut arguments = Vec::new();

        loop {
            if self.consume_if(TokenType::RParen) {
                break
            }

            arguments.push(self.parse_expr()?);

            if self.consume_if(TokenType::Comma) {
                continue
            }

            if self.consume_if(TokenType::RParen) {
                break
            } else {
                return self.error("expected closing parenthesis after arguments")
            }
        }

        Ok(arguments)
    }

    fn parse_primary(&mut self) -> Result<BoxedExpr> {
        let token = match self.consume() {
            Some(t) => t,
            None => return self.error("expected expression"),
        };

        match token {
            TokenType::LParen => {
                let expr = self.parse_expr()?;
                if !self.consume_if(TokenType::RParen) {
                    return self.error("expected closing parenthesis")
                }

                Ok(Box::new(GroupingExpr {
                    expr,
                }))
            },

            TokenType::Dot => {
                let identifier = match self.peek() {
                    Some(TokenType::Identifier(s)) => s.to_owned(),
                    _ => return self.error("expected identifier for global resolution"),
                };

                self.consume();
                Ok(Box::new(GlobalExpr { identifier }))
            },

            TokenType::Identifier(s) => Ok(Box::new(IdentifierExpr {
                identifier: s.to_owned(),
            })),

            other => {
                match Value::try_from(other) {
                    Ok(value) => Ok(Box::new(ValueExpr {
                        value,
                    })),
                    Err(_) => self.error("expected expression"),
                }
            },

        }
    }

    fn peek(&self) -> Option<&TokenType> {
        self.get_at(self.read_index)
    }

    fn get_at(&self, index: usize) -> Option<&TokenType> {
        self.tokens.get(index).map(|tok| tok.ty())
    }

    fn consume(&mut self) -> Option<&TokenType> {
        match self.tokens.get(self.read_index) {
            Some(t) => {
                self.read_index += 1;
                self.line = t.line();
                self.column = t.column();
                Some(t.ty())
            },
            None => None,
        }
    }

    fn consume_if(&mut self, variant: TokenType) -> bool {
        use std::mem::discriminant;

        match self.peek() {
            Some(t) if discriminant(t) == discriminant(&variant) => {
                self.consume();
                true
            },
            _ => false,
        }
    }

    fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::new(self.line, self.column, msg))
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokenizer;

    #[test]
    fn grammar() {
        let expressions = [
            "one ? two : three",
            "one && two",
            "one > two || one >= two || one < two || one <= two",
            "one == two || one != two || one in two",
            "one + two - three / four % five",
            "!one",
            "-one",
            "one.two.three",
            "one()",
            "one(two, three, four)",
            ".one.two.three",
            ".one.two.three()",
            "(one + two + three)",
            "1",
            "1.0",
            "true && false",
            "'test'",
            "b'test'",
            "null",
        ];

        for string in expressions {
            let mut tokens = Vec::new();
            let mut tokenizer = Tokenizer::new(string);
            while let Some(result) = tokenizer.next_token() {
                tokens.push(result.unwrap());
            }

            match super::parse(&tokens) {
                Err(e) => panic!("failed to parse \"{}\" with error \"{}\"", string, e),
                _ => {},
            }
        }
    }
}
