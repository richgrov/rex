use crate::error::{Error, Result};
use crate::expression::*;
use crate::tokenizer::Token;
use crate::value::Value;

pub(crate) fn parse(tokens: &[Token]) -> Result<BoxedExpr> {
    let mut parser = Parser {
        tokens,
        read_index: 0,
    };

    let expr = parser.parse_expr()?;

    if parser.read_index < parser.tokens.len() {
        return Err(Error::new(0, 0, "unexpected code after expression"))
    }

    Ok(expr)
}

struct Parser<'a> {
    tokens: &'a[Token],
    read_index: usize,
}

impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_or()?;

        while self.consume_if(Token::Question) {
            let when_true = self.parse_or()?;
            if !self.consume_if(Token::Colon) {
                return Err(Error::new(0, 0, "expected colon following terenary"))
            }
            let when_false = self.parse_expr()?;

            expr = Box::new(ConditionalExpr {
                condition: expr,
                when_true,
                when_false,
            });
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_and()?;

        while self.consume_if(Token::Or) {
            expr = Box::new(BinaryExpr {
                left: expr,
                operator: BinaryOperator::Or,
                right: self.parse_and()?,
            });
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_relative()?;

        while self.consume_if(Token::And) {
            expr = Box::new(BinaryExpr {
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
                        Token::GreaterThan => BinaryOperator::GreaterThan,
                        Token::GreaterEqual => BinaryOperator::GreaterEqual,
                        Token::LessEqual => BinaryOperator::LessEqual,
                        Token::LessThan => BinaryOperator::LessThan,
                        Token::EqualEqual => BinaryOperator::Equal,
                        Token::NotEqual => BinaryOperator::NotEqual,
                        Token::In => BinaryOperator::In,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
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
                        Token::Plus => BinaryOperator::Add,
                        Token::Minus => BinaryOperator::Sub,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
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
                        Token::Star => BinaryOperator::Multiply,
                        Token::Slash => BinaryOperator::Divide,
                        Token::Percent => BinaryOperator::Remainder,
                        _ => break,
                    };
                    self.consume();

                    expr = Box::new(BinaryExpr {
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
            Some(Token::Not) => UnaryOperator::Not,
            Some(Token::Minus) => UnaryOperator::Negate,
            _ => return self.parse_member()
        };
        self.consume();

        Ok(Box::new(UnaryExpr {
            operator,
            expr: self.parse_member()?,
        }))
    }

    fn parse_member(&mut self) -> Result<BoxedExpr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.consume_if(Token::Dot) {
                let ident = match self.consume() {
                    Some(Token::Identifier(s)) => s,
                    _ => return Err(Error::new(0, 0, "expected identifier following '.'")),
                };

                expr = Box::new(AccessorExpr {
                    expr,
                    accessor: Box::new(ValueExpr {
                        value: Value::String(ident.to_owned())
                    }),
                });
            } else if self.consume_if(Token::LParen) {
                expr = Box::new(CallExpr {
                    expr,
                    arguments: self.parse_function_args()?,
                });
            } else if self.consume_if(Token::LBracket) {
                let accessor = self.parse_expr()?;
                if !self.consume_if(Token::RBracket) {
                    return Err(Error::new(0, 0, "expected closing bracket after index access"))
                }

                expr = Box::new(AccessorExpr {
                    expr,
                    accessor,
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
            if self.consume_if(Token::RParen) {
                break
            }

            arguments.push(self.parse_expr()?);

            if self.consume_if(Token::Comma) {
                continue
            }

            if self.consume_if(Token::RParen) {
                break
            } else {
                return Err(Error::new(0, 0, "expected closing parenthesis after arguments"))
            }
        }

        Ok(arguments)
    }

    fn parse_primary(&mut self) -> Result<BoxedExpr> {
        match self.consume() {
            Some(Token::LParen) => {
                let expr = self.parse_expr()?;
                if !self.consume_if(Token::RParen) {
                    return Err(Error::new(0, 0, "expected closing parenthesis"))
                }

                Ok(Box::new(GroupingExpr {
                    expr,
                }))
            },

            Some(Token::LBracket) => Ok(Box::new(ListExpr {
                expressions: self.parse_list_literal()?,
            })),
            
            Some(Token::LBrace) => Ok(Box::new(MapExpr {
                pairs: self.parse_map_literal()?,
            })),

            Some(Token::Dot) => {
                let identifier = match self.peek() {
                    Some(Token::Identifier(s)) => s.to_owned(),
                    _ => return Err(Error::new(0, 0, "expected identifier for global resolution")),
                };

                self.consume();
                Ok(Box::new(GlobalExpr { identifier }))
            },

            Some(Token::Identifier(s)) => Ok(Box::new(IdentifierExpr {
                identifier: s.to_owned(),
            })),

            Some(other) => {
                match Value::try_from(other) {
                    Ok(value) => Ok(Box::new(ValueExpr {
                        value,
                    })),
                    Err(_) => Err(Error::new(0, 0, "expected expression")),
                }
            },

            None => Err(Error::new(0, 0, "expected expression")),
        }
    }

    /// Expects opening `[` to already be consumed
    fn parse_list_literal(&mut self) -> Result<Vec<BoxedExpr>> {
        let mut expressions = Vec::new();

        loop {
            if self.consume_if(Token::RBracket) {
                break
            }
            
            expressions.push(self.parse_expr()?);

            if self.consume_if(Token::Comma) {
                continue
            }

            if self.consume_if(Token::RBracket) {
                break
            } else {
                return Err(Error::new(0, 0, "expected closing bracket after list"))
            }
        }

        Ok(expressions)
    }

    /// Expects opening `{` to already be consumed
    fn parse_map_literal(&mut self) -> Result<Vec<(BoxedExpr, Box<dyn Expr>)>> {
        let mut pairs = Vec::new();

        loop {
            if self.consume_if(Token::RBrace) {
                break
            }

            let key = self.parse_expr()?;
            if !self.consume_if(Token::Colon) {
                return Err(Error::new(0, 0,"expected colon after map key"))
            }
            let value = self.parse_expr()?;
            pairs.push((key, value));

            if self.consume_if(Token::Comma) {
                continue
            }

            if self.consume_if(Token::RBrace) {
                break
            } else {
                return Err(Error::new(0, 0, "expected closing brace after map"))
            }
        }

        Ok(pairs)
    }

    fn peek(&self) -> Option<&Token> {
        self.get_at(self.read_index)
    }

    fn get_at(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }

    fn consume(&mut self) -> Option<&Token> {
        match self.tokens.get(self.read_index) {
            Some(t) => { self.read_index += 1; Some(t) },
            None => None,
        }
    }

    fn consume_if(&mut self, variant: Token) -> bool {
        use std::mem::discriminant;

        match self.peek() {
            Some(t) if discriminant(t) == discriminant(&variant) => {
                self.consume();
                true
            },
            _ => false,
        }
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
            "one[two]",
            ".one.two.three",
            ".one.two.three()",
            ".one.two.three[four]",
            "(one + two + three)",
            "[]",
            "[one, 'two', three]",
            "[one, 'two', three,]",
            "{}",
            "{one: two, 'three': four, five: six}",
            "{one: two, 'three': four, five: six,}",
            "1",
            "1u",
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
