use crate::error::{Error, Result};
use crate::expression::*;
use crate::tokenizer::{Token, TokenType};

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
        let mut expr = self.parse_relative()?;

        while self.consume_if(TokenType::If) {
            let condition = self.parse_relative()?;
            if !self.consume_if(TokenType::Else) {
                return self.error("expected colon following terenary")
            }
            let when_false = self.parse_expr()?;

            expr = Box::new(ConditionalExpr {
                condition,
                when_true: expr,
                when_false,
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
                        TokenType::Equal => BinaryOperator::Equal,
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
                        TokenType::Plus => BinaryOperator::Add,
                        TokenType::Minus => BinaryOperator::Sub,
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
        let mut expr = self.parse_negative()?;

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
                        left: expr,
                        operator,
                        right: self.parse_negative()?,
                    });
                },
                None => break,
            }
        }

        Ok(expr)
    }

    fn parse_negative(&mut self) -> Result<BoxedExpr> {
        if self.consume_if(TokenType::Minus) {
            Ok(Box::new(BinaryExpr {
                left: Box::new(-1.),
                operator: BinaryOperator::Multiply,
                right: self.parse_primary()?,
            }))
        } else {
            self.parse_primary()
        }
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

                Ok(expr)
            },

            TokenType::Identifier(s) => {
                let identifier = s.to_owned();
                let line = self.line;
                let column = self.column;

                if self.consume_if(TokenType::LParen) {
                    Ok(Box::new(CallExpr {
                        line,
                        column,
                        function: identifier,
                        arguments: self.parse_function_args()?,
                    }))
                } else {
                    Ok(Box::new(IdentifierExpr {
                        line,
                        column,
                        identifier
                    }))
                }
            },

            TokenType::Number(n) => Ok(Box::new(*n)),

            _ => self.error("expected expression"),
        }
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
    use crate::tokenizer::{TokenType, Token};
    use crate::expression::{ConditionalExpr, BinaryExpr, BinaryOperator, IdentifierExpr, Expr, CallExpr};

    #[test]
    fn conditional() {
        use TokenType::*;
        // one if 1 = 2 else three
        let tokens = &vec![
            Identifier("one".to_owned()), If, Number(1.), Equal, Number(2.), Else,
            Identifier("three".to_owned()),
        ];

        assert_equal_ast(&tokens, &ConditionalExpr {
            condition: Box::new(BinaryExpr {
                left: Box::new(1.),
                operator: BinaryOperator::Equal,
                right: Box::new(2.),
            }),
            when_true: Box::new(IdentifierExpr {
                line: 0,
                column: 0,
                identifier: "one".to_owned(),
            }),
            when_false: Box::new(IdentifierExpr {
                line: 0,
                column: 0,
                identifier: "three".to_owned(),
            }),
        });
    }

    #[test]
    fn math() {
        use TokenType::*;
        // -2 % 1 + 3 * (4 - 5) / 6
        let tokens = &vec![
            Minus, Number(2.), Percent, Number(1.), Plus, Number(3.), Star, LParen, Number(4.), Minus,
            Number(5.), RParen, Slash, Number(6.),
        ];

        assert_equal_ast(&tokens, &BinaryExpr {
            left: Box::new(BinaryExpr {
                left: Box::new(BinaryExpr {
                    left: Box::new(-1.),
                    operator: BinaryOperator::Multiply,
                    right: Box::new(2.),
                }),
                operator: BinaryOperator::Remainder,
                right: Box::new(1.),
            }),
            operator: BinaryOperator::Add,
            right: Box::new(BinaryExpr {
                left: Box::new(BinaryExpr {
                    left: Box::new(3.),
                    operator: BinaryOperator::Multiply,
                    right: Box::new(BinaryExpr {
                        left: Box::new(4.),
                        operator: BinaryOperator::Sub,
                        right: Box::new(5.),
                    }),
                }),
                operator: BinaryOperator::Divide,
                right: Box::new(6.),
            }),
        });
    }

    #[test]
    fn comparison() {
        use TokenType::*;
        // 1 = 2 < 3 <= 4 >= 5 < 6
        let tokens = &vec![
            Number(1.), Equal, Number(2.), LessThan, Number(3.), LessEqual, Number(4.),
            GreaterEqual, Number(5.), LessThan, Number(6.),
        ];

        assert_equal_ast(&tokens, &BinaryExpr {
            left: Box::new(BinaryExpr {
                left: Box::new(BinaryExpr {
                    left: Box::new(BinaryExpr {
                        left: Box::new(BinaryExpr {
                            left: Box::new(1.),
                            operator: BinaryOperator::Equal,
                            right: Box::new(2.),
                        }),
                        operator: BinaryOperator::LessThan,
                        right: Box::new(3.),
                    }),
                    operator: BinaryOperator::LessEqual,
                    right: Box::new(4.),
                }),
                operator: BinaryOperator::GreaterEqual,
                right: Box::new(5.),
            }),
            operator: BinaryOperator::LessThan,
            right: Box::new(6.)
        });
    }

    #[test]
    fn function() {
        use TokenType::*;
        // all(1, true, 2 + 3)
        let tokens = &vec![
            Identifier("all".to_owned()), LParen, Number(1.), Comma,
            TokenType::Identifier("true".to_owned()), Comma, Number(2.), Plus, Number(3.), RParen,
        ];

        assert_equal_ast(&tokens, &CallExpr {
            line: 0,
            column: 0,
            function: "all".to_owned(),
            arguments: vec![
                Box::new(1.),
                Box::new(IdentifierExpr {
                    line: 0,
                    column: 0,
                    identifier: "true".to_owned(),
                }),
                Box::new(BinaryExpr {
                    left: Box::new(2.),
                    operator: BinaryOperator::Add,
                    right: Box::new(3.),
                }),
            ],
        });
    }

    fn assert_equal_ast(token_types: &[TokenType], ast: &dyn Expr) {
        let tokens: Vec<Token> = token_types.iter()
            .map(|t| Token::new(t.clone(), 0, 0))
            .collect();

        let actual = super::parse(&tokens).unwrap();
        assert!(actual.values_equal(ast), "actual = {:?}", actual);
    }
}
