use crate::error::{Error, Result};

const FORM_FEED: char = '\u{000c}';

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
    LParen,
    RParen,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Comma,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    If,
    Else,
    Identifier(String),
    Number(f64),
}

#[derive(Debug)]
pub(crate) struct Token {
    ty: TokenType,
    line: usize,
    /// The position of the first character that was found to parse the token
    column: usize,
}

impl Token {
    pub fn ty(&self) -> &TokenType {
        &self.ty
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

pub(crate) fn tokenize(text: &str) -> Result<Vec<Token>> {
    let mut tokenizer = Tokenizer {
        text: text.chars().collect(),
        read_index: 0,
        line: 1,
        // Consuming the first character will set the column to 1
        column: 0,

        token_start_line: 1,
        token_start_column: 0,
    };

    let mut tokens = Vec::new();
    while let Some(result) = tokenizer.next_token() {
        tokens.push(result?);
    }
    Ok(tokens)
}

struct Tokenizer {
    text: Vec<char>,
    read_index: usize,

    line: usize,
    column: usize,

    token_start_line: usize,
    token_start_column: usize,
}

impl Tokenizer {
    fn next_token(&mut self) -> Option<Result<Token>> {
        self.consume_whitespace();

        self.token_start_line = self.line;
        self.token_start_column = self.column + 1;

        Some(match self.consume()? {
            '(' => self.token(TokenType::LParen),
            ')' => self.token(TokenType::RParen),
            '+' => self.token(TokenType::Plus),
            '-' => self.token(TokenType::Minus),
            '*' => self.token(TokenType::Star),
            '/' => self.token(TokenType::Slash),
            '%' => self.token(TokenType::Percent),
            ',' => self.token(TokenType::Comma),
            // https://github.com/google/cel-spec/issues/137
            '.' => self.lex_numeric('.'),

            '=' => self.token(TokenType::Equal),

            '<' => if self.consume_if(|c| c == '=').is_some() {
                self.token(TokenType::LessEqual)
            } else {
                self.token(TokenType::LessThan)
            },

            '>' => if self.consume_if(|c| c == '=').is_some() {
                self.token(TokenType::GreaterEqual)
            } else {
                self.token(TokenType::GreaterThan)
            },

            c if c.is_ascii_digit() => self.lex_numeric(c),

            c => {
                if c.is_ascii_alphabetic() || c == '_' {
                    return Some(self.lex_identifier_or_keyword(c))
                }

                self.error(&format!("unexpected character '{}'", c))
            },
        })
    }

    fn consume_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\n') | Some('\r') | Some(FORM_FEED) => {
                    self.consume();
                },
                _ => break,
            }
        }
    }

    fn consume_digits(&mut self) -> String {
        let mut chars = String::new();
        loop {
            match self.peek() {
                Some(c) if c.is_ascii_digit() => { self.consume(); chars.push(c) },
                _ => break,
            }
        }
        chars
    }

    fn consume_hex_digits(&mut self) -> String {
        let mut chars = String::new();
        loop {
            match self.peek() {
                Some(c) if c.is_ascii_hexdigit() => { self.consume(); chars.push(c) },
                _ => break,
            }
        }
        chars
    }

    /// Reads any of the following integer literals:
    /// `1`
    /// `.1`
    /// `1.0`
    /// `1.0e0`
    /// `1e0`
    /// `0x1`
    ///
    /// Expects `initial_char` to be the first character of the integer literal
    fn lex_numeric(&mut self, initial_char: char) -> Result<Token> {
        if initial_char == '0' && self.consume_if(|c| c == 'x').is_some() {
            let digits = self.consume_hex_digits();

            return if digits.is_empty() {
                self.error("\"0x\" must be followed by at least one hexadecimal character")
            } else {
                match i64::from_str_radix(&digits, 16) {
                    Ok(i) => self.token(TokenType::Number(i as f64)),
                    Err(e) => self.error_or_panic_if_debug(
                        &format!("consuming hexadecimal failed: {}", e)
                    ),
                }
            }
        }

        let mut chars = String::new();
        chars.push(initial_char);

        if initial_char == '.' {
            let digits = self.consume_digits();
            if digits.is_empty() {
                // This should panic because `initial_char` was a period. This function can only be
                // called with `initial_char` is a period if the lexer knew a number followed it
                return self.error_or_panic_if_debug("no number following decial")
            }
            chars += &digits;
        } else {
            chars += &self.consume_digits();
        
            if self.consume_if(|c| c == '.').is_some() {
                chars.push('.');

                let digits = self.consume_digits();
                if digits.is_empty() {
                    return self.error(
                        "floating-point literals must be followed by at least one number"
                    )
                }
                chars += &digits;
            }
        }

        if self.consume_if(|c| c == 'e' || c == 'E').is_some() {
            chars.push('e');

            if self.consume_if(|c| c == '+').is_none() && self.consume_if(|c| c == '-').is_some() {
                chars.push('-');
            }

            let digits = self.consume_digits();
            if digits.is_empty() {
                return self.error("exponent must be followed by at least one number")
            }
            chars += &digits;
        }

        self.token(match chars.parse() {
            Ok(n) => TokenType::Number(n),
            Err(e) => return self.error_or_panic_if_debug(
                &format!("consuming number failed: {}", e)
            ),
        })
    }

    fn lex_identifier_or_keyword(&mut self, initial_char: char) -> Result<Token> {
        let mut identifier = String::new();
        identifier.push(initial_char);

        loop {
            match self.peek() {
                Some(c) => {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        self.consume();
                        identifier.push(c);
                    } else {
                        break
                    }
                },
                _ => break,
            }
        }

        self.token(match identifier.as_str() {
            "else" => TokenType::Else,
            "if" => TokenType::If,
            _ => TokenType::Identifier(identifier),
        })
    }

    fn peek(&self) -> Option<char> {
        self.get_at(self.read_index)
    }
    
    fn get_at(&self, index: usize) -> Option<char> {
        self.text.get(index).copied()
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.peek()?;
        if c == '\n' {
            self.line += 1;
            self.column = 0; // Consuming the first character will set the column to 1
        } else {
            self.column +=1 ;
        }

        self.read_index += 1;
        Some(c)
    }

    fn consume_if<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
        match self.peek() {
            Some(c) if f(c) => self.consume(),
            _ => None,
        }
    }

    fn token(&self, ty: TokenType) -> Result<Token> {
        Ok(Token {
            ty,
            line: self.token_start_line,
            column: self.token_start_column,
        })
    }

    fn error<T>(&self, message: &str) -> Result<T> {
        Err(Error::new(self.line, self.column, message))
    }

    /// Runtime assertions that will cleanly return an error during release mode to avoid potential
    /// denial of service exploits.
    fn error_or_panic_if_debug<T>(&self, message: &str) -> Result<T> {
        if cfg!(debug_assertions) {
            panic!("{}", message);
        } else {
            self.error(message)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{tokenize, Token, TokenType};
    use TokenType::*;

    #[test]
    fn symbols() {
        let string = "
            ( ) ( ) + - * / % , =
            < <= > >= else if
        ";

        let tokens = [
            LParen, RParen, LParen, RParen, Plus, Minus, Star, Slash, Percent, Comma, Equal,
            LessThan, LessEqual, GreaterThan, GreaterEqual, Else, If,
        ];

        assert_equal_tokens(string, &tokens);
    }

    #[test]
    fn identifiers_and_spacing() {
        let string = "
            identifier\tidentifier2\r_identifier3\n\n
            _identifier_4_\u{000c}Identifier5 IdenTifier6
        ";

        let tokens = [
            Identifier("identifier".to_owned()),
            Identifier("identifier2".to_owned()),
            Identifier("_identifier3".to_owned()),
            Identifier("_identifier_4_".to_owned()),
            Identifier("Identifier5".to_owned()),
            Identifier("IdenTifier6".to_owned()),
        ];

        assert_equal_tokens(string, &tokens);

        assert_ne!(*tokenize("1dentifier").unwrap()[0].ty(), Identifier("1dentifier".to_string()));
    }

    #[test]
    fn numeric() {
        let string = "
            1 .2 0.3 .4e1 .5e+2 .6e-1
            0.7e3 0.8e+4 0.99e-5
        ";

        let tokens = [
            Number(1.0),
            Number(0.2),
            Number(0.3),
            Number(0.4e1),
            Number(0.5e2),
            Number(0.6e-1),
            Number(0.7e3),
            Number(0.8e4),
            Number(0.99e-5),
        ];

        assert_equal_tokens(string, &tokens);

        assert_error("1.");
    }

    #[test]
    fn hex() {
        let string = "0x1 0x02 0x102030 0xCafeBabe";

        let tokens = [
            Number(1.0),
            Number(2.0),
            Number(0x102030 as f64),
            Number(0xCAFEBABE as i64 as f64),
        ];

        assert_equal_tokens(string, &tokens);
    }

    #[test]
    fn token_positions() {
        let string = "myVar \nconst +\n  0.01";

        let tokens = tokenize(string).unwrap();

        fn assert_pos(token: &Token, line: usize, column: usize) {
            assert!(token.line() == line);
            assert!(token.column() == column);
        }

        assert_pos(&tokens[0], 1, 1);
        assert_pos(&tokens[1], 2, 1);
        assert_pos(&tokens[2], 2, 7);
        assert_pos(&tokens[3], 3, 3);
    }

    fn assert_equal_tokens(s: &str, tokens: &[TokenType]) {
        let actual = tokenize(s).unwrap();
        assert_eq!(actual.len(), tokens.len());

        for (i, tok) in tokens.iter().enumerate() {
            assert_eq!(*tok, *actual[i].ty(), "entry number {}", i + 1);
        }
    }

    fn assert_error(s: &str) {
        assert!(tokenize(s).is_err(), "src: {}", s);
    }
}
