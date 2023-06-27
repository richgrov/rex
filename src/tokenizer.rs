use crate::error::{Error, Result};

const BELL: char = '\u{0007}';
const BACKSPACE: char = '\u{0008}';
const VERTICAL_TAB: char = '\u{000b}';
const FORM_FEED: char = '\u{000c}';

/// All grammatical tokens defined by the CEL spec at
/// https://github.com/google/cel-spec/blob/master/doc/langdef.md#syntax
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
    And,
    Or,
    Question,
    Colon,
    Dot,
    Equal,
    EqualEqual,
    Not,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    Else,
    Function,
    If,
    Let,
    Null,
    Return,

    Identifier(String),
    Bool(bool),
    Number(f64),
    String(String),
    Bytes(Vec<u8>),
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

/// Used to differentiate string literals from byte literals during parsing. For example, '\xFF'
/// will contain character 'ÿ' encoded as [195, 191] while b'\xFF' will simply contain 0xFF.
enum StringOrBytes {
    String(String),
    Bytes(Vec<u8>),
}

impl StringOrBytes {
    fn push_u8(&mut self, u: u8) {
        match self {
            StringOrBytes::String(s) => s.push(u as char),
            StringOrBytes::Bytes(b) => b.push(u),
        }
    }

    fn push_char(&mut self, c: char) {
        match self {
            StringOrBytes::String(s) => s.push(c),
            StringOrBytes::Bytes(b) => {
                let mut slice = [0; 4];
                b.extend_from_slice(c.encode_utf8(&mut slice).as_bytes());
            }
        }
    }
}

/// An [`Iterator`] that converts a string of characters into [`Token`]s.
pub(crate) struct Tokenizer {
    text: Vec<char>,
    read_index: usize,

    line: usize,
    column: usize,

    token_start_line: usize,
    token_start_column: usize,
}

impl Tokenizer {
    pub fn new(text: &str) -> Tokenizer {
        Tokenizer {
            text: text.chars().collect(),
            read_index: 0,
            line: 1,
            // Consuming the first character will set the column to 1
            column: 0,

            token_start_line: 1,
            token_start_column: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Result<Token>> {
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
            '?' => self.token(TokenType::Question),
            ':' => self.token(TokenType::Colon),
            // https://github.com/google/cel-spec/issues/137
            '.' => if let Some(_) = self.next_is(|c| c.is_ascii_digit()) {
                self.lex_numeric('.')
            } else {
                self.token(TokenType::Dot)
            },

            '=' => if self.consume_if(|c| c == '=').is_some() {
                self.token(TokenType::EqualEqual)
            } else {
                self.token(TokenType::Equal)
            },

            '!' => if self.consume_if(|c| c == '=').is_some() {
                self.token(TokenType::NotEqual)
            } else {
                self.token(TokenType::Not)
            },

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

            '&' => match self.consume() {
                Some('&') => self.token(TokenType::And),
                Some(_) => self.error(
                    "unexpected character following '&'. Only logical AND (&&) operator is supported"
                ),
                None => self.error_eof_during("logical AND operator"),
            },
            '|' => match self.consume() {
                Some('|') => self.token(TokenType::Or),
                Some(_) => self.error(
                    "unexpected character following '|'. Only logical OR (||) operator is supported"
                ),
                None => self.error_eof_during("logical OR operator"),
            },

            c if c.is_ascii_digit() => self.lex_numeric(c),

            c => {
                if let Some(tok) = self.try_lex_string(c) {
                    return Some(tok)
                }

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

    fn consume_hex_digits_into_u32(&mut self, num_digits: usize) -> Result<u32> {
        let mut chars = String::with_capacity(num_digits);
        for _ in 0..num_digits {
            match self.consume() {
                Some(c) if c.is_ascii_hexdigit() => chars.push(c),
                _ => return self.error(
                    &format!("expected {} hex digits", num_digits)
                ),
            }
        }

        match u32::from_str_radix(&chars, 16) {
            Ok(i) => Ok(i),
            Err(e) => self.error_or_panic_if_debug(
                &format!("consuming {} hex digits failed: {}", num_digits, e)
            ),
        }
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
            "false" => TokenType::Bool(false),
            "function" => TokenType::Function,
            "if" => TokenType::If,
            "let" => TokenType::Let,
            "null" => TokenType::Null,
            "return" => TokenType::Return,
            "true" => TokenType::Bool(true),
            _ => TokenType::Identifier(identifier),
        })
    }

    /// Tries to treat the next sequence characters as a string. If an error occurs *before*
    /// reading the quotations, the read index will remain unchanged and None is returned. If an
    /// error occurs *after* reading the quotations, Some(Err) is returned.
    ///
    /// Expects the first character of the string to already be consumed.
    fn try_lex_string(&mut self, initial_char: char) -> Option<Result<Token>> {
        let mut index = 0;
        let next_three = [
            Some(initial_char),
            self.peek(),
            self.get_at(self.read_index + 1),
        ];

        let byte_literal = if next_three[index] == Some('b') || next_three[index] == Some('B') {
            index += 1;
            true
        } else {
            false
        };

        let raw_literal = if next_three[index] == Some('r') || next_three[index] == Some('R') {
            index += 1;
            true
        } else {
            false
        };

        match next_three[index] {
            Some(c) if c == '\'' || c == '"' => {
                self.read_index += index;
                let mut buffer = if byte_literal {
                    StringOrBytes::Bytes(Vec::new())
                } else {
                    StringOrBytes::String(String::new())
                };

                Some(match self.read_string_literal(c, raw_literal, &mut buffer) {
                    Ok(_) => self.token(match buffer {
                        StringOrBytes::String(s) => TokenType::String(s),
                        StringOrBytes::Bytes(b) => TokenType::Bytes(b),
                    }),
                    Err(e) => Err(e),
                })
            },
            _ => None,
        }
    }

    /// Reads a string literal *without* the [b][r] prefix. Expects the *first* opening quote to be
    /// passed to `quote_type`.
    fn read_string_literal(
        &mut self, quote_type: char, ignore_escapes: bool, buffer: &mut StringOrBytes,
    ) -> Result<()>
    {
        debug_assert!(quote_type == '\'' || quote_type == '"');

        let triple_quote = if self.consume_if(|c| c == quote_type).is_some() {
            if self.consume_if(|c| c == quote_type).is_some() {
                true
            } else {
                return Ok(())
            }
        } else {
            false
        };

        loop {
            match self.consume() {
                Some('\\') => {
                    if ignore_escapes {
                        buffer.push_char('\\');
                        continue
                    }

                    self.consume_string_escape(buffer)?;
                },
                Some('\r') => {
                    if !triple_quote {
                        return self.error(
                            "unexpected end of line while reading string. Insert closing quotation or use \
                            triple-quotes to span multiple lines"
                        )
                    }
                    buffer.push_char('\r');
                },
                Some('\n') => {
                    if !triple_quote {
                        return self.error(
                            "unexpected end of line while reading string. Insert closing quotation or use \
                            triple-quotes to span multiple lines"
                        )
                    }
                    self.line += 1;
                    buffer.push_char('\n');
                },
                Some(c) => {
                    if c == quote_type {
                        if !triple_quote {
                            break
                        }

                        if self.peek() == Some(quote_type) && self.get_at(self.read_index + 1) == Some(quote_type) {
                            self.consume();
                            self.consume();
                            break
                        }
                    }

                    buffer.push_char(c);
                },
                None => return self.error_eof_during("string"),
            }
        }

        Ok(())
    }

    /// Expects the leading `\` character to already be consumed
    fn consume_string_escape(&mut self, buffer: &mut StringOrBytes) -> Result<()> {
        match self.consume() {
            Some('\\') => buffer.push_char('\\'),
            Some('?') => buffer.push_char('?'),
            Some('"') => buffer.push_char('"'),
            Some('\'') => buffer.push_char('\''),
            Some('`') => buffer.push_char('`'),

            Some('a') => buffer.push_char(BELL),
            Some('b') => buffer.push_char(BACKSPACE),
            Some('f') => buffer.push_char(FORM_FEED),
            Some('n') => buffer.push_char('\n'),
            Some('r') => buffer.push_char('\r'),
            Some('t') => buffer.push_char('\t'),
            Some('v') => buffer.push_char(VERTICAL_TAB),

            Some('u') => {
                if let StringOrBytes::Bytes(_) = buffer {
                    return self.error("\\u in strings is not defined for byte literals")
                }

                let hex_number = self.consume_hex_digits_into_u32(4)?;
                match char::from_u32(hex_number) {
                    Some(c) => buffer.push_char(c),
                    None => return self.error("not a valid unicode code point"),
                }
            },
            Some('U') => {
                if let StringOrBytes::Bytes(_) = buffer {
                    return self.error("\\u in strings is not defined for byte literals")
                }

                let hex_number = self.consume_hex_digits_into_u32(8)?;
                match char::from_u32(hex_number) {
                    Some(c) => buffer.push_char(c),
                    None => return self.error("not a valid unicode code point"),
                }
            },
            Some('x') | Some('X') => {
                let hex_number = self.consume_hex_digits_into_u32(2)? as u8;
                buffer.push_u8(hex_number);
            },
            Some(other) => {
                let mut octals = String::with_capacity(3);
                if !other.is_ascii_digit() {
                    return self.error("invalid character following string escape")
                }
                octals.push(other);

                for _ in 0..2 {
                    match self.consume() {
                        Some(c) if c.is_ascii_digit() => {
                            octals.push(c)
                        },
                        _ => return self.error("")
                    }
                }

                match u8::from_str_radix(&octals, 8) {
                    Ok(u) => buffer.push_u8(u),
                    Err(_) => return self.error("invalid octal"),
                }
            },
            None => return self.error_eof_during("string escape"),
        }
        Ok(())
    }

    fn peek(&self) -> Option<char> {
        self.get_at(self.read_index)
    }
    
    fn next_is<F: FnOnce(char) -> bool>(&self, f: F) -> Option<char> {
        match self.peek() {
            Some(c) if f(c) => Some(c),
            _ => None,
        }
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

    fn error_eof_during<T>(&self, while_reading: &str) -> Result<T> {
        self.error(&format!("end of file while reading {}", while_reading))
    }
}

#[cfg(test)]
mod tests {
    use super::{Tokenizer, Token, TokenType};
    use TokenType::*;

    #[test]
    fn symbols() {
        let string = "
            ( ) ( ) + - * / % , && ||
            ? : . = == ! != < <=
            > >= else function if let null return true
            false
        ";

        let tokens = [
            LParen, RParen, LParen, RParen, Plus, Minus, Star, Slash, Percent, Comma, And, Or,
            Question, Colon, Dot, Equal, EqualEqual, Not, NotEqual, LessThan, LessEqual,
            GreaterThan, GreaterEqual, Else, Function, If, Let, Null, Return, Bool(true),
            Bool(false),
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

        let mut tokenizer = Tokenizer::new("1dentifier");
        assert_ne!(*tokenizer.next_token().unwrap().unwrap().ty(), Identifier("1dentifier".to_string()));
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
    fn string() {
        use super::{BELL, BACKSPACE, FORM_FEED, VERTICAL_TAB};

        let string = r#"
            'hello, world!'
            "''"
            '''x''x'''
            """
            hello
            """

            "\u00A7"
            '\U00020CCF'
            "ÿ"
            '\377'
            "\xFF"
            r'\u00A7\\'
            "\"\\\'\`"
            '\a\b\f\n\r\t\v'

            b'ÿ'
            b"\377"
            b'\x21'
            br"\377"
            BR"\377"
        "#;

        let tokens = [
            String("hello, world!".to_owned()),
            String("''".to_owned()),
            String("x''x".to_owned()),
            String("\n            hello\n            ".to_owned()),
            
            String("\u{00A7}".to_owned()),
            String("\u{20CCF}".to_owned()),
            String("ÿ".to_owned()),
            String("ÿ".to_owned()),
            String("ÿ".to_owned()),
            String("\\u00A7\\\\".to_owned()),
            String("\"\\'`".to_owned()),
            String(format!("{}{}{}\n\r\t{}", BELL, BACKSPACE, FORM_FEED, VERTICAL_TAB)),

            Bytes(vec![195, 191]),
            Bytes(vec![0xFF]),
            Bytes(vec![0x21]),
            Bytes(vec![b'\\', b'3', b'7', b'7']),
            Bytes(vec![b'\\', b'3', b'7', b'7']),
        ];

        assert_equal_tokens(string, &tokens);

        assert_error(r#"'""#);
        assert_error(r#""""'''"#);
        assert_error("'\n'");
        assert_error(r#"b'\u00A7'"#);
        assert_error(r#"b'\U10FFFF'"#);
    }

    #[test]
    fn token_positions() {
        let string = "myVar \nconst +\n  0.01 'test'";

        let mut tokenizer = Tokenizer::new(string);
        let mut tokens = Vec::new();
        while let Some(tok) = tokenizer.next_token() {
            tokens.push(tok.unwrap());
        }

        fn assert_pos(token: &Token, line: usize, column: usize) {
            assert!(token.line() == line);
            assert!(token.column() == column);
        }

        assert_pos(&tokens[0], 1, 1);
        assert_pos(&tokens[1], 2, 1);
        assert_pos(&tokens[2], 2, 7);
        assert_pos(&tokens[3], 3, 3);
        assert_pos(&tokens[4], 3, 8);
    }

    fn assert_equal_tokens(s: &str, tokens: &[TokenType]) {
        let mut tokenizer = Tokenizer::new(s);
        for (i, tok1) in tokens.iter().enumerate() {
            let tok2 = tokenizer.next_token().unwrap().unwrap();
            assert_eq!(*tok1, *tok2.ty(), "entry number {}", i + 1);
        }

        match tokenizer.next_token() {
            Some(t) => panic!("extra token {:?}", t),
            None => {},
        }
    }

    fn assert_error(s: &str) {
        assert!(Tokenizer::new(s).next_token().unwrap().is_err(), "src: {}", s);
    }
}
