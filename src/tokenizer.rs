use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Plus, Minus, Star, Slash, Bang, Equal, LeftParen, RightParen, LeftBracket, RightBracket,

    EqualEqual, BangEqual, Semicolon, Comma, Colon, Dot, Ellipsis, Less, LessEqual, Greater, GreaterEqual,
    Identifier, Number, String, Fn, If, Else, While, For, LeftCurly, RightCurly, Var, Return, Include,
    StrType, IntType,
}

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    pos: (u32, u32),

    value: Option<String>
}

impl Token {
    pub fn new(token_type: TokenType, pos: (u32, u32), value: Option<String>) -> Token {
        Token {
            token_type: token_type,
            pos: pos,
            value: value
        }
    }
    pub fn token_type(&self) -> TokenType {
        self.token_type
    }
    pub fn value(&self) -> Option<&str> {
        self.value.as_deref()
    }
}


fn consume_comment(chars: &mut Peekable<Chars<'_>>, pos: &mut (u32, u32)) {
    // Consume the second '/'
    chars.next();
    *pos = (pos.0 + 1, pos.1);

    // Consume until newline (do NOT consume the '\n')
    while let Some(&ch) = chars.peek() {
        if ch == '\n' {
            break;
        }
        chars.next();
        *pos = (pos.0 + 1, pos.1);
    }
}

pub fn scan(input: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    let mut chars = input.chars().peekable();


    let mut current_pos : (u32, u32) = (1, 1);

    while let Some(c) = chars.next() {
        match c {
            // Single char operators
            '+' => tokens.push(Token::new(TokenType::Plus, current_pos , None)),
            '-' => tokens.push(Token::new(TokenType::Minus, current_pos, None)),
            '*' => tokens.push(Token::new(TokenType::Star, current_pos, None)),
            '/' => {
                if let Some('/') = chars.peek() {
                    consume_comment(&mut chars, &mut current_pos);
                } else {
                    tokens.push(Token::new(TokenType::Slash, current_pos, None));
                }
            }

            // String literals
            '"' => tokens.push(scan_string('"', &mut chars, &mut current_pos)),
            '\'' => tokens.push(scan_string('\'', &mut chars,  &mut current_pos)),

            // Grouping and punctuation
            '(' => tokens.push(Token::new(TokenType::LeftParen, current_pos, None)),
            ')' => tokens.push(Token::new(TokenType::RightParen, current_pos, None)),
            '{' => tokens.push(Token::new(TokenType::LeftCurly, current_pos, None)),
            '}' => tokens.push(Token::new(TokenType::RightCurly, current_pos, None)),
            '[' => tokens.push(Token::new(TokenType::LeftBracket, current_pos, None)),
            ']' => tokens.push(Token::new(TokenType::RightBracket, current_pos, None)),
            ';' => tokens.push(Token::new(TokenType::Semicolon, current_pos, None)),
            ',' => tokens.push(Token::new(TokenType::Comma, current_pos, None)),
            '.' => {
                // Ellipsis '...'
                let mut it = chars.clone();
                let is_ellipsis = if let Some('.') = it.peek() {
                    it.next();
                    if let Some('.') = it.peek() { true } else { false }
                } else { false };
                if is_ellipsis {
                    // consume the two additional '.'
                    chars.next();
                    chars.next();
                    tokens.push(Token::new(TokenType::Ellipsis, current_pos, None));
                } else {
                    tokens.push(Token::new(TokenType::Dot, current_pos, None));
                }
            },
            ':' => tokens.push(Token::new(TokenType::Colon, current_pos, None)),

            // Two-char operators and their single-char variants
            '=' => {
                if let Some('=') = chars.peek() {
                    chars.next(); // consume the second '='
                    tokens.push(Token::new(TokenType::EqualEqual, current_pos, None));
                } else {
                    tokens.push(Token::new(TokenType::Equal, current_pos, None));
                }
            }

            '<' => {
                if let Some('=') = chars.peek() {
                    chars.next(); // consume the second '='
                    tokens.push(Token::new(TokenType::LessEqual, current_pos, None));
                } else {
                    tokens.push(Token::new(TokenType::Less, current_pos, None));
                }
            }

            '>' => {
                if let Some('=') = chars.peek() {
                    chars.next(); // consume the second '='
                    tokens.push(Token::new(TokenType::GreaterEqual, current_pos, None));
                } else {
                    tokens.push(Token::new(TokenType::Greater, current_pos, None));
                }
            }

            '!' => {
                if let Some('=') = chars.peek() {
                    chars.next(); // consume the second '='
                    tokens.push(Token::new(TokenType::BangEqual, current_pos, None));
                } else {
                    tokens.push(Token::new(TokenType::Bang, current_pos, None));
                }
            }

            '\n' => {
                current_pos = (1, current_pos.1+1)
            },

            // Whitespace: ignore
            ' ' | '\t' | '\r' => {}

            // Identifiers (start with letter or underscore) and numbers
            _ => {
                if c.is_ascii_digit() {
                    tokens.push(scan_number(c, &mut chars, &mut current_pos));
                } else if c.is_alphabetic() || c == '_' {
                    tokens.push(scan_identifier(c, &mut chars, &mut current_pos));
                }
            }
        }
        current_pos = (current_pos.0+1, current_pos.1)

    }
    tokens
}

fn scan_number(first: char, chars: &mut Peekable<Chars<'_>>, pos: &mut (u32, u32)) -> Token {
    // Number literal: integer or decimal with a single dot
    let mut number = String::from(first);
    // consume digits
    while let Some(&c2) = chars.peek() {
        if c2.is_ascii_digit() {
            number.push(c2);
            chars.next();
            *pos = (pos.0+1, pos.1);
        } else {
            break;
        }
    }
    // optional fractional part: '.' followed by one or more digits
    if let Some('.') = chars.peek() {
        // lookahead to ensure it's a real decimal (digit after dot)
        let mut it = chars.clone();
        it.next(); // skip '.'
        *pos = (pos.0+1, pos.1);
        if let Some(next_after_dot) = it.peek() {
            if next_after_dot.is_ascii_digit() {
                // consume '.'
                number.push('.');
                chars.next(); // consume following digits
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        number.push(d);
                        chars.next();
                        *pos = (pos.0+1, pos.1);

                    } else {
                        break;
                    }
                }
            }
        }
    }
    Token::new(TokenType::Number, *pos, Some(number))
}

fn scan_identifier(first: char, chars: &mut Peekable<Chars<'_>>, pos: &mut (u32, u32)) -> Token {
    let mut ident = String::from(first);
    while let Some(&c2) = chars.peek() {
        if c2.is_alphanumeric() || c2 == '_' {
            ident.push(c2);
            chars.next(); // consume it
            *pos = (pos.0+1, pos.1);

        } else {
            break;
        }
    }
    if let Some(s) = check_reserved_words(ident.as_str()) {
        return Token::new(s, *pos, Some(ident));
    }

    Token::new(TokenType::Identifier, *pos, Some(ident))
}

fn scan_string(delim: char, chars: &mut Peekable<Chars<'_>>, pos: &mut (u32, u32)) -> Token {
    let mut s = String::new();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(esc) = chars.next() {
                let mapped = match esc {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    '\'' => '\'',
                    other => other,
                };
                s.push(mapped);
            } else {
                break;
            }
        } else if ch == delim {
            return Token::new(TokenType::String, *pos, Some(s));
        } else {
            s.push(ch);
        }
        *pos = (pos.0+1, pos.1);
    }
    Token::new(TokenType::String, *pos, Some(s))
}


fn check_reserved_words(word: &str) -> Option<TokenType> {
    match word {
        "fn" => Some(TokenType::Fn),
        "if" => Some(TokenType::If),
        "else" => Some(TokenType::Else),
        "while" => Some(TokenType::While),
        "for" => Some(TokenType::For),
        "var" => Some(TokenType::Var),
        "return" => Some(TokenType::Return),
        "include" => Some(TokenType::Include),
       // "str" => Some(TokenType::StrType),
        //"int" => Some(TokenType::IntType),
        _ => None,
    }
}
