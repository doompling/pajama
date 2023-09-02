use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub struct TokenPosition {
    line: usize,
    start_column: usize,
    end_column: usize,
}

#[derive(Debug, Clone)]
pub enum Token {
    Arrow,
    Assign,
    Binary,
    Class,
    Comma,
    Const(TokenPosition, String),
    Def,
    Dot,
    End,
    Ident(TokenPosition, String),
    Illegal(TokenPosition, String),
    Impl,
    LParen,
    NewLine(usize),
    Number(TokenPosition, u64),
    Op(char),
    RParen,
    SelfRef,
    Space(usize),
    StringLiteral(TokenPosition, String),
    Trait,
    Unary,
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    char_pos: usize,
    line_pos: usize,
    column_pos: usize,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            char_pos: 0,
            line_pos: 1,
            column_pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(token) = self.lex() {
            tokens.push(token);
        }

        tokens
    }

    pub fn lex(&mut self) -> Option<Token> {
        let ch = match self.chars.next() {
            Some(ch) => ch,
            None => return None,
        };

        self.column_pos += 1;

        let mut pos = self.char_pos;
        let src = self.input;
        let start = pos;

        pos += 1;

        let token = match ch {
            ' ' => {
                let mut whitespace_length = 1;

                loop {
                    let next_ch = match self.chars.peek() {
                        Some(ch) => ch,
                        None => break,
                    };

                    match next_ch {
                        ' ' => {
                            self.chars.next();

                            whitespace_length += 1;
                            self.column_pos += 1;
                            pos += 1;
                        },
                        _ => { break }
                    }
                }

                Token::Space(whitespace_length)
            },
            '\n' => {
                self.line_pos += 1;
                self.column_pos = 0;

                let mut newline_length = 1;

                loop {
                    let next_ch = match self.chars.peek() {
                        Some(ch) => ch,
                        None => break,
                    };

                    match next_ch {
                        '\n' => {
                            self.chars.next();
                            self.line_pos += 1;
                            newline_length += 1;
                            pos += 1;
                        },
                        _ => { break }
                    }
                }

                Token::NewLine(newline_length)
            },
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '"' => {
                let mut token_pos = TokenPosition {
                    line: self.line_pos,
                    start_column: self.column_pos,
                    end_column: self.column_pos,
                };

                loop {
                    let ch = self.chars.next();

                    self.column_pos += 1;
                    pos += 1;

                    let ch = match ch {
                        Some(ch) => ch,
                        None => break,
                    };

                    if let '"' = ch {
                        break;
                    }
                }

                token_pos.end_column = self.column_pos;

                Token::StringLiteral(
                    token_pos,
                    src[start + 1..pos - 1].to_string()
                )
            },


            '1'..='9' => {
                let mut token_pos = TokenPosition {
                    line: self.line_pos,
                    start_column: self.column_pos,
                    end_column: self.column_pos,
                };

                // Parse number literal
                loop {
                    let next_ch = match self.chars.peek() {
                        Some(ch) => ch,
                        None => break,
                    };

                    match next_ch {
                        '0'..='9' => {
                            self.chars.next();

                            self.column_pos += 1;
                            pos += 1;
                        },
                        // '.' => {
                        //     self.chars.next();

                        //     self.column_pos += 1;
                        //     pos += 1;
                        // },
                        _ => { break }
                    }
                }

                token_pos.end_column = self.column_pos;

                Token::Number(
                    token_pos,
                    src[start..pos].parse().unwrap()
                )
            },

            'A'..='Z' => {
                let mut token_pos = TokenPosition {
                    line: self.line_pos,
                    start_column: self.column_pos,
                    end_column: self.column_pos,
                };

                loop {
                    let ch = match self.chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    match ch {
                        'a'..='z' | 'A'..='Z' => {}
                        _ => break
                    }

                    self.chars.next();

                    self.column_pos += 1;
                    pos += 1;
                }

                let src_ident = &src[start..pos];

                match src_ident {
                    ident => {
                        token_pos.end_column = self.column_pos;
                        Token::Const(token_pos, ident.to_string())
                    },
                }
            }

            'a'..='z' | '_' => {
                let mut token_pos = TokenPosition {
                    line: self.line_pos,
                    start_column: self.column_pos,
                    end_column: self.column_pos,
                };

                loop {
                    let ch = match self.chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    match ch {
                        'a'..='z' | '_' => {}
                        _ => break
                    }

                    self.chars.next();

                    self.column_pos += 1;
                    pos += 1;
                }

                let src_ident = &src[start..pos];

                match src_ident {
                    "binary" => Token::Binary,
                    "class" => Token::Class,
                    "def" => Token::Def,
                    "end" => Token::End,
                    "impl" => Token::Impl,
                    "self" => Token::SelfRef,
                    "trait" => Token::Trait,
                    "unary" => Token::Unary,
                    ident => {
                        token_pos.end_column = self.column_pos;
                        Token::Ident(token_pos, ident.to_string())
                    },
                }
            },

            '-' => {
                let next_chr = match self.chars.peek() {
                    Some(ch) => *ch,
                    None => return Some(Token::Op('-')),
                };

                if next_chr != '>' {
                    self.char_pos = pos;
                    return Some(Token::Op('-'));
                }

                self.chars.next();

                self.column_pos += 1;
                pos += 1;

                Token::Arrow
            }

            '>' => {
                Token::Op('>')
            }


            '=' => {
                Token::Assign
            }

            _ => {
                println!("NOT IMPL{:#?}", ch);
                todo!()
            }

            // op => {
            //     // Parse operator
            //     Ok(Token::Op(op))
            // },
        };

        // Update stored position, and return
        self.char_pos = pos;

        Some(token)
    }
}
