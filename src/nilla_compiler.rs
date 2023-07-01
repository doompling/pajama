use std::{collections::HashMap, iter::Peekable, str::Chars};

pub struct NillaCompiler {

}

impl NillaCompiler {
    pub fn compile(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        println!("{:#?}", tokens);

        let mut parser = Parser {
            tokens,
            pos: 0,
            op_precedence: &mut NillaCompiler::build_op_precedence_map()
        };

        // ...
    }

    fn build_op_precedence_map() -> HashMap<char, i32> {
        let mut op_precedence_map = HashMap::with_capacity(6);

        op_precedence_map.insert('=', 2);
        op_precedence_map.insert('<', 10);
        op_precedence_map.insert('+', 20);
        op_precedence_map.insert('-', 20);
        op_precedence_map.insert('*', 40);
        op_precedence_map.insert('/', 40);

        op_precedence_map
    }
}

#[derive(Debug, Clone)]
struct TokenPosition {
    line: usize,
    start_column: usize,
    end_column: usize,
}

#[derive(Debug, Clone)]
enum Token {
    Arrow,
    Binary,
    Comma,
    Def,
    End,
    Ident(TokenPosition, String),
    Illegal(TokenPosition, String),
    LParen,
    NewLine,
    Number(TokenPosition, f64),
    Op(char),
    RParen,
    StringLiteral(TokenPosition, String),
    Unary,
    Whitespace,
}

struct Lexer<'a> {
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
            None => return None
        };

        self.column_pos += 1;

        let mut pos = self.char_pos;
        let src = self.input;
        let start = pos;

        pos += 1;

        let token = match ch {
            ' ' => Token::Whitespace,
            '\n' => {
                self.line_pos += 1;
                self.column_pos = 0;

                Token::NewLine
            },
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,

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
                        '.' => {
                            self.chars.next();

                            self.column_pos += 1;
                            pos += 1;
                        },
                        _ => { break }
                    }
                }

                token_pos.end_column = self.column_pos;

                Token::Number(
                    token_pos,
                    src[start..pos].parse().unwrap()
                )
            },

            'a'..='z' | 'A'..='Z' | '_' => {
                let mut token_pos = TokenPosition {
                    line: self.line_pos,
                    start_column: self.column_pos,
                    end_column: self.column_pos,
                };

                // Parse identifier
                loop {
                    let ch = match self.chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !(ch).is_alphanumeric() {
                        break;
                    }

                    self.chars.next();

                    self.column_pos += 1;
                    pos += 1;
                }

                let src_ident = &src[start..pos];

                match src_ident {
                    "binary" => Token::Binary,
                    "def" => Token::Def,
                    "end" => Token::End,
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

struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    op_precedence: &'a mut HashMap<char, i32>,
}
