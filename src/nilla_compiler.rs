use std::{collections::HashMap, iter::Peekable, str::Chars};

pub struct NillaCompiler {

}

impl NillaCompiler {
    pub fn compile(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let mut precedence_map = NillaCompiler::build_op_precedence_map();
        let results = Parser::new(tokens, &mut precedence_map).parse();

        println!("{:#?}", results);

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

#[derive(Debug)]
struct Binary {
    op: char,
    left: Box<Node>,
    right: Box<Node>,
}

#[derive(Debug)]
struct Call {
    fn_name: String,
    args: Vec<Node>,
}

#[derive(Debug)]
struct Int {
    value: f64
}

#[derive(Debug)]
struct InterpolableString {
    value: String
}

#[derive(Debug)]
struct Module {
    body: Vec<Node>
}

#[derive(Debug)]
enum Node {
    Binary(Binary),
    Call(Call),
    Def(Def),
    Int(Int),
    InterpolableString(InterpolableString),
    Module(Module)
}

#[derive(Debug)]
enum BaseType {
    StringType,
    Void,
    Undef
}

#[derive(Debug)]
struct Arg {
    name: String,
    return_type: BaseType
}

#[derive(Debug)]
struct Prototype {
    name: String,
    args: Vec<Arg>,
    is_op: bool,
    prec: usize,
}

#[derive(Debug)]
struct Def {
    main_fn: bool,
    prototype: Prototype,
    return_type: Option<BaseType>,
    body: Vec<Node>,
}

#[derive(Debug)]
struct ParserResult {
   ast: Node,
}

#[derive(Debug)]
struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    op_precedence: &'a mut HashMap<char, i32>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, op_precedence: &mut HashMap<char, i32>) -> Parser {
        Parser {
            tokens,
            op_precedence,
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> Result<ParserResult, &'static str> {
        let mut body = vec![];

        loop {
            self.advance_optional_whitespace();
            if self.at_end() { break }

            let result = match self.current()? {
                Token::Def => self.parse_def(),
                _ => Err("Expected function definition"),
            };

            body.push(result?);
        }

        Ok(ParserResult { ast: Node::Module(Module { body }) })
    }

    /// Parses a user-defined function.
    fn parse_def(&mut self) -> Result<Node, &'static str> {
        // Eat 'def' keyword
        self.pos += 1;

        // Parse signature of function
        let proto = self.parse_prototype()?;
        let return_type = self.parse_return_type()?;

        self.advance_optional_space();

        match self.curr() {
            Token::NewLine(_) => { self.advance(); }
            _ => {
                return Err("Expected newline after function return type")
            },
        }

        let mut body = vec![];

        loop {
            self.advance_optional_space();

            match self.current()? {
                Token::End => {
                    self.advance();
                    break;
                },
                _ => { body.push(self.parse_expr()?) }
            }
        }

        // Return new function
        Ok(
            Node::Def(Def {
                main_fn: proto.name == "main",
                prototype: proto,
                body,
                return_type,
            })
        )
    }

    /// Parses the prototype of a function, whether external or user-defined.
    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        match self.current()? {
            Token::Space(_) => { self.advance(); },
            _ => return Err("Expected space after def keyword")
        }

        let (id, is_operator, precedence) = match self.curr() {
            Token::Ident(pos, id) => {
                self.advance()?;

                (id, false, 0)
            },
            _ => return {
                Err("Expected identifier in prototype declaration.")
            },
        };

        self.advance_optional_space();

        match self.curr() {
            Token::LParen => (),
            Token::NewLine(_) => {
                return Ok(Prototype {
                    name: id,
                    args: vec![],
                    is_op: is_operator,
                    prec: precedence,
                });
            }
            _ => {
                return Err("Expected '(' character in prototype declaration. 2")
            },
        }

        self.advance_optional_whitespace();
        self.advance()?;

        if let Token::RParen = self.curr() {
            self.advance();

            return Ok(Prototype {
                name: id,
                args: vec![],
                is_op: is_operator,
                prec: precedence,
            });
        }

        let mut args = vec![];

        loop {
            self.advance_optional_whitespace();

            match self.curr() {
                Token::Ident(pos, name) => {
                    args.push(Arg { name, return_type: BaseType::Undef })
                },
                _ => return Err("Expected identifier in parameter declaration."),
            }

            self.advance_optional_whitespace();
            self.advance()?;

            match self.curr() {
                Token::RParen => {
                    self.advance();
                    break;
                },
                Token::Comma => {
                    self.advance();
                },
                _ => {
                    return Err("Expected ',' or ')' character in prototype declaration. 2")
                },
            }
        }

        Ok(Prototype {
            name: id,
            args,
            is_op: is_operator,
            prec: precedence,
        })
    }

    fn parse_return_type(&mut self) -> Result<Option<BaseType>, &'static str> {
        match self.current()? {
            Token::Arrow => {
                self.advance();
            },
            Token::Space(_) => {
                if let Ok(next_token) = self.peek() {
                    match next_token {
                        Token::Arrow => {
                            self.advance();
                            self.advance();
                            self.advance_optional_whitespace();

                        },
                        Token::NewLine(_) => {
                            self.advance();
                            self.advance();
                            return Ok(None)
                        }
                        _ => return Err("Expected an arrow to indicate a return type")
                    }
                }
            },
            _ => {}
        }

        match self.peek()? {
            Token::Arrow => {
                self.advance_optional_whitespace();
                self.advance();
            },
            _ => return Ok(None)
        }

        self.advance_optional_whitespace();

        match self.curr() {
            Token::Ident(pos, name) => {
                match name.as_ref() {
                    "String" => {
                        self.advance();
                        Ok(Some(BaseType::StringType))
                    },
                    _ => Err("Only String return type is currently supported"),
                }
            },
            _ => Err("Expected a return type after an arrow")
        }
    }

    /// Parses any expression.
    fn parse_expr(&mut self) -> Result<Node, &'static str> {
        match self.parse_unary_expr() {
            Ok(left) => {
                self.advance_optional_whitespace();
                self.parse_binary_expr(0, left)
            },
            err => err,
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(&mut self) -> Result<Node, &'static str> {
        let op = match self.current()? {
            Token::Op(ch) => {
                self.advance()?;
                ch
            },
            _ => return self.parse_primary(),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Node::Call(Call {
            fn_name: name,
            args: vec![self.parse_unary_expr()?],
        }))
    }

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    fn parse_primary(&mut self) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Ident(_, _) => self.parse_ident_expr(),
            Token::Number(_, _) => self.parse_nb_expr(),
            Token::StringLiteral(_, _) => self.parse_string_expr(),
            Token::LParen => self.parse_paren_expr(),
            _ => {
                panic!("{:#?}", self);
                Err("Unknown expression.")
            },
        }
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self) -> Result<Node, &'static str> {
        let id = match self.curr() {
            Token::Ident(pos, id) => {
                self.advance();
                id
            },
            _ => return Err("Expected identifier."),
        };

        match self.curr() {
            Token::LParen => {
                self.advance()?;
                self.advance_optional_whitespace();

                if let Token::RParen = self.curr() {
                    self.advance();

                    return Ok(Node::Call(Call {
                        fn_name: id,
                        args: vec![],
                    }));
                }

                let mut args = vec![];

                loop {
                    self.advance_optional_whitespace();

                    args.push(self.parse_expr()?);

                    self.advance_optional_whitespace();

                    match self.curr() {
                        Token::RParen => {
                            self.advance();
                            break;
                        },
                        Token::Comma => {
                            self.advance();
                        },
                        _ => {
                            return Err("Expected ',' or ')' character in function call.")
                        },
                    }
                }

                Ok(Node::Call(Call { fn_name: id, args }))
            },

            // _ => Ok(Node::Variable(id)),
            _ => {
                todo!("variable reference")
            },
        }
    }

    /// Parses a literal number.
    fn parse_nb_expr(&mut self) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Number(pos, nb) => {
                self.advance();
                Ok(Node::Int(Int { value: nb }))
            },
            _ => Err("Expected number literal."),
        }
    }

    /// Parses a literal string.
    fn parse_string_expr(&mut self) -> Result<Node, &'static str> {
        match self.curr() {
            Token::StringLiteral(pos, string) => {
                self.advance();
                Ok(Node::InterpolableString(InterpolableString { value: string }))
            },
            _ => Err("Expected string literal."),
        }
    }

    /// Parses an expression enclosed in parenthesis.
    fn parse_paren_expr(&mut self) -> Result<Node, &'static str> {
        match self.current()? {
            Token::LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        self.advance_optional_whitespace();
        self.advance()?;

        let expr = self.parse_expr()?;

        self.advance_optional_whitespace();

        match self.current()? {
            Token::RParen => self.advance()?,
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        };

        Ok(expr)
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_binary_expr(&mut self, prec: i32, mut left: Node) -> Result<Node, &'static str> {
        loop {
            if let Ok(Token::End) = self.current() {
                // self.advance()?;
                return Ok(left)
            }

            let curr_prec = self.get_tok_precedence();

            if curr_prec < prec || self.at_end() {
                return Ok(left);
            }

            let op = match self.curr() {
                Token::Op(op) => op,
                _ => return Err("Invalid operator."),
            };

            self.advance()?;
            self.advance_optional_whitespace();

            let mut right = self.parse_unary_expr()?;
            let next_prec = self.get_tok_precedence();

            self.advance_optional_whitespace();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(curr_prec + 1, right)?;
            }

            left = Node::Binary(Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });
        }
     }

    fn peek(&self) -> Result<Token, &'static str> {
        if self.pos + 1 >= self.tokens.len() {
            Err("Peeked at end of file")
        } else {
            Ok(self.tokens[self.pos + 1].clone())
        }
    }

    /// Returns the current `Token`, without performing safety checks beforehand.
    fn curr(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    /// Returns the current `Token`, or an error that
    /// indicates that the end of the file has been unexpectedly reached if it is the case.
    fn current(&self) -> Result<Token, &'static str> {
        if self.pos >= self.tokens.len() {
            Err("Position doesn't match the token count")
        } else {
            Ok(self.tokens[self.pos].clone())
        }
    }

    /// Advances the position, and returns an empty `Result` whose error
    /// indicates that the end of the file has been unexpectedly reached.
    /// This allows to use the `self.advance()?;` syntax.
    fn advance(&mut self) -> Result<(), &'static str> {
        let npos = self.pos + 1;

        self.pos = npos;

        if npos < self.tokens.len() {
            Ok(())
        } else {
            Err("Unexpected end of file.")
        }
    }

    fn advance_token(&mut self) -> Result<Token, &'static str> {
        let npos = self.pos + 1;

        self.pos = npos;

        if npos < self.tokens.len() {
            Ok(self.curr())
        } else {
            Err("Unexpected end of file.")
        }
    }

    fn advance_optional_whitespace(&mut self) {
        match self.current() {
            Ok(token) => {
                match token {
                    Token::Space(_) => { self.advance(); },
                    Token::NewLine(_) => { self.advance(); },
                    _ => {}
                }
            },
            Err(_) => {}
        }
    }

    fn advance_optional_space(&mut self) {
        match self.current() {
            Ok(token) => {
                match token {
                    Token::Space(_) => { self.advance(); },
                    _ => {}
                }
            },
            Err(_) => {}
        }
    }

    /// Returns a value indicating whether or not the `Parser`
    /// has reached the end of the input.
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Returns the precedence of the current `Token`, or 0 if it is not recognized as a binary operator.
    fn get_tok_precedence(&self) -> i32 {
        if let Ok(Token::Op(op)) = self.current() {
            *self.op_precedence.get(&op).unwrap_or(&100)
        } else {
            -1
        }
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
    NewLine(usize),
    Number(TokenPosition, f64),
    Op(char),
    RParen,
    StringLiteral(TokenPosition, String),
    Unary,
    Space(usize),
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
