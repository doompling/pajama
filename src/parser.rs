use std::collections::HashMap;

use crate::lexer::Token;

#[derive(Debug)]
pub struct AssignLocalVar {
    pub name: String,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct Binary {
    pub op: char,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug)]
pub struct Call {
    pub fn_name: String,
    pub args: Vec<Node>,
}

#[derive(Debug)]
pub struct Int {
    pub value: u64,
}

#[derive(Debug)]
pub struct InterpolableString {
    pub value: String,
}


#[derive(Debug)]
pub struct LocalVar {
    pub name: String,
    pub return_type: Option<BaseType>
}

#[derive(Debug)]
pub struct Module {
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub enum Node {
    AssignLocalVar(AssignLocalVar),
    Binary(Binary),
    Call(Call),
    Def(Def),
    Int(Int),
    InterpolableString(InterpolableString),
    Module(Module),
    LocalVar(LocalVar),
}

// impl Node {
//   pub(crate) fn inner_ref(&self) -> String {
//     match &self {
//         Node::(inner) => inner,
//     }
// }

#[derive(Debug)]
pub enum BaseType {
    Int,
    StringType,
    Void,
    Undef,
}

#[derive(Debug)]
pub struct Arg {
    pub name: String,
    pub return_type: BaseType,
}

#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<Arg>,
    pub return_type: Option<BaseType>,
    pub is_op: bool,
    pub prec: usize,
}

#[derive(Debug)]
pub struct Def {
    pub main_fn: bool,
    pub prototype: Prototype,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct ParserResult {
    pub ast: Node,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub pos: usize,
    pub op_precedence: &'a mut HashMap<char, i32>,
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
            if self.at_end() {
                break;
            }

            let result = match self.current()? {
                Token::Def => self.parse_def(),
                _ => Err("Expected function definition"),
            };

            body.push(result?);
        }

        Ok(ParserResult {
            ast: Node::Module(Module { body }),
        })
    }

    /// Parses a user-defined function.
    fn parse_def(&mut self) -> Result<Node, &'static str> {
        // Eat 'def' keyword
        self.pos += 1;

        // Parse signature of function
        let proto = self.parse_prototype()?;

        self.advance_optional_whitespace();

        let mut body = vec![];

        loop {
            self.advance_optional_whitespace();

            match self.current()? {
                Token::End => {
                    self.advance();
                    break;
                }
                _ => body.push(self.parse_expr()?),
            }
        }

        // Return new function
        Ok(Node::Def(Def {
            main_fn: proto.name == "main",
            prototype: proto,
            body,
        }))
    }

    /// Parses the prototype of a function, whether external or user-defined.
    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        match self.current()? {
            Token::Space(_) => {
                self.advance();
            }
            _ => return Err("Expected space after def keyword"),
        }

        let (id, is_operator, precedence) = match self.curr() {
            Token::Ident(pos, id) => {
                self.advance()?;

                (id, false, 0)
            }
            _ => return { Err("Expected identifier in prototype declaration.") },
        };

        self.advance_optional_space();

        match self.curr() {
            Token::LParen => { self.advance(); },
            Token::NewLine(_) => {
                self.advance();

                return Ok(Prototype {
                    name: id,
                    args: vec![],
                    return_type: None,
                    is_op: is_operator,
                    prec: precedence,
                });
            }
            _ => return Err("Expected '(' character in prototype declaration. 2"),
        }

        self.advance_optional_whitespace();

        if let Token::RParen = self.curr() {
            self.advance();

            let return_type = self.parse_return_type()?;

            return Ok(Prototype {
                name: id,
                args: vec![],
                return_type,
                is_op: is_operator,
                prec: precedence,
            });
        }

        let mut args = vec![];

        loop {
            self.advance_optional_whitespace();

            let arg_name = match self.curr() {
                Token::Ident(pos, name) => name,
                _ => return Err("Expected identifier in parameter declaration."),
            };

            self.advance()?;
            self.advance_optional_space();

            let type_name = match self.curr() {
                Token::Ident(pos, type_name) => type_name,
                _ => return Err("Expected type name for argument"),
            };

            let return_type = match type_name.as_str() {
                "Str" => BaseType::StringType,
                "Int" => BaseType::Int,
                _ => return Err("Expected return type Str or Int")
            };

            args.push(Arg {
                name: arg_name,
                return_type,
            });

            self.advance()?;
            self.advance_optional_whitespace();

            match self.curr() {
                Token::RParen => {
                    self.advance();
                    break;
                }
                Token::Comma => {
                    self.advance();
                }
                _ => return Err("Expected ',' or ')' character in prototype declaration. 2"),
            }
        }

        let return_type = self.parse_return_type()?;

        Ok(Prototype {
            name: id,
            args,
            return_type,
            is_op: is_operator,
            prec: precedence,
        })
    }

    fn parse_return_type(&mut self) -> Result<Option<BaseType>, &'static str> {
        match self.current()? {
            Token::NewLine(_) => {
                self.advance();
                return Ok(None)
            }
            Token::Arrow => {
                self.advance();
            }
            Token::Space(_) => {
                self.advance();

                match self.curr() {
                    Token::Arrow => {
                        self.advance();
                        self.advance_optional_space();
                    }
                    Token::NewLine(_) => {
                        self.advance();
                        return Ok(None);
                    }
                    _ => return Err("Expected an arrow to indicate a return type"),
                }
            }
            _ => { return Err("Expected an end to the function definition") }
        }

        match self.curr() {
            Token::Ident(pos, name) => match name.as_ref() {
                "String" => {
                    self.advance();
                    Ok(Some(BaseType::StringType))
                }
                _ => Err("Only String return type is currently supported"),
            },
            _ => Err("Expected a return type after an arrow"),
        }
    }

    fn parse_expr(&mut self) -> Result<Node, &'static str> {
        match self.parse_unary_expr() {
            Ok(left) => {
                self.advance_optional_whitespace();
                self.parse_binary_expr(0, left)
            }
            err => err,
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(&mut self) -> Result<Node, &'static str> {
        let op = match self.current()? {
            Token::Op(ch) => {
                self.advance()?;
                ch
            }
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
                panic!("{:#?}", self.curr());
                panic!("{:#?}", self);
                Err("Unknown expression.")
            }
        }
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self) -> Result<Node, &'static str> {
        let id = match self.curr() {
            Token::Ident(pos, id) => {
                self.advance();
                id
            }
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
                        }
                        Token::Comma => {
                            self.advance();
                        }
                        _ => return Err("Expected ',' or ')' character in function call."),
                    }
                }

                Ok(Node::Call(Call { fn_name: id, args }))
            }

            _ => {
                self.advance_optional_space();

                match self.curr() {
                    Token::Assign => {
                        self.advance()?;
                        self.advance_optional_whitespace();

                        Ok(Node::AssignLocalVar(AssignLocalVar {
                            name: id,
                            value: Box::new(self.parse_expr()?)
                        }))
                    }
                    _ => {
                        Ok(Node::LocalVar(LocalVar { name: id, return_type: Some(BaseType::Undef) }))
                    }
                }
            },
        }
    }

    /// Parses a literal number.
    fn parse_nb_expr(&mut self) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Number(pos, nb) => {
                self.advance();
                Ok(Node::Int(Int { value: nb }))
            }
            _ => Err("Expected number literal."),
        }
    }

    /// Parses a literal string.
    fn parse_string_expr(&mut self) -> Result<Node, &'static str> {
        match self.curr() {
            Token::StringLiteral(pos, string) => {
                self.advance();
                Ok(Node::InterpolableString(InterpolableString {
                    value: string,
                }))
            }
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
                return Ok(left);
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
            Ok(token) => match token {
                Token::Space(_) => {
                    self.advance();
                }
                Token::NewLine(_) => {
                    self.advance();
                }
                _ => {}
            },
            Err(_) => {}
        }
    }

    fn advance_optional_space(&mut self) {
        match self.current() {
            Ok(token) => match token {
                Token::Space(_) => {
                    self.advance();
                }
                _ => {}
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
