use std::borrow::Borrow;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue, BasicValue};
use inkwell::AddressSpace;

use Token::*;
use safer_ffi::string;

const ANONYMOUS_FUNCTION_NAME: &str = "main";

// ======================================================================================
// LEXER ================================================================================
// ======================================================================================

/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    Arrow,
    Binary,
    Comma,
    Comment,
    Def,
    Else,
    End,
    EOF,
    Extern,
    For,
    Ident(String),
    If,
    In,
    LParen,
    NewLine,
    Number(f64),
    Op(char),
    RParen,
    Semicolon,
    StringLiteral(String),
    Then,
    Unary,
    Var,
    Whitespace,
}

/// Defines an error encountered by the `Lexer`.
pub struct LexError {
    pub error: &'static str,
    pub index: usize,
}

impl LexError {
    #[allow(unused)]
    pub fn new(msg: &'static str) -> LexError {
        LexError { error: msg, index: 0 }
    }

    #[allow(unused)]
    pub fn with_index(msg: &'static str, index: usize) -> LexError {
        LexError { error: msg, index }
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexResult = Result<Token, LexError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            chars: Box::new(input.chars().peekable()),
            pos: 0,
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lex(&mut self) -> LexResult {
        let mut pos = self.pos;
        let src = self.input;
        let start = pos;

        let ch = match self.chars.next() {
            Some(ch) => ch,
            None => return Ok(Token::EOF)
        };

        pos += 1;

        // Actually get the next token.
        let result = match ch {
            ' ' => Token::Whitespace,
            '\n' => Token::NewLine,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,

            // '#' => {
            //     // Comment
            //     loop {
            //         let ch = self.chars.next();
            //         pos += 1;

            //         if ch == Some('\n') {
            //             break;
            //         }
            //     }

            //     Token::Comment
            // },

            '"' => {
                loop {
                    let ch = self.chars.next();
                    pos += 1;

                    let ch = match ch {
                        Some(ch) => ch,
                        None => break,
                    };

                    if let '"' = ch {
                        break;
                    }
                }

                Token::StringLiteral(src[start + 1..pos - 1].to_string())
            },


            '1'..='9' => {
                // Parse number literal
                loop {
                    let next_ch = match self.chars.peek() {
                        Some(ch) => ch,
                        None => break,
                    };

                    match next_ch {
                        '0'..='9' => {
                            self.chars.next();
                            pos += 1;
                        },
                        '.' => {
                            self.chars.next();
                            pos += 1;
                        },
                        _ => { break }
                    }
                }

                Token::Number(src[start..pos].parse().unwrap())
            },

            'a'..='z' | 'A'..='Z' | '_' => {
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
                    pos += 1;
                }

                let src_ident = &src[start..pos];

                match src_ident {
                    "binary" => Token::Binary,
                    "def" => Token::Def,
                    "else" => Token::Else,
                    "end" => Token::End,
                    "extern" => Token::Extern,
                    "for" => Token::For,
                    "if" => Token::If,
                    "in" => Token::In,
                    "then" => Token::Then,
                    "unary" => Token::Unary,
                    "var" => Token::Var,
                    ident => { Token::Ident(ident.to_string())},
                }
            },

            '-' => {
                let next_chr = match self.chars.peek() {
                    Some(ch) => *ch,
                    None => return Ok(Token::Op('-')),
                };

                if next_chr != '>' {
                    self.pos = pos;
                    return Ok(Token::Op('-'));
                }

                self.chars.next();
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
        self.pos = pos;

        Ok(result)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(EOF) | Err(_) => None,
            Ok(token) => Some(token),
        }
    }
}

// ======================================================================================
// PARSER ===============================================================================
// ======================================================================================

/// Defines a primitive expression.
#[derive(Debug)]
pub enum Expr {
    Binary {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Call {
        fn_name: String,
        args: Vec<Expr>,
    },

    // Conditional {
    //     cond: Box<Expr>,
    //     consequence: Box<Expr>,
    //     alternative: Box<Expr>,
    // },

    // For {
    //     var_name: String,
    //     start: Box<Expr>,
    //     end: Box<Expr>,
    //     step: Option<Box<Expr>>,
    //     body: Box<Expr>,
    // },

    Number(f64),

    InterpolableString(String),

    Variable(String),

    // VarIn {
    //     variables: Vec<(String, Option<Expr>)>,
    //     body: Box<Expr>,
    // },
}

#[derive(Debug)]
pub enum BaseType {
    StringType,
    Void,
    Undef
}

#[derive(Debug)]
pub struct Arg {
    pub name: String,
    pub return_type: BaseType
}

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<Arg>,
    pub is_op: bool,
    pub prec: usize,
}

/// Defines a user-defined or external function.
#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub return_type: Option<BaseType>,
    pub body: Option<Expr>,
    pub is_main: bool,
}

/// Represents the `Expr` parser.
pub struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    prec: &'a mut HashMap<char, i32>,
}

// I'm ignoring the 'must_use' lint in order to call 'self.advance' without checking
// the result when an EOF is acceptable.
#[allow(unused_must_use)]
impl<'a> Parser<'a> {
    /// Creates a new parser, given an input `str` and a `HashMap` binding
    /// an operator and its precedence in binary expressions.
    pub fn new(input: String, op_precedence: &'a mut HashMap<char, i32>) -> Self {
        let mut tokens = vec![];
        let mut lexer = Lexer::new(input.as_str());

        loop {
            let token = match lexer.next() {
                Some(token) => token,
                None => break
            };

            match token {
                Whitespace => {},
                _ => tokens.push(token)
            }
        }

        Parser {
            tokens,
            prec: op_precedence,
            pos: 0,
        }
    }

    /// Parses the content of the parser.
    pub fn parse(&mut self) -> Result<Function, &'static str> {
        let result = match self.current()? {
            Def => self.parse_def(),
            Extern => self.parse_extern(),
            // _ => self.parse_toplevel_expr(),
            _ => Err("Expected function definition"),
        };

        match result {
            Ok(result) => {
                while let Ok(NewLine) = self.current() {
                    self.advance()?;
                }

                if let Ok(End) = self.current() {
                    self.advance()?;
                }

                while let Ok(NewLine) = self.current() {
                    self.advance();
                }

                if !self.at_end() {
                    println!("{:#?}", self.curr());

                    Err("Unexpected token after parsed expression.")
                } else {
                    Ok(result)
                }
            },

            err => err,
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

    /// Returns a value indicating whether or not the `Parser`
    /// has reached the end of the input.
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Returns the precedence of the current `Token`, or 0 if it is not recognized as a binary operator.
    fn get_tok_precedence(&self) -> i32 {
        if let Ok(Op(op)) = self.current() {
            *self.prec.get(&op).unwrap_or(&100)
        } else {
            -1
        }
    }

    fn parse_extern_prototype(&mut self) -> Result<Prototype, &'static str> {
        let fn_name = match self.curr() {
            Ident(id) => id,
            _ => return Err("Expected identifier in extern prototype declaration."),
        };

        self.advance();


        match self.curr() {
            LParen => (),
            _ => return Err("Expected '(' character in prototype declaration. 1"),
        }

        self.advance()?;

        if let RParen = self.curr() {
            self.advance();

            return Ok(Prototype {
                name: fn_name,
                args: vec![],
                is_op: false,
                prec: 0,
            });
        }

        let mut args = vec![];

        loop {
            match self.curr() {
                Ident(name) => {
                    let return_type = match name.as_ref() {
                        "String" => {
                            BaseType::StringType
                        },
                        _ => todo!(),
                    };

                    let uniq_name = format!("{}{}", name, args.len());

                    args.push(Arg { name: uniq_name, return_type });
                },
                _ => return Err("Expected identifier in parameter declaration."),
            }

            self.advance()?;

            match self.curr() {
                RParen => {
                    self.advance();
                    break;
                },
                Comma => {
                    if let Ok(NewLine) = self.peek() {
                        self.advance()?;
                    }

                    self.advance();
                },
                _ => return Err("Expected ',' or ')' character in prototype declaration. 1"),
            }
        }

        Ok(Prototype {
            name: fn_name,
            args,
            is_op: false,
            prec: 0,
        })
    }

    /// Parses the prototype of a function, whether external or user-defined.
    fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
        let (id, is_operator, precedence) = match self.curr() {
            Ident(id) => {
                self.advance()?;

                (id, false, 0)
            },

            // Binary => {
            //     self.advance()?;

            //     let op = match self.curr() {
            //         Op(ch) => ch,
            //         _ => return Err("Expected operator in custom operator declaration."),
            //     };

            //     self.advance()?;

            //     let mut name = String::from("binary");

            //     name.push(op);

            //     let prec = if let Number(prec) = self.curr() {
            //         self.advance()?;

            //         prec as usize
            //     } else {
            //         0
            //     };

            //     self.prec.insert(op, prec as i32);

            //     (name, true, prec)
            // },

            // Unary => {
            //     self.advance()?;

            //     let op = match self.curr() {
            //         Op(ch) => ch,
            //         _ => return Err("Expected operator in custom operator declaration."),
            //     };

            //     let mut name = String::from("unary");

            //     name.push(op);

            //     self.advance()?;

            //     (name, true, 0)
            // },

            _ => return Err("Expected identifier in prototype declaration."),
        };

        match self.curr() {
            LParen => (),
            NewLine => {
                self.advance();

                return Ok(Prototype {
                    name: id,
                    args: vec![],
                    is_op: is_operator,
                    prec: precedence,
                });
            }
            _ => return Err("Expected '(' character in prototype declaration. 2"),
        }

        self.advance()?;

        if let RParen = self.curr() {
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
            match self.curr() {
                Ident(name) => {
                    args.push(Arg { name, return_type: BaseType::Undef })
                },
                _ => return Err("Expected identifier in parameter declaration."),
            }

            self.advance()?;

            match self.curr() {
                RParen => {
                    self.advance();
                    break;
                },
                Comma => {
                    self.advance();

                    if let Ok(NewLine) = self.current() {
                        self.advance();
                    }
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
        if let Err(_) = self.current() {
            return Ok(None)
        }

        match self.curr() {
            Arrow => { self.advance()?; }
            _ => return Ok(None)
        }

        match self.curr() {
            Ident(name) => {
                match name.as_ref() {
                    "String" => {
                        self.advance();
                        Ok(Some(BaseType::StringType))
                    },
                    _ => todo!(),
                }
            },
            _ => Err("")
        }
    }

    /// Parses a user-defined function.
    fn parse_def(&mut self) -> Result<Function, &'static str> {
        // Eat 'def' keyword
        self.pos += 1;

        // Parse signature of function
        let proto = self.parse_prototype()?;
        let return_type = self.parse_return_type()?;

        let mut is_main = false;

        if proto.name == "main" {
            is_main = true;
        }

        // match self.curr() {
        //     NewLine => { self.advance(); },
        //     _ => { return Err("Expected a new line after a function declaration.") }
        // }

        // Parse body of function
        let body = self.parse_expr()?;

        // Return new function
        Ok(Function {
            prototype: proto,
            body: Some(body),
            is_main,
            return_type,
        })
    }

    /// Parses an external function declaration.
    fn parse_extern(&mut self) -> Result<Function, &'static str> {
        // Eat 'extern' keyword
        self.pos += 1;

        // Parse signature of extern function
        let proto = self.parse_extern_prototype()?;
        let return_type = self.parse_return_type()?;

        Ok(Function {
            prototype: proto,
            body: None,
            is_main: false,
            return_type
        })
    }

    /// Parses any expression.
    fn parse_expr(&mut self) -> Result<Expr, &'static str> {
        match self.parse_unary_expr() {
            Ok(left) => self.parse_binary_expr(0, left),
            err => err,
        }
    }

    /// Parses a literal number.
    fn parse_nb_expr(&mut self) -> Result<Expr, &'static str> {
        // Simply convert Token::Number to Expr::Number
        match self.curr() {
            Number(nb) => {
                self.advance();
                Ok(Expr::Number(nb))
            },
            _ => Err("Expected number literal."),
        }
    }

    /// Parses a literal string.
    fn parse_string_expr(&mut self) -> Result<Expr, &'static str> {
        // Simply convert Token::StringLiteral to Expr::StringLiteral
        match self.curr() {
            StringLiteral(string) => {
                self.advance();
                Ok(Expr::InterpolableString(string))
            },
            _ => Err("Expected string literal."),
        }
    }

    /// Parses an expression enclosed in parenthesis.
    fn parse_paren_expr(&mut self) -> Result<Expr, &'static str> {
        match self.current()? {
            LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        self.advance()?;

        let expr = self.parse_expr()?;

        match self.current()? {
            RParen => (),
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        }

        self.advance();

        Ok(expr)
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self) -> Result<Expr, &'static str> {
        let id = match self.curr() {
            Ident(id) => id,
            _ => return Err("Expected identifier."),
        };

        if self.advance().is_err() {
            return Ok(Expr::Variable(id));
        }

        match self.curr() {
            LParen => {
                self.advance()?;

                if let RParen = self.curr() {
                    return Ok(Expr::Call {
                        fn_name: id,
                        args: vec![],
                    });
                }

                let mut args = vec![];

                loop {
                    args.push(self.parse_expr()?);

                    match self.current()? {
                        Comma => {
                            if let Ok(NewLine) = self.peek() {
                                self.advance()?;
                            }
                        },
                        RParen => break,
                        _ => return Err("Expected ',' character in function call."),
                    }

                    self.advance()?;
                }

                self.advance();

                Ok(Expr::Call { fn_name: id, args })
            },

            _ => Ok(Expr::Variable(id)),
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(&mut self) -> Result<Expr, &'static str> {
        let op = match self.current()? {
            Op(ch) => {
                self.advance()?;
                ch
            },
            _ => return self.parse_primary(),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Expr::Call {
            fn_name: name,
            args: vec![self.parse_unary_expr()?],
        })
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_binary_expr(&mut self, prec: i32, mut left: Expr) -> Result<Expr, &'static str> {
        loop {
            let curr_prec = self.get_tok_precedence();

            if let Ok(End) = self.current() {
                self.advance()?;
                return Ok(left)
            }

            if curr_prec < prec || self.at_end() {
                return Ok(left);
            }

            let op = match self.curr() {
                Op(op) => op,
                _ => return Err("Invalid operator."),
            };

            self.advance()?;

            let mut right = self.parse_unary_expr()?;

            let next_prec = self.get_tok_precedence();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(curr_prec + 1, right)?;
            }

            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
    }

    /// Parses a conditional if..then..else expression.
    // fn parse_conditional_expr(&mut self) -> Result<Expr, &'static str> {
    //     // eat 'if' token
    //     self.advance()?;

    //     let cond = self.parse_expr()?;

    //     // eat 'then' token
    //     match self.current() {
    //         Ok(Then) => self.advance()?,
    //         _ => return Err("Expected 'then' keyword."),
    //     }

    //     let then = self.parse_expr()?;

    //     // eat 'else' token
    //     match self.current() {
    //         Ok(Else) => self.advance()?,
    //         _ => return Err("Expected 'else' keyword."),
    //     }

    //     let otherwise = self.parse_expr()?;

    //     Ok(Expr::Conditional {
    //         cond: Box::new(cond),
    //         consequence: Box::new(then),
    //         alternative: Box::new(otherwise),
    //     })
    // }

    /// Parses a loop for..in.. expression.
    // fn parse_for_expr(&mut self) -> Result<Expr, &'static str> {
    //     // eat 'for' token
    //     self.advance()?;

    //     let name = match self.curr() {
    //         Ident(n) => n,
    //         _ => return Err("Expected identifier in for loop."),
    //     };

    //     // eat identifier
    //     self.advance()?;

    //     // eat '=' token
    //     match self.curr() {
    //         Op('=') => self.advance()?,
    //         _ => return Err("Expected '=' character in for loop."),
    //     }

    //     let start = self.parse_expr()?;

    //     // eat ',' token
    //     match self.current()? {
    //         Comma => self.advance()?,
    //         _ => return Err("Expected ',' character in for loop."),
    //     }

    //     let end = self.parse_expr()?;

    //     // parse (optional) step expression
    //     let step = match self.current()? {
    //         Comma => {
    //             self.advance()?;

    //             Some(self.parse_expr()?)
    //         },

    //         _ => None,
    //     };

    //     // eat 'in' token
    //     match self.current()? {
    //         In => self.advance()?,
    //         _ => return Err("Expected 'in' keyword in for loop."),
    //     }

    //     let body = self.parse_expr()?;

    //     Ok(Expr::For {
    //         var_name: name,
    //         start: Box::new(start),
    //         end: Box::new(end),
    //         step: step.map(Box::new),
    //         body: Box::new(body),
    //     })
    // }

    /// Parses a var..in expression.
    // fn parse_var_expr(&mut self) -> Result<Expr, &'static str> {
    //     // eat 'var' token
    //     self.advance()?;

    //     let mut variables = Vec::new();

    //     // parse variables
    //     loop {
    //         let name = match self.curr() {
    //             Ident(name) => name,
    //             _ => return Err("Expected identifier in 'var..in' declaration."),
    //         };

    //         self.advance()?;

    //         // read (optional) initializer
    //         let initializer = match self.curr() {
    //             Op('=') => Some({
    //                 self.advance()?;
    //                 self.parse_expr()?
    //             }),

    //             _ => None,
    //         };

    //         variables.push((name, initializer));

    //         match self.curr() {
    //             Comma => {
    //                 self.advance()?;
    //             },
    //             In => {
    //                 self.advance()?;
    //                 break;
    //             },
    //             _ => return Err("Expected comma or 'in' keyword in variable declaration."),
    //         }
    //     }

    //     // parse body
    //     let body = self.parse_expr()?;

    //     Ok(Expr::VarIn {
    //         variables,
    //         body: Box::new(body),
    //     })
    // }

    /// Parses a primary expression (an identifier, a number or a parenthesized expression).
    fn parse_primary(&mut self) -> Result<Expr, &'static str> {
        let curr = self.curr();

        match curr {
            Ident(_) => self.parse_ident_expr(),
            Number(_) => self.parse_nb_expr(),
            StringLiteral(_) => self.parse_string_expr(),
            LParen => self.parse_paren_expr(),
            // If => self.parse_conditional_expr(),
            // For => self.parse_for_expr(),
            // Var => self.parse_var_expr(),
            _ => {
                Err("Unknown expression.")
            },
        }
    }

    // /// Parses a top-level expression and makes an anonymous function out of it,
    // /// for easier compilation.
    // fn parse_toplevel_expr(&mut self) -> Result<Function, &'static str> {
    //     match self.parse_expr() {
    //         Ok(expr) => Ok(Function {
    //             prototype: Prototype {
    //                 name: ANONYMOUS_FUNCTION_NAME.to_string(),
    //                 args: vec![],
    //                 is_op: false,
    //                 prec: 0,
    //             },
    //             body: Some(expr),
    //             is_main: true,
    //             return_type: Some(BaseType::Void)
    //         }),

    //         Err(err) => Err(err),
    //     }
    // }
}

// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub enum ReturnValue<'a> {
    FloatValue(FloatValue<'a>),
    ArrayPtrValue(PointerValue<'a>),
    VoidValue,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let struct_type = self.context.struct_type(&[self.context.i8_type().ptr_type(AddressSpace::default()).into(), self.context.i32_type().into(), self.context.i32_type().into()], false);

        // fix: hardcoded to string type
        builder.build_alloca(struct_type, name)
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &Expr) -> Result<ReturnValue<'ctx>, &'static str> {
        match *&expr {
            Expr::InterpolableString(string) => {
                let i8_type = self.context.i8_type();
                let i8_array_type = i8_type.array_type(20);

                let hello_world_str = self.context.const_string(string.as_bytes(), false);
                let global_str = self.module.add_global(i8_array_type, None, "0");

                global_str.set_initializer(&hello_world_str);

                let malloc_string_fn = self.module.get_function("allocate_string").unwrap();
                let i32_type = self.context.i32_type();

                // Get a pointer to the first element of the array
                let zero = self.context.i32_type().const_int(0, false);
                let indices = [zero, zero];

                let element_pointer = unsafe {
                    self.builder.build_gep(global_str.as_pointer_value(), &indices, "element_ptr")
                };

                let args = &[
                    element_pointer.into(),
                    i32_type.const_int(string.len() as u64, false).as_basic_value_enum().into()
                ];

                let pajama_str_ptr = self.builder.build_call(malloc_string_fn, args, "tmp")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                // Ok(ReturnValue::ArrayPtrValue(global_str.as_pointer_value()))
                Ok(ReturnValue::ArrayPtrValue(pajama_str_ptr.as_basic_value_enum().into_pointer_value()))
            },

            Expr::Number(nb) => Ok(ReturnValue::FloatValue(self.context.f64_type().const_float(*nb))),

            Expr::Variable(ref _name) => todo!(),

            // Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
            //     Some(var) => Ok(self.builder.build_load(*var, name.as_str()).into_float_value()),
            //     None => Err("Could not find a matching variable."),
            // },

            // Expr::VarIn {
            //     ref variables,
            //     ref body,
            // } => {
            //     let mut old_bindings = Vec::new();

            //     for &(ref var_name, ref initializer) in variables {
            //         let var_name = var_name.as_str();

            //         let initial_val = match *initializer {
            //             Some(ref init) => self.compile_expr(init)?,
            //             None => self.context.f64_type().const_float(0.),
            //         };

            //         let alloca = self.create_entry_block_alloca(var_name);

            //         self.builder.build_store(alloca, initial_val);

            //         if let Some(old_binding) = self.variables.remove(var_name) {
            //             old_bindings.push(old_binding);
            //         }

            //         self.variables.insert(var_name.to_string(), alloca);
            //     }

            //     let body = self.compile_expr(body)?;

            //     for binding in old_bindings {
            //         self.variables
            //             .insert(binding.get_name().to_str().unwrap().to_string(), binding);
            //     }

            //     Ok(body)
            // },

            Expr::Binary {
                op,
                ref left,
                ref right,
            } => {
                match op {
                    '=' => {
                        // handle assignement
                        let var_name = match *left.borrow() {
                            Expr::Variable(ref var_name) => var_name,
                            _ => {
                                return Err("Expected variable as left-hand operator of assignement.");
                            },
                        };

                        let var_val = self.compile_expr(right)?;
                        let var = self.variables.get(var_name.as_str()).ok_or("Undefined variable.")?;

                        match var_val {
                            ReturnValue::FloatValue(float_value) => self.builder.build_store(*var, float_value),
                            ReturnValue::ArrayPtrValue(array_value) => self.builder.build_store(*var, array_value),
                            _ => todo!()
                        };

                        Ok(var_val)
                    },
                    _ => {
                        Err("only assign, no use!")
                    }
                }

                // if op == '=' {
                //     // handle assignement
                //     let var_name = match *left.borrow() {
                //         Expr::Variable(ref var_name) => var_name,
                //         _ => {
                //             return Err("Expected variable as left-hand operator of assignement.");
                //         },
                //     };

                //     let var_val = self.compile_expr(right)?;
                //     let var = self.variables.get(var_name.as_str()).ok_or("Undefined variable.")?;

                //     self.builder.build_store(*var, var_val);

                //     Ok(var_val)
                // } else {
                //     let lhs = self.compile_expr(left)?;
                //     let rhs = self.compile_expr(right)?;

                //     match op {
                //         '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                //         '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                //         '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                //         '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
                //         '<' => Ok({
                //             let cmp = self
                //                 .builder
                //                 .build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp");

                //             self.builder
                //                 .build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
                //         }),
                //         '>' => Ok({
                //             let cmp = self
                //                 .builder
                //                 .build_float_compare(FloatPredicate::ULT, rhs, lhs, "tmpcmp");

                //             self.builder
                //                 .build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
                //         }),

                //         custom => {
                //             let mut name = String::from("binary");

                //             name.push(custom);

                //             match self.get_function(name.as_str()) {
                //                 Some(fun) => {
                //                     match self
                //                         .builder
                //                         .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                //                         .try_as_basic_value()
                //                         .left()
                //                     {
                //                         Some(value) => Ok(value.into_float_value()),
                //                         None => Err("Invalid call produced."),
                //                     }
                //                 },

                //                 None => Err("Undefined binary operator."),
                //             }
                //         },
                //     }
                // }
            },

            Expr::Call { ref fn_name, ref args } => match self.get_function(fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());

                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicMetadataValueEnum> =
                        compiled_args.iter().map(|val| {
                            match *val {
                                ReturnValue::FloatValue(float_value) => float_value.into(),
                                ReturnValue::ArrayPtrValue(string_ptr) => { string_ptr.into() },
                                _ => todo!()
                            }
                        }).collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => {
                            match value {
                                inkwell::values::BasicValueEnum::PointerValue(value) => Ok(ReturnValue::ArrayPtrValue(value)),
                                inkwell::values::BasicValueEnum::FloatValue(value) => Ok(ReturnValue::FloatValue(value)),
                                _ => todo!()
                            }
                        },
                        None => Ok(ReturnValue::VoidValue),
                    }
                },
                None => Err("Unknown function."),
            },

            // Expr::Conditional {
            //     ref cond,
            //     ref consequence,
            //     ref alternative,
            // } => {
            //     let parent = self.fn_value();
            //     let zero_const = self.context.f64_type().const_float(0.0);

            //     // create condition by comparing without 0.0 and returning an int
            //     let cond = self.compile_expr(cond)?;
            //     let cond = self
            //         .builder
            //         .build_float_compare(FloatPredicate::ONE, cond, zero_const, "ifcond");

            //     // build branch
            //     let then_bb = self.context.append_basic_block(parent, "then");
            //     let else_bb = self.context.append_basic_block(parent, "else");
            //     let cont_bb = self.context.append_basic_block(parent, "ifcont");

            //     self.builder.build_conditional_branch(cond, then_bb, else_bb);

            //     // build then block
            //     self.builder.position_at_end(then_bb);
            //     let then_val = self.compile_expr(consequence)?;
            //     self.builder.build_unconditional_branch(cont_bb);

            //     let then_bb = self.builder.get_insert_block().unwrap();

            //     // build else block
            //     self.builder.position_at_end(else_bb);
            //     let else_val = self.compile_expr(alternative)?;
            //     self.builder.build_unconditional_branch(cont_bb);

            //     let else_bb = self.builder.get_insert_block().unwrap();

            //     // emit merge block
            //     self.builder.position_at_end(cont_bb);

            //     let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

            //     phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

            //     Ok(phi.as_basic_value().into_float_value())
            // },

            // Expr::For {
            //     ref var_name,
            //     ref start,
            //     ref end,
            //     ref step,
            //     ref body,
            // } => {
            //     let parent = self.fn_value();

            //     let start_alloca = self.create_entry_block_alloca(var_name);
            //     let start = self.compile_expr(start)?;

            //     self.builder.build_store(start_alloca, start);

            //     // go from current block to loop block
            //     let loop_bb = self.context.append_basic_block(parent, "loop");

            //     self.builder.build_unconditional_branch(loop_bb);
            //     self.builder.position_at_end(loop_bb);

            //     let old_val = self.variables.remove(var_name.as_str());

            //     self.variables.insert(var_name.to_owned(), start_alloca);

            //     // emit body
            //     self.compile_expr(body)?;

            //     // emit step
            //     let step = match *step {
            //         Some(ref step) => self.compile_expr(step)?,
            //         None => self.context.f64_type().const_float(1.0),
            //     };

            //     // compile end condition
            //     let end_cond = self.compile_expr(end)?;

            //     let curr_var = self.builder.build_load(start_alloca, var_name);
            //     let next_var = self
            //         .builder
            //         .build_float_add(curr_var.into_float_value(), step, "nextvar");

            //     self.builder.build_store(start_alloca, next_var);

            //     let end_cond = self.builder.build_float_compare(
            //         FloatPredicate::ONE,
            //         end_cond,
            //         self.context.f64_type().const_float(0.0),
            //         "loopcond",
            //     );
            //     let after_bb = self.context.append_basic_block(parent, "afterloop");

            //     self.builder.build_conditional_branch(end_cond, loop_bb, after_bb);
            //     self.builder.position_at_end(after_bb);

            //     self.variables.remove(var_name);

            //     if let Some(val) = old_val {
            //         self.variables.insert(var_name.to_owned(), val);
            //     }

            //     Ok(self.context.f64_type().const_float(0.0))
            // },
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut args_types = vec![];

        for arg in &proto.args {
            match &arg.return_type {
                BaseType::StringType => {
                    let struct_type = self.context.struct_type(&[self.context.i8_type().ptr_type(AddressSpace::default()).into(), self.context.i32_type().into(), self.context.i32_type().into()], false);
                    let ptr = struct_type.ptr_type(AddressSpace::default()).into();

                    args_types.push(ptr);
                },
                _ => todo!()
            };
        }

        let args_types = args_types.as_slice();

        let fn_type = match &self.function.return_type {
            Some(ret_type) => {
                match ret_type {
                    BaseType::StringType => {
                        let struct_type = self.context.struct_type(&[self.context.i8_type().ptr_type(AddressSpace::default()).into(), self.context.i32_type().into(), self.context.i32_type().into()], false);
                        struct_type.ptr_type(AddressSpace::default()).fn_type(args_types, false)
                    },
                    BaseType::Void => {
                        self.context.void_type().fn_type(args_types, false)
                    },
                    _ => todo!()
                }
            },
            None => self.context.void_type().fn_type(args_types, false)
        };

        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        // for (i, arg) in fn_val.get_param_iter().enumerate() {
        //     arg.into_float_value().set_name(proto.args[i].name.as_str());
        // }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].name.as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(proto.args[i].name.clone(), alloca);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        match body {
            ReturnValue::FloatValue(value) => self.builder.build_return(Some(&value)),
            ReturnValue::ArrayPtrValue(value) => self.builder.build_return(Some(&value)),
            ReturnValue::VoidValue => self.builder.build_return(None)
        };

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}
