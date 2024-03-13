use std::{collections::HashMap, ops::Deref};

use melior::ir::attribute;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Access {
    pub receiver: Box<Node>,
    pub message: Box<Node>,
    pub index: i32,
    pub return_type: Option<BaseType>,
}

#[derive(Debug)]
pub struct Attribute {
    pub name: String,
    pub index: i32,
    pub return_type: BaseType,
}

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
    pub return_type: Option<BaseType>,
}

#[derive(Debug)]
pub struct Send {
    pub receiver: Box<Node>,
    pub message: Box<Node>,
    pub return_type: Option<BaseType>,
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
    pub return_type: Option<BaseType>,
}

impl LocalVar {
    pub fn nilla_class_name(&self) -> &str {
        match &self.return_type {
            Some(rt) => match rt {
                BaseType::Int => "Int",
                BaseType::StringType => "Str",
                BaseType::Void => "",
                BaseType::Undef(class_name) => class_name.as_str(),
                BaseType::BytePtr => "BytePtr",
            },
            None => "",
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub attributes: Vec<Node>,
}

#[derive(Debug)]
pub struct Trait {
    pub name: String,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct Impl {
    pub name: String,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct SelfRef {
    pub return_type: BaseType,
}

#[derive(Debug)]
pub struct Ret {
    pub value: Box<Node>,
}

#[derive(Debug)]
pub enum Node {
    Access(Access),
    AssignLocalVar(AssignLocalVar),
    Attribute(Attribute),
    Binary(Binary),
    Call(Call),
    Class(Class),
    Def(Def),
    DefE(DefE),
    Impl(Impl),
    Int(Int),
    InterpolableString(InterpolableString),
    LocalVar(LocalVar),
    Module(Module),
    Ret(Ret),
    SelfRef(SelfRef),
    Send(Send),
    Trait(Trait),
}

// impl Node {
//   pub(crate) fn inner_ref(&self) -> String {
//     match &self {
//         Node::(inner) => inner,
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub enum BaseType {
    BytePtr,
    Int,
    StringType,
    Void,
    Undef(String),
}

#[derive(Debug)]
pub struct Arg {
    pub name: String,
    pub return_type: BaseType,
}

impl Arg {
    pub fn nilla_class_name(&self) -> &str {
        match &self.return_type {
            BaseType::BytePtr => "BytePtr",
            BaseType::Int => "Int",
            BaseType::StringType => "Str",
            BaseType::Void => "",
            BaseType::Undef(class_name) => class_name.as_str(),
        }
    }
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
    pub class_name: String,
    pub impl_name: String,
    pub trait_name: String,
}

#[derive(Debug)]
pub struct DefE {
    pub prototype: Prototype,
}

#[derive(Debug)]
pub struct ParserResult {
    pub ast: Node,
    pub index: ParserResultIndex,
}

#[derive(Debug)]
pub struct ParserResultIndex {
    pub trait_index: HashMap<String, Vec<Class>>,
    pub fn_index: HashMap<String, Vec<BaseType>>,
}

#[derive(Debug)]
pub struct ParserContext {
    pub class_name: String,
    pub body: Vec<Node>,
    pub prototype: Prototype,
    pub parsing_dot: bool,
    pub parsing_returnable_loc: bool,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: Vec<Token>,
    pub pos: usize,
    pub op_precedence: &'a mut HashMap<char, i32>,
    pub index: ParserResultIndex,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, op_precedence: &mut HashMap<char, i32>) -> Parser {
        Parser {
            tokens,
            op_precedence,
            pos: 0,
            index: ParserResultIndex {
                trait_index: HashMap::new(),
                fn_index: HashMap::new(),
            },
        }
    }
    pub fn parse(&mut self) -> Result<ParserResult, &'static str> {
        let mut body = vec![];

        loop {
            self.advance_optional_whitespace();
            if self.at_end() {
                break;
            }

            let results = match self.current()? {
                Token::Class => self.parse_class(),
                Token::Trait => self.parse_trait(),
                Token::Def => self.parse_def("".to_string(), "".to_string(), "".to_string()),
                Token::DefE => self.parse_def_e(),
                _ => {
                    println!("{:#?}", self.curr());
                    Err("Expected class, def, or trait")
                }
            };

            for result in results? {
                body.push(result);
            }
        }

        Ok(ParserResult {
            ast: Node::Module(Module { body }),
            index: ParserResultIndex {
                // trait_index: self.index.trait_index
                trait_index: HashMap::new(),
                fn_index: HashMap::new(),
            },
        })
    }

    fn parse_class(&mut self) -> Result<Vec<Node>, &'static str> {
        // Advance past the keyword
        self.pos += 1;

        self.advance_optional_space();

        let (pos, class_name) = match self.current()? {
            Token::Const(pos, name) => {
                self.advance()?;
                (pos, name)
            }
            _ => return Err("Expected identifier in prototype declaration."),
        };

        self.advance_optional_space();

        match self.curr() {
            Token::NewLine(_) => self.advance(),
            _ => return Err("Expected a new line after class name"),
        };

        // parse attributes
        let mut attributes = vec![];
        let mut index = 0;
        loop {
            self.advance_optional_whitespace();

            match self.current()? {
                Token::Ident(_attr_pos, attr_name) => {
                    self.advance();
                    self.advance_optional_whitespace();

                    match self.current()? {
                        Token::Const(_type_pos, type_name) => {
                            self.advance();

                            let return_type = match type_name.as_str() {
                                "Str" => BaseType::StringType,
                                "Int" => BaseType::Int,
                                "BytePtr" => BaseType::BytePtr,
                                _ => BaseType::Undef(type_name),
                            };

                            attributes.push(Node::Attribute(Attribute {
                                name: attr_name,
                                index,
                                return_type,
                            }));
                            index += 1;
                        }
                        _ => return Err("Expected a type after the attribute name"),
                    }
                }
                _ => break,
            };
        }

        let mut functions = vec![];

        functions.push(Node::Class(Class {
            name: class_name.clone(),
            attributes,
        }));

        loop {
            self.advance_optional_whitespace();

            let results = match self.current()? {
                Token::Def => self.parse_def(class_name.clone(), "".to_string(), "".to_string()),
                Token::Impl => self.parse_impl(class_name.clone()),
                Token::End => {
                    self.advance();
                    break;
                }
                _ => return Err("Expected def, impl, or end to to the class."),
            };

            for result in results? {
                functions.push(result)
            }
        }

        Ok(functions)
    }

    fn parse_trait(&mut self) -> Result<Vec<Node>, &'static str> {
        let mut functions = vec![];

        // Advance past the keyword
        self.pos += 1;

        self.advance_optional_space();

        let name = match self.current()? {
            Token::Const(pos, name) => {
                self.advance()?;
                name
            }
            _ => return Err("Expected identifier in prototype declaration."),
        };

        self.advance_optional_space();

        match self.curr() {
            Token::NewLine(_) => self.advance(),
            _ => return Err("Expected a new line after class name"),
        };

        loop {
            self.advance_optional_whitespace();

            let results = match self.current()? {
                Token::Def => self.parse_def("".to_string(), "".to_string(), name.clone()),
                Token::End => {
                    self.advance();
                    break;
                }
                _ => {
                    println!("{:#?}", self.curr());
                    return Err("Expected only def within a trait");
                }
            };

            for result in results? {
                functions.push(result)
            }
        }

        Ok(functions)
    }

    fn parse_impl(&mut self, class_name: String) -> Result<Vec<Node>, &'static str> {
        // Advance past the keyword
        self.pos += 1;

        self.advance_optional_space();

        let impl_name = match self.current()? {
            Token::Const(pos, name) => {
                self.advance()?;
                name
            }
            _ => return Err("Expected identifier in impl declaration."),
        };

        self.advance_optional_space();

        match self.curr() {
            Token::NewLine(_) => self.advance(),
            _ => return Err("Expected a new line after impl name"),
        };

        println!("{:#?}", 1);

        if let Some(nodes) = self.index.trait_index.get_mut(&impl_name) {
            println!("{:#?}", 2);
            nodes.push(Class {
                name: class_name.clone(),
                attributes: vec![],
            });
        } else {
            println!("{:#?}", 3);

            self.index.trait_index.insert(
                impl_name.clone(),
                vec![
                    (Class {
                        name: class_name.clone(),
                        attributes: vec![],
                    }),
                ],
            );

            println!("{:#?}", self);
        };

        let mut functions = vec![];

        loop {
            self.advance_optional_whitespace();

            let results = match self.current()? {
                Token::Def => self.parse_def(class_name.clone(), impl_name.clone(), "".to_string()),
                Token::End => {
                    self.advance();
                    break;
                }
                _ => {
                    return Err("Expected only def within an impl block");
                }
            };

            for result in results? {
                functions.push(result)
            }
        }

        Ok(functions)
    }

    fn parse_def(
        &mut self,
        class_name: String,
        impl_name: String,
        trait_name: String,
    ) -> Result<Vec<Node>, &'static str> {
        // Advance past 'def' keyword
        self.pos += 1;

        let prototype = self.parse_prototype()?;

        self.advance_optional_whitespace();

        let mut ctx = ParserContext {
            class_name,
            body: vec![],
            prototype,
            parsing_dot: false,
            parsing_returnable_loc: true,
        };

        loop {
            self.advance_optional_whitespace();

            match self.current()? {
                Token::End => {
                    ctx.parsing_returnable_loc = false;

                    if !trait_name.is_empty() && ctx.body.len() == 0 {
                        break;
                    }

                    self.advance();
                    break;
                }
                _ => {
                    let expr = self.parse_expr(&ctx)?;
                    ctx.body.push(expr);
                    ctx.parsing_returnable_loc = true
                }
            }
        }

        let def_node = Def {
            main_fn: ctx.prototype.name == "main",
            prototype: ctx.prototype,
            body: ctx.body,
            class_name: ctx.class_name,
            impl_name,
            trait_name,
        };

        let mut arg_return_types = vec![];

        let fn_name = if !def_node.class_name.is_empty() {
            if !def_node.impl_name.is_empty() {
                arg_return_types.push(BaseType::Undef(def_node.impl_name.clone()));
                format!(
                    "{}:{}:{}",
                    &def_node.class_name, &def_node.impl_name, &def_node.prototype.name
                )
            } else {
                if def_node.class_name == "Str" {
                    arg_return_types.push(BaseType::StringType)
                } else {
                    arg_return_types.push(BaseType::Undef(def_node.impl_name.clone()));
                }
                format!("{}:{}", &def_node.class_name, &def_node.prototype.name)
            }
        } else if !def_node.trait_name.is_empty() {
            arg_return_types.push(BaseType::Undef(def_node.trait_name.clone()));
            format!("{}:{}", &def_node.trait_name, &def_node.prototype.name)
        } else {
            def_node.prototype.name.clone()
        };

        for arg in &def_node.prototype.args {
            arg_return_types.push(arg.return_type.clone());
        }
        self.index.fn_index.insert(fn_name, arg_return_types);

        Ok(vec![Node::Def(def_node)])
    }

    fn parse_def_e(&mut self) -> Result<Vec<Node>, &'static str> {
        // Advance past 'def' keyword
        self.pos += 1;

        let prototype = self.parse_prototype()?;

        self.advance_optional_whitespace();

        let def_e_node = DefE { prototype };

        let mut arg_return_types = vec![];

        let fn_name = def_e_node.prototype.name.clone();

        for arg in &def_e_node.prototype.args {
            arg_return_types.push(arg.return_type.clone());
        }
        self.index.fn_index.insert(fn_name, arg_return_types);

        Ok(vec![Node::DefE(def_e_node)])
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
            Token::LParen => {
                self.advance();
            }
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
            _ => {
                println!("{:#?}", self.curr());
                return Err("Expected '(' character in prototype declaration. 2");
            }
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
                Token::Const(pos, type_name) => type_name,
                _ => return Err("Expected type name for argument"),
            };

            let return_type = match type_name.as_str() {
                "Str" => BaseType::StringType,
                "Int" => BaseType::Int,
                "BytePtr" => BaseType::BytePtr,
                _ => BaseType::Undef(type_name),
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
                return Ok(None);
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
            _ => return Err("Expected an end to the function definition"),
        }

        match self.curr() {
            Token::Const(pos, name) => match name.as_ref() {
                "Str" => {
                    self.advance();
                    Ok(Some(BaseType::StringType))
                }
                "Int" => {
                    self.advance();
                    Ok(Some(BaseType::Int))
                }
                "BytePtr" => {
                    self.advance();
                    Ok(Some(BaseType::BytePtr))
                }
                _ => {
                    self.advance();
                    Ok(Some(BaseType::Undef(name)))
                }
            },
            _ => Err("Expected a return type after an arrow"),
        }
    }

    fn parse_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        match self.parse_unary_expr(ctx) {
            Ok(left) => {
                self.advance_optional_whitespace();
                self.parse_binary_expr(ctx, 0, left)
            }
            err => err,
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        let op = match self.current()? {
            Token::Op(ch) => {
                self.advance()?;
                ch
            }
            _ => return self.parse_primary(ctx),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Node::Call(Call {
            fn_name: name,
            args: vec![self.parse_unary_expr(ctx)?],
            return_type: None,
        }))
    }

    fn parse_primary(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        let node = match self.curr() {
            Token::Ident(_, _) => self.parse_ident_expr(ctx),
            Token::LParen => self.parse_paren_expr(ctx),
            Token::Number(_, _) => self.parse_nb_expr(),
            Token::Ret => self.parse_ret_expr(ctx),
            Token::SelfRef => self.parse_self_ref_expr(ctx),
            Token::StringLiteral(_, _) => self.parse_string_expr(),
            _ => {
                panic!("{:#?}", self.curr());
                panic!("{:#?}", self);
                Err("Unknown expression.")
            }
        };

        self.advance_optional_whitespace();

        match self.curr() {
            Token::Dot => self.parse_dot_expr(ctx, node),
            _ => node,
        }
    }

    fn parse_ret_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Ret => {
                if !ctx.parsing_returnable_loc {
                    return Err("Return can only be used at the root of a function.");
                }
                self.advance();

                Ok(Node::Ret(Ret {
                    value: Box::new(self.parse_expr(ctx)?),
                }))
            }
            _ => Err("Expected Ret"),
        }
    }

    fn parse_self_ref_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        match self.curr() {
            Token::SelfRef => {
                self.advance();

                Ok(Node::SelfRef(SelfRef {
                    return_type: BaseType::Undef(ctx.class_name.clone()),
                }))
            }
            _ => Err("Expected SelfRef"),
        }
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        let ident_name = match self.curr() {
            Token::Ident(pos, id) => {
                self.advance();
                id
            }
            _ => return Err("Expected identifier."),
        };

        self.advance_optional_whitespace();

        match self.curr() {
            Token::LParen => {
                self.advance()?;
                self.advance_optional_whitespace();

                if let Token::RParen = self.curr() {
                    self.advance();

                    return Ok(Node::Call(Call {
                        fn_name: ident_name,
                        args: vec![],
                        return_type: None,
                    }));
                }

                let mut args = vec![];

                loop {
                    self.advance_optional_whitespace();

                    args.push(self.parse_expr(ctx)?);

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

                Ok(Node::Call(Call {
                    fn_name: ident_name,
                    args,
                    return_type: None,
                }))
            }

            _ => {
                self.advance_optional_space();

                match self.curr() {
                    Token::Assign => {
                        self.advance()?;
                        self.advance_optional_whitespace();

                        Ok(Node::AssignLocalVar(AssignLocalVar {
                            name: ident_name,
                            value: Box::new(self.parse_expr(ctx)?),
                        }))
                    }
                    _ => {
                        // After all that, it's just a lvar. Fetch the type from the nearest assignment.
                        let closest_assignment = ctx.body.iter().rev().find(|node| match node {
                            Node::AssignLocalVar(asgnLvar) => asgnLvar.name == ident_name,
                            _ => false,
                        });

                        match closest_assignment {
                            Some(asgnLvar) => match asgnLvar {
                                Node::AssignLocalVar(asgnLvar) => {
                                    let return_type_name = match asgnLvar.value.as_ref() {
                                            Node::Int(_) => "Int",
                                            Node::InterpolableString(_) => "Str",
                                            Node::LocalVar(val) => val.nilla_class_name(),
                                            _ => return Err("Local variable assignment was given an unsupprted node")
                                        };

                                    Ok(Node::LocalVar(LocalVar {
                                        name: ident_name,
                                        return_type: Some(BaseType::Undef(
                                            return_type_name.to_string(),
                                        )),
                                    }))
                                }
                                _ => Err("Node other than AssignLocalVar in closest_assignment"),
                            },
                            None => {
                                let arg_assignment = ctx
                                    .prototype
                                    .args
                                    .iter()
                                    .find(|node| node.name == ident_name);

                                match arg_assignment {
                                    Some(arg) => Ok(Node::LocalVar(LocalVar {
                                        name: ident_name,
                                        return_type: Some(BaseType::Undef(
                                            arg.nilla_class_name().to_string(),
                                        )),
                                    })),
                                    None => Err("Local variable isn't assigned anywhere"),
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn parse_dot_expr(
        &mut self,
        ctx: &ParserContext,
        receiver: Result<Node, &'static str>,
    ) -> Result<Node, &'static str> {
        let receiver = match receiver {
            Ok(node) => node,
            Err(err) => return Err(err),
        };

        self.advance();

        let node = match self.peek()? {
            Token::LParen => match self.parse_dot_send_expr(ctx) {
                Ok(node) => Ok(Node::Send(Send {
                    receiver: Box::new(receiver),
                    message: Box::new(node),
                    return_type: Some(BaseType::Undef("".to_string())),
                })),
                Err(err) => return Err(err),
            },
            _ => match self.parse_dot_attribute_expr(ctx) {
                Ok(node) => Ok(Node::Access(Access {
                    receiver: Box::new(receiver),
                    message: Box::new(node),
                    index: 0,
                    return_type: Some(BaseType::Undef("".to_string())),
                })),
                Err(err) => return Err(err),
            },
            _ => return Err("Expected attribute or method call"),
        };

        self.advance_optional_whitespace();

        match self.curr() {
            Token::Dot => self.parse_dot_expr(ctx, node),
            _ => node,
        }
    }

    fn parse_dot_send_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        self.parse_ident_expr(ctx)
    }

    fn parse_dot_attribute_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        match self.current()? {
            Token::Ident(_pos, ident_name) => {
                self.advance()?;
                Ok(Node::Attribute(Attribute {
                    name: ident_name,
                    index: 0,
                    return_type: BaseType::Undef("".to_string()),
                }))
            }
            _ => Err("Expected Identifier for attribute access"),
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
    fn parse_paren_expr(&mut self, ctx: &ParserContext) -> Result<Node, &'static str> {
        match self.current()? {
            Token::LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        self.advance_optional_whitespace();
        self.advance()?;

        let expr = self.parse_expr(ctx)?;

        self.advance_optional_whitespace();

        match self.current()? {
            Token::RParen => self.advance()?,
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        };

        Ok(expr)
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_binary_expr(
        &mut self,
        ctx: &ParserContext,
        prec: i32,
        mut left: Node,
    ) -> Result<Node, &'static str> {
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

            let mut right = self.parse_unary_expr(ctx)?;
            let next_prec = self.get_tok_precedence();

            self.advance_optional_whitespace();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(ctx, curr_prec + 1, right)?;
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
        while let Ok(token) = self.current() {
            match token {
                Token::Space(_) => {
                    self.advance();
                }
                Token::NewLine(_) => {
                    self.advance();
                }
                _ => break,
            }
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
