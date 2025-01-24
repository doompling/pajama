use std::{
    collections::HashMap,
    fs::OpenOptions,
    hash::Hash,
    ops::{Deref, DerefMut},
};

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
pub struct Array {
    pub items: Vec<Node>,
    pub item_type: BaseType,
    pub length: i64,
}

#[derive(Debug)]
pub struct Attribute {
    pub name: String,
    pub index: i32,
    pub return_type: BaseType,
}

#[derive(Debug)]
pub struct AssignAttribute {
    pub name: String,
    pub index: i32,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct AssignAttributeAccess {
    pub access: Access,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct AssignLocalVar {
    pub name: String,
    pub value: Box<Node>,
}

#[derive(Debug)]
pub struct AssignConstant {
    pub name: String,
    pub value: Box<Node>,
    pub return_type: BaseType,
}

#[derive(Debug)]
pub struct Binary {
    pub op: [char; 4],
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
pub struct FnRef {
    pub fn_name: String,
}

#[derive(Debug)]
pub struct Int {
    pub value: u64,
}

#[derive(Debug)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(Debug)]
pub struct LocalVar {
    pub name: String,
    pub return_type: Option<BaseType>,
}

impl LocalVar {
    pub fn pajama_class_name(&self) -> &str {
        match &self.return_type {
            Some(rt) => match rt {
                BaseType::Array(_, _) => "Array",
                BaseType::Byte => "Byte",
                BaseType::BytePtr => "BytePtr",
                BaseType::Class(class_name) => class_name.as_str(),
                BaseType::FnRef => "FnRef",
                BaseType::Int => "Int",
                BaseType::Int16 => "Int16",
                BaseType::Int32 => "Int32",
                BaseType::Int64 => "Int64",
                BaseType::Void => "",
                BaseType::Struct(_) => "Struct",
            },
            None => "",
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub methods: Vec<Node>,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub return_type: BaseType,
}

#[derive(Debug)]
pub struct BuildStruct {
    pub name: String,
    pub args: Vec<Node>,
    pub return_type: BaseType,
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
pub struct Const {
    pub name: String,
}

#[derive(Debug)]
pub enum Node {
    Access(Access),
    Array(Array),
    AssignAttribute(AssignAttribute),
    AssignAttributeAccess(AssignAttributeAccess),
    AssignConstant(AssignConstant),
    AssignLocalVar(AssignLocalVar),
    Attribute(Attribute),
    Binary(Binary),
    BuildStruct(BuildStruct),
    Call(Call),
    Class(Class),
    Const(Const),
    Def(Def),
    DefE(DefE),
    FnRef(FnRef),
    Impl(Impl),
    Int(Int),
    LocalVar(LocalVar),
    Loop(Loop),
    Module(Module),
    Ret(Ret),
    SelfRef(SelfRef),
    Send(Send),
    StringLiteral(StringLiteral),
    Struct(Struct),
    Trait(Trait),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BaseType {
    // Integer Types
    Byte,
    Int, // Int64 by default
    Int16,
    Int32,
    Int64,
    FnRef,

    // Dynamic Types
    Array(i64, Box<BaseType>),
    Class(String),
    Struct(String),

    // Pointer Types
    BytePtr,

    // To Remove
    Void,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: String,
    pub return_type: BaseType,
}

impl Arg {
    pub fn pajama_class_name(&self) -> &str {
        match &self.return_type {
            BaseType::Array(_, _) => "Array",
            BaseType::Byte => "Byte",
            BaseType::BytePtr => "BytePtr",
            BaseType::Class(class_name) => class_name.as_str(),
            BaseType::FnRef => "FnRef",
            BaseType::Int => "Int",
            BaseType::Int16 => "Int16",
            BaseType::Int32 => "Int32",
            BaseType::Int64 => "Int64",
            BaseType::Struct(_) => "Struct",
            BaseType::Void => "",
        }
    }
}

#[derive(Debug, Clone)]
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
pub struct Loop {
    // pub args: HashMap<String, LocalVar>,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct ParserResult {
    pub module: Node,
    pub index: ParserResultIndex,
}

#[derive(Debug)]
pub struct ParserResultIndex {
    pub trait_index: HashMap<String, Vec<Class>>,
    pub class_index: HashMap<String, Class>,
    pub struct_index: HashMap<String, Struct>,
    pub constant_index: HashMap<String, BaseType>,
    pub fn_prototype_index: HashMap<String, Prototype>,
}

#[derive(Debug)]
pub struct ParserModuleCtx {
    pub class_name: String,
    pub self_node: Option<Node>,
}

#[derive(Debug)]
pub struct ParserFunctionCtx {
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
    // pub fn new(tokens: Vec<Token>, op_precedence: &mut HashMap<char, i32>) -> Parser {
    //     Parser {
    //         tokens,
    //         op_precedence,
    //         pos: 0,
    //         index: ParserResultIndex {
    //             trait_index: HashMap::new(),
    //             class_index: HashMap::new(),
    //         },
    //     }
    // }

    pub fn start_parse(tokens: Vec<Token>, op_precedence: &mut HashMap<char, i32>) -> ParserResult {
        let mut parser = Parser {
            tokens,
            op_precedence,
            pos: 0,
            index: ParserResultIndex {
                trait_index: HashMap::new(),
                class_index: HashMap::new(),
                struct_index: HashMap::new(),
                constant_index: HashMap::new(),
                fn_prototype_index: HashMap::new(),
            },
        };

        let module = parser.parse().unwrap();

        ParserResult {
            module,
            index: parser.index,
        }
    }

    // pub fn parse(&mut self) -> Result<ParserResult, &'static str> {
    pub fn parse(&mut self) -> Result<Node, &'static str> {
        let mut methods = vec![];
        let mut mctx = ParserModuleCtx {
            self_node: None,
            class_name: "".to_string(),
        };

        loop {
            self.advance_optional_whitespace();
            if self.at_end() {
                mctx.self_node = None;
                break;
            }

            let results = match self.current()? {
                Token::Const(pos, name) => self.parse_constant_assignment_expr(&mut mctx),
                Token::Class => self.parse_class(&mut mctx),
                Token::Struct => self.parse_struct(&mut mctx),
                Token::Trait => self.parse_trait(&mut mctx),
                Token::Def => self.parse_def(
                    &mut mctx,
                    "".to_string(),
                    "".to_string(),
                    "".to_string(),
                    None,
                ),
                Token::DefE => self.parse_def_e(&mut mctx),
                _ => {
                    println!("{:#?}", self.curr());
                    Err("Expected class, def, or trait")
                }
            };

            for result in results? {
                methods.push(result);
            }
        }

        Ok(Node::Module(Module { methods }))

        // Ok(ParserResult {
        //     module: Node::Module(Module { methods }),
        //     index: self.index
        // })
    }

    // fn parse_comment(&mut self, mctx: &mut ParserModuleCtx) -> Result<Vec<Node>, &'static str> {
    //     match self.curr() {
    //         Token::Comment(pos, text) => {
    //             self.advance()?;
    //             self.advance_optional_whitespace();

    //             Ok(vec![Node::Comment(Comment { text })])
    //         }
    //         _ => return Err("Expected const node")
    //     }
    // }

    fn parse_constant_assignment_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
    ) -> Result<Vec<Node>, &'static str> {
        println!("{:#?}", self.curr());

        let name = match self.current()? {
            Token::Const(pos, name) => {
                self.advance()?;
                self.advance_optional_whitespace();

                name
            }
            _ => return Err("Expected const node"),
        };

        println!("name {:#?}", name);
        println!("{:#?}", self.curr());

        let return_type = match self.current()? {
            Token::Const(_type_pos, type_name) => {
                self.advance();
                self.advance_optional_whitespace();

                self.class_base_type(type_name)
            }
            _ => return Err("Expected type for constant"),
        };

        println!("{:#?}", self.curr());

        match self.current()? {
            Token::Assign => {
                self.advance();
                self.advance_optional_whitespace();
            }
            _ => return Err("Expected constant assignment"),
        };

        let value = Box::new(self.parse_constant_value_expr(mctx).unwrap());

        self.index
            .constant_index
            .insert(name.clone(), return_type.clone());

        Ok(vec![Node::AssignConstant(AssignConstant {
            name,
            value,
            return_type,
        })])
        // Ok(vec![])
    }

    fn parse_constant_value_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
    ) -> Result<Node, &'static str> {
        self.parse_nb_expr()

        // match self.current()? {
        //     Token::Number(pos, value) => Ok(Node::Int(Int { value })),
        //     // Token::LSquareBrace => todo!(),
        //     // Token::StringLiteral(_, _) => todo!(),
        //     // Token::Const(_, _) => todo!(),
        //     _ => todo!()
        // }
    }

    fn parse_class(&mut self, mctx: &mut ParserModuleCtx) -> Result<Vec<Node>, &'static str> {
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

        let attributes = self.parse_attributes().unwrap();

        let class_node = Class {
            name: class_name.clone(),
            attributes,
        };

        mctx.class_name = class_name.clone();
        mctx.self_node = Some(Node::SelfRef(SelfRef {
            return_type: BaseType::Class(mctx.class_name.clone()),
        }));

        let mut functions = vec![];
        let mut new_fn = None;

        loop {
            self.advance_optional_whitespace();

            let results = match self.current()? {
                Token::Def => self.parse_def(
                    mctx,
                    class_name.clone(),
                    "".to_string(),
                    "".to_string(),
                    new_fn,
                ),
                Token::Impl => self.parse_impl(mctx, class_name.clone()),
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

        if let Some(function) = new_fn {
            todo!()
        } else {
            // Construct a new function
            let mut args = vec![Arg {
                name: "sret".to_string(),
                return_type: BaseType::Class(mctx.class_name.clone()),
            }];

            let mut body = vec![];

            for (index, attribute) in class_node.attributes.iter().enumerate() {
                args.push(Arg {
                    name: attribute.name.clone(),
                    return_type: attribute.return_type.clone(),
                });

                body.push(Node::AssignAttribute(AssignAttribute {
                    name: attribute.name.clone(),
                    index: index as i32,
                    value: Box::new(Node::LocalVar(LocalVar {
                        name: attribute.name.clone(),
                        return_type: Some(attribute.return_type.clone()),
                    })),
                }))
            }

            let prototype = Prototype {
                name: format!("{}.new", mctx.class_name.clone()).to_string(),
                args,
                return_type: Some(BaseType::Class(class_name.clone())),
                is_op: false,
                prec: 0,
            };

            self.index
                .fn_prototype_index
                .insert(prototype.name.clone(), prototype.clone());

            let new_fn = Node::Def(Def {
                main_fn: false,
                prototype,
                body,
                class_name: mctx.class_name.clone(),
                impl_name: "".to_string(),
                trait_name: "".to_string(),
            });

            functions.push(new_fn);

            // Construct an alloca function
            let mut args = vec![Arg {
                name: "sret".to_string(),
                return_type: BaseType::Class(mctx.class_name.clone()),
            }];

            let mut body = vec![];

            let prototype = Prototype {
                name: format!("{}.alloca", mctx.class_name.clone()).to_string(),
                args,
                return_type: Some(BaseType::Class(class_name.clone())),
                is_op: false,
                prec: 0,
            };

            self.index
                .fn_prototype_index
                .insert(prototype.name.clone(), prototype.clone());

            let new_fn = Node::Def(Def {
                main_fn: false,
                prototype,
                body,
                class_name: mctx.class_name.clone(),
                impl_name: "".to_string(),
                trait_name: "".to_string(),
            });

            functions.push(new_fn);
        }

        self.index
            .class_index
            .insert(class_name.clone(), class_node);

        mctx.class_name = "".to_string();
        mctx.self_node = None;

        Ok(functions)
    }

    fn parse_struct(&mut self, mctx: &mut ParserModuleCtx) -> Result<Vec<Node>, &'static str> {
        // Advance past the keyword
        self.pos += 1;

        self.advance_optional_space();

        let (pos, struct_name) = match self.current()? {
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

        let attributes = self.parse_attributes().unwrap();

        match self.current()? {
            Token::End => {
                self.advance();
                self.advance_optional_whitespace();
            }
            _ => return Err("Expected End to struct"),
        }

        // let attr_return_types = attributes.iter().map(|attr| attr.return_type.clone()).collect();
        let return_type = BaseType::Struct(struct_name.clone());

        let struct_struct = Struct {
            name: struct_name.clone(),
            attributes,
            return_type,
        };

        self.index
            .struct_index
            .insert(struct_name.clone(), struct_struct);

        mctx.class_name = "".to_string();
        mctx.self_node = None;

        Ok(vec![])
    }

    fn parse_attributes(&mut self) -> Result<Vec<Attribute>, &'static str> {
        let mut attributes = vec![];
        let mut index = 0;
        loop {
            self.advance_optional_whitespace();

            match self.current()? {
                Token::Attribute(_attr_pos, attr_name) => {
                    self.advance();
                    self.advance_optional_whitespace();

                    let return_type = match self.current()? {
                        Token::Const(_type_pos, type_name) => {
                            self.advance();
                            self.class_base_type(type_name)
                        }
                        Token::LSquareBrace => {
                            self.advance();

                            let length = match self.current()? {
                                Token::Number(_, n) => n,
                                _ => return Err("Expected length of array"),
                            };

                            self.advance();
                            self.advance_optional_space();

                            match self.current()? {
                                Token::Ident(_, sym) => match sym.as_str() {
                                    "x" => {
                                        self.advance();
                                        self.advance_optional_space();
                                    }
                                    _ => return Err("Expected an 'x' for such as [4 x Byte]"),
                                },
                                _ => return Err("Expected type for array 1"),
                            };

                            let array_return_type = match self.current()? {
                                Token::Const(_type_pos, type_name) => {
                                    self.advance();
                                    self.class_base_type(type_name)
                                }
                                _ => return Err("Expected type for array 2"),
                            };

                            match self.current()? {
                                Token::RSquareBrace => self.advance(),
                                _ => return Err("Expected ] to end array type"),
                            };

                            BaseType::Array(length as i64, Box::new(array_return_type))
                        }
                        _ => return Err("Expected a type after the attribute name"),
                    };

                    attributes.push(Attribute {
                        name: attr_name,
                        index,
                        return_type,
                    });
                    index += 1;
                }
                _ => break,
            };
        }

        Ok(attributes)
    }

    fn parse_trait(&mut self, mctx: &mut ParserModuleCtx) -> Result<Vec<Node>, &'static str> {
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
                Token::Def => {
                    self.parse_def(mctx, "".to_string(), "".to_string(), name.clone(), None)
                }
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

    fn parse_impl(
        &mut self,
        mctx: &mut ParserModuleCtx,
        class_name: String,
    ) -> Result<Vec<Node>, &'static str> {
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

        if let Some(nodes) = self.index.trait_index.get_mut(&impl_name) {
            nodes.push(Class {
                name: class_name.clone(),
                attributes: vec![],
            });
        } else {
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
                Token::Def => self.parse_def(
                    mctx,
                    class_name.clone(),
                    impl_name.clone(),
                    "".to_string(),
                    None,
                ),
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
        mctx: &mut ParserModuleCtx,
        class_name: String,
        impl_name: String,
        trait_name: String,
        new_function: Option<&Def>,
    ) -> Result<Vec<Node>, &'static str> {
        // Advance past 'def' keyword
        self.pos += 1;

        let prototype = self.parse_prototype(mctx)?;

        self.advance_optional_whitespace();

        let mut ctx = ParserFunctionCtx {
            class_name: mctx.class_name.clone(),
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
                    let expr = self.parse_expr(mctx, &ctx)?;
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

        // let namespaced_fn_name = format!("{}.{}", mctx.class_name.clone(), def_node.prototype.name.clone());
        // self.index.fn_prototype_index.insert(namespaced_fn_name, def_node.prototype.clone());
        self.index
            .fn_prototype_index
            .insert(def_node.prototype.name.clone(), def_node.prototype.clone());

        Ok(vec![Node::Def(def_node)])

        // let mut arg_return_types = vec![];
        // let fn_name = if !def_node.class_name.is_empty() {
        //     if !def_node.impl_name.is_empty() {
        //         arg_return_types.push(BaseType::Class(def_node.impl_name.clone()));
        //         format!(
        //             "{}:{}:{}",
        //             &def_node.class_name, &def_node.impl_name, &def_node.prototype.name
        //         )
        //     } else {
        //         if def_node.class_name == "Str" {
        //             arg_return_types.push(BaseType::Class("Str".to_string()))
        //         } else {
        //             arg_return_types.push(BaseType::Class(def_node.impl_name.clone()));
        //         }
        //         format!("{}:{}", &def_node.class_name, &def_node.prototype.name)
        //     }
        // } else if !def_node.trait_name.is_empty() {
        //     arg_return_types.push(BaseType::Class(def_node.trait_name.clone()));
        //     format!("{}:{}", &def_node.trait_name, &def_node.prototype.name)
        // } else {
        //     def_node.prototype.name.clone()
        // };

        // for arg in &def_node.prototype.args {
        //     arg_return_types.push(arg.return_type.clone());
        // }
        // self.index.fn_index.insert(fn_name, arg_return_types);
    }

    fn parse_def_e(&mut self, mctx: &mut ParserModuleCtx) -> Result<Vec<Node>, &'static str> {
        // Advance past 'def' keyword
        self.pos += 1;

        let prototype = self.parse_prototype(mctx)?;

        self.advance_optional_whitespace();

        let def_e_node = DefE { prototype };

        // let mut arg_return_types = vec![];

        // let fn_name = def_e_node.prototype.name.clone();

        // for arg in &def_e_node.prototype.args {
        //     arg_return_types.push(arg.return_type.clone());
        // }
        // self.index.fn_index.insert(fn_name, arg_return_types);

        // let namespaced_fn_name = format!(".{}", def_e_node.prototype.name.clone());
        // self.index.fn_prototype_index.insert(namespaced_fn_name, def_e_node.prototype.clone());
        self.index.fn_prototype_index.insert(
            def_e_node.prototype.name.clone(),
            def_e_node.prototype.clone(),
        );

        Ok(vec![Node::DefE(def_e_node)])
    }

    /// Parses the prototype of a function, whether external or user-defined.
    fn parse_prototype(&mut self, mctx: &mut ParserModuleCtx) -> Result<Prototype, &'static str> {
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
            _ => return Err("Expected identifier in prototype declaration."),
        };

        let mut id = id;

        self.advance_optional_space();

        let mut args = vec![];

        if !mctx.class_name.is_empty() {
            args.push(Arg {
                name: "sret".to_string(),
                return_type: BaseType::Class(mctx.class_name.clone()),
            });

            id = format!("{}.{}", mctx.class_name, id);
        }

        match self.curr() {
            Token::Arrow => {
                let return_type = self.parse_return_type()?;
                return Ok(Prototype {
                    name: id,
                    args,
                    return_type,
                    is_op: is_operator,
                    prec: precedence,
                });
            }
            Token::LParen => {
                self.advance();
            }
            Token::NewLine(_) => {
                self.advance();

                return Ok(Prototype {
                    name: id,
                    args,
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
                args,
                return_type,
                is_op: is_operator,
                prec: precedence,
            });
        }

        loop {
            self.advance_optional_whitespace();

            // println!("{:#?}", self.curr());

            let arg_name = match self.curr() {
                Token::Ident(pos, name) => name,
                _ => return Err("Expected identifier in parameter declaration."),
            };

            self.advance()?;
            self.advance_optional_space();

            let return_type = match self.curr() {
                Token::Const(pos, type_name) => self.class_base_type(type_name),
                Token::LSquareBrace => {
                    self.advance();

                    let length = match self.current()? {
                        Token::Number(_, n) => n,
                        _ => return Err("Expected length of array"),
                    };

                    self.advance_optional_space();

                    match self.current()? {
                        Token::Ident(_, sym) => {
                            self.advance();

                            match sym.as_str() {
                                "x" => {
                                    self.advance();
                                    self.advance_optional_space();
                                }
                                _ => return Err("Expected an 'x' for such as [4 x Byte]"),
                            }
                        }
                        _ => return Err("Expected type for array 3"),
                    };

                    let array_return_type = match self.current()? {
                        Token::Const(_type_pos, type_name) => {
                            self.advance();
                            self.class_base_type(type_name)
                        }
                        _ => return Err("Expected type for array 4"),
                    };

                    match self.current()? {
                        Token::RSquareBrace => self.advance(),
                        _ => return Err("Expected ] to end array type"),
                    };

                    BaseType::Array(length as i64, Box::new(array_return_type))
                }
                _ => return Err("Expected type name for argument"),
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
                self.advance()?;
                return Ok(None);
            }
            Token::Arrow => {
                self.advance()?;
                self.advance_optional_space();
            }
            Token::Space(_) => {
                self.advance_optional_space();

                match self.curr() {
                    Token::Arrow => {
                        self.advance()?;
                        self.advance_optional_space();
                    }
                    // Token::NewLine(_) => {
                    //     self.advance();
                    //     return Ok(None);
                    // }
                    _ => return Err("Expected an arrow to indicate a return type"),
                }
            }
            _ => return Err("Expected an end to the function definition"),
        }

        match self.curr() {
            Token::Const(pos, type_name) => {
                self.advance()?;
                Ok(Some(self.class_base_type(type_name)))
            }
            _ => Err("Expected a return type after an arrow"),
        }
    }

    fn parse_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.parse_unary_expr(mctx, ctx) {
            Ok(left) => {
                self.advance_optional_whitespace();
                self.parse_binary_expr(mctx, ctx, 0, left)
            }
            err => err,
        }
    }

    /// Parses an unary expression.
    fn parse_unary_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        let op = match self.current()? {
            Token::Op(ch) => {
                self.advance()?;
                ch
            }
            _ => return self.parse_primary(mctx, ctx),
        };

        let mut name = String::from("unary");

        name.push(op);

        Ok(Node::Call(Call {
            fn_name: name,
            args: vec![self.parse_unary_expr(mctx, ctx)?],
            return_type: None,
        }))
    }

    fn parse_primary(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        // println!("current:");
        // println!("{:#?}", self.curr());

        let node = match self.curr() {
            Token::Attribute(_, _) => self.parse_attribute_expr(mctx, ctx),
            Token::Const(_, _) => self.parse_const_expr(mctx, ctx),
            Token::Ident(_, _) => self.parse_ident_expr(mctx, ctx),
            Token::Loop => self.parse_loop_expr(mctx, ctx),
            Token::LParen => self.parse_paren_expr(mctx, ctx),
            Token::LSquareBrace => self.parse_array_expr(mctx, ctx),
            Token::Number(_, _) => self.parse_nb_expr(),
            Token::Ret => self.parse_ret_expr(mctx, ctx),
            Token::SelfRef => self.parse_self_ref_expr(mctx, ctx),
            Token::StringLiteral(_, _) => self.parse_string_expr(),
            _ => {
                println!("Debug:");
                println!("{:#?}", self.curr());

                // panic!("{:#?}", self.curr());
                // panic!("{:#?}", self);
                Err("Unknown expression.")
            }
        };

        self.advance_optional_whitespace();

        match self.curr() {
            Token::Dot => self.parse_dot_expr(mctx, ctx, node),
            _ => node,
        }
    }

    fn parse_attribute_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Attribute(pos, name) => {
                self.advance();
                self.advance_optional_whitespace();

                // let node = match self.peek()? {
                //     Token::Assign => return self.parse_assign_attribute_expr(mctx, ctx),
                //     _ => {}
                // };

                let receiver = Box::new(Node::SelfRef(SelfRef {
                    return_type: BaseType::Class(mctx.class_name.clone()),
                }));

                let message = Box::new(Node::Attribute(Attribute {
                    name,
                    index: 0,
                    return_type: BaseType::Class("".to_string()),
                }));

                Ok(Node::Access(Access {
                    receiver,
                    message,
                    index: 0,
                    return_type: None,
                }))
            }
            _ => Err("Expected SelfRef"),
        }
    }

    fn parse_ret_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.curr() {
            Token::Ret => {
                if !ctx.parsing_returnable_loc {
                    return Err("Return can only be used at the root of a function.");
                }
                self.advance()?;
                self.advance_optional_whitespace();

                Ok(Node::Ret(Ret {
                    value: Box::new(self.parse_expr(mctx, ctx)?),
                }))
            }
            _ => Err("Expected Ret"),
        }
    }

    fn parse_self_ref_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.curr() {
            Token::SelfRef => {
                self.advance();

                Ok(Node::SelfRef(SelfRef {
                    return_type: BaseType::Class(ctx.class_name.clone()),
                }))
            }
            _ => Err("Expected SelfRef"),
        }
    }

    /// Parses an expression that starts with an identifier (either a variable or a function call).
    fn parse_ident_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
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

                    args.push(self.parse_expr(mctx, ctx)?);

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
                            value: Box::new(self.parse_expr(mctx, ctx)?),
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
                                        Node::Call(call) => {
                                            self.pajama_class_name(&call.return_type)
                                        }
                                        Node::Int(_) => "Int".to_string(),
                                        Node::LocalVar(val) => val.pajama_class_name().to_string(),
                                        Node::Send(send) => {
                                            self.pajama_class_name(&send.return_type)
                                        }
                                        Node::StringLiteral(_) => "Str".to_string(),
                                        Node::BuildStruct(build) => {
                                            return Ok(Node::LocalVar(LocalVar {
                                                name: ident_name,
                                                return_type: Some(build.return_type.clone()),
                                            }))
                                        }
                                        Node::Array(array) => {
                                            return Ok(Node::LocalVar(LocalVar {
                                                name: ident_name,
                                                return_type: Some(BaseType::Array(
                                                    array.length,
                                                    Box::new(array.item_type.clone()),
                                                )),
                                            }))
                                        }
                                        _ => {
                                            println!("{:#?}", asgnLvar.value.as_ref());
                                            return Err("Local variable assignment was given an unsupprted node, given");
                                        }
                                    };

                                    Ok(Node::LocalVar(LocalVar {
                                        name: ident_name,
                                        return_type: Some(BaseType::Class(return_type_name)),
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

                                println!("{:#?}", ident_name);

                                match arg_assignment {
                                    Some(arg) => Ok(Node::LocalVar(LocalVar {
                                        name: ident_name,
                                        return_type: Some(BaseType::Class(
                                            arg.pajama_class_name().to_string(),
                                        )),
                                    })),
                                    // maybe a function reference, or just a typo lool
                                    None => Ok(Node::LocalVar(LocalVar {
                                        name: ident_name,
                                        return_type: None,
                                    })),
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
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
        receiver: Result<Node, &'static str>,
    ) -> Result<Node, &'static str> {
        let receiver = match receiver {
            Ok(node) => node,
            Err(err) => return Err(err),
        };

        self.advance();

        let node = match self.peek()? {
            Token::LParen => match self.parse_dot_send_expr(mctx, ctx) {
                Ok(node) => Ok(Node::Send(Send {
                    receiver: Box::new(receiver),
                    message: Box::new(node),
                    return_type: None,
                })),
                Err(err) => return Err(err),
            },
            _ => match self.parse_dot_attribute_expr(mctx, ctx) {
                Ok(node) => Ok(Node::Access(Access {
                    receiver: Box::new(receiver),
                    message: Box::new(node),
                    index: 0,
                    return_type: None,
                })),
                Err(err) => return Err(err),
            },
            // _ => return Err("Expected attribute or method call"),
        };

        self.advance_optional_whitespace();

        match self.curr() {
            Token::Dot => self.parse_dot_expr(mctx, ctx, node),
            Token::Assign => self.parse_assignment_expr(mctx, ctx, node),
            _ => node,
        }
    }

    fn parse_assignment_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
        receiver: Result<Node, &'static str>,
    ) -> Result<Node, &'static str> {
        let receiver = match receiver {
            Ok(node) => node,
            Err(err) => return Err(err),
        };

        self.advance();
        self.advance_optional_whitespace();

        let value = Box::new(self.parse_expr(mctx, ctx).unwrap());

        match receiver {
            Node::Access(access) => Ok(Node::AssignAttributeAccess(AssignAttributeAccess {
                access,
                value,
            })),
            Node::AssignAttribute(_) => todo!(),
            Node::AssignAttributeAccess(_) => todo!(),
            Node::AssignLocalVar(_) => todo!(),
            Node::Attribute(_) => todo!(),
            Node::Binary(_) => todo!(),
            Node::Call(_) => todo!(),
            Node::Class(_) => todo!(),
            Node::Const(_) => todo!(),
            Node::Def(_) => todo!(),
            Node::DefE(_) => todo!(),
            Node::Impl(_) => todo!(),
            Node::Int(_) => todo!(),
            Node::LocalVar(_) => todo!(),
            Node::Loop(_) => todo!(),
            Node::Module(_) => todo!(),
            Node::Ret(_) => todo!(),
            Node::SelfRef(_) => todo!(),
            Node::Send(_) => todo!(),
            Node::StringLiteral(_) => todo!(),
            Node::Trait(_) => todo!(),
            Node::AssignConstant(_) => todo!(),
            Node::Struct(_) => todo!(),
            Node::BuildStruct(_) => todo!(),
            Node::Array(_) => todo!(),
            Node::FnRef(_) => todo!(),
        }
    }

    fn parse_dot_send_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        self.parse_ident_expr(mctx, ctx)
    }

    fn parse_dot_attribute_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.current()? {
            Token::Ident(_pos, ident_name) => {
                self.advance()?;
                Ok(Node::Attribute(Attribute {
                    name: ident_name,
                    index: 0,
                    return_type: BaseType::Class("".to_string()),
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
                Ok(Node::StringLiteral(StringLiteral { value: string }))
            }
            _ => Err("Expected string literal."),
        }
    }

    fn parse_const_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        // match self.curr() {
        //     Token::Const(pos, string) => {
        //         self.advance();
        //         Ok(Node::Const(Const { name: string }))
        //     }
        //     _ => Err("Expected string literal."),
        // }

        let const_name = match self.curr() {
            Token::Const(pos, name) => {
                self.advance();
                name
            }
            _ => return Err("Expected string literal."),
        };

        match self.curr() {
            Token::LParen => {
                self.advance()?;
                self.advance_optional_whitespace();

                if let Token::RParen = self.curr() {
                    return Err("At least one struct field is required");
                }

                let mut args = vec![];

                loop {
                    self.advance_optional_whitespace();

                    args.push(self.parse_expr(mctx, ctx)?);

                    self.advance_optional_whitespace();

                    match self.curr() {
                        Token::RParen => {
                            self.advance();
                            break;
                        }
                        Token::Comma => {
                            self.advance();
                        }
                        _ => return Err("Expected ',' or ')' character in struct build."),
                    }
                }

                Ok(Node::BuildStruct(BuildStruct {
                    name: const_name.clone(),
                    args,
                    return_type: BaseType::Struct(const_name.clone()),
                }))
            }
            _ => Ok(Node::Const(Const { name: const_name })),
        }
    }

    /// Parses an expression enclosed in parenthesis.
    fn parse_paren_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.current()? {
            Token::LParen => (),
            _ => return Err("Expected '(' character at start of parenthesized expression."),
        }

        self.advance_optional_whitespace();
        self.advance()?;

        let expr = self.parse_expr(mctx, ctx)?;

        self.advance_optional_whitespace();

        match self.current()? {
            Token::RParen => self.advance()?,
            _ => return Err("Expected ')' character at end of parenthesized expression."),
        };

        Ok(expr)
    }

    fn parse_array_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        match self.current()? {
            Token::LSquareBrace => (),
            _ => return Err("Expected '[' character at start of an array."),
        }

        self.advance_optional_whitespace();
        self.advance()?;

        let mut items = vec![];

        match self.current()? {
            Token::RSquareBrace => {
                self.advance()?;
                let default_type = BaseType::Byte;
                return Ok(Node::Array(Array {
                    items,
                    item_type: default_type,
                    length: 0,
                }));
            }
            _ => {}
        }

        loop {
            self.advance_optional_whitespace();

            items.push(self.parse_expr(mctx, ctx)?);

            self.advance_optional_whitespace();

            match self.curr() {
                Token::RSquareBrace => {
                    self.advance();
                    break;
                }
                Token::Comma => {
                    self.advance();
                }
                _ => return Err("Expected ',' or ']' character in array."),
            }
        }

        // Hardcode byte for now. Use semantic analysis to look at the first
        // element for dynamic types
        let item_type = BaseType::Byte;

        let length = items.len() as i64;

        Ok(Node::Array(Array {
            items,
            item_type,
            length,
        }))
    }

    fn parse_loop_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
    ) -> Result<Node, &'static str> {
        self.pos += 1; // Advance past 'loop' keyword
        self.advance_optional_whitespace();

        match self.current()? {
            Token::LCurlyBrace => self.advance()?,
            _ => return Err("Expected a curly brace after loop"),
        }

        let mut body = vec![];

        loop {
            self.advance_optional_whitespace();

            match self.current()? {
                Token::RCurlyBrace => {
                    self.advance();
                    break;
                }
                _ => {
                    body.push(self.parse_expr(mctx, &ctx)?);
                }
            }
        }

        let loop_node = Loop { body };

        Ok(Node::Loop(loop_node))
    }

    /// Parses a binary expression, given its left-hand expression.
    fn parse_binary_expr(
        &mut self,
        mctx: &mut ParserModuleCtx,
        ctx: &ParserFunctionCtx,
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

            let mut op: [char; 4] = ['\0'; 4];

            // Single char op
            match self.curr() {
                Token::Op(op_part) => op[0] = op_part,
                _ => return Err("Invalid operator."),
            };

            self.advance()?;
            self.advance_optional_whitespace();

            // Two char op
            match self.curr() {
                Token::Op(op_part) => {
                    op[1] = op_part;
                    self.advance()?;

                    // Three char op
                    match self.curr() {
                        Token::Op(op_part) => {
                            op[2] = op_part;
                            self.advance()?;

                            // Four char op
                            match self.curr() {
                                Token::Op(op_part) => {
                                    op[3] = op_part;
                                    self.advance()?;
                                },
                                _ => {},
                            };

                        },
                        _ => {},
                    };
                },
                _ => {},
            };

            self.advance_optional_whitespace();

            let mut right = self.parse_unary_expr(mctx, ctx)?;
            let next_prec = self.get_tok_precedence();

            self.advance_optional_whitespace();

            if curr_prec < next_prec {
                right = self.parse_binary_expr(mctx, ctx, curr_prec + 1, right)?;
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
                Token::Comment(_, _) => {
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

    pub fn pajama_class_name(&self, return_type: &'a Option<BaseType>) -> String {
        match return_type {
            Some(rt) => match rt {
                BaseType::Array(_, _) => "Array".to_string(),
                BaseType::Byte => "Byte".to_string(),
                BaseType::BytePtr => "BytePtr".to_string(),
                BaseType::Class(class_name) => class_name.to_string(),
                BaseType::FnRef => "FnRef".to_string(),
                BaseType::Int => "Int".to_string(),
                BaseType::Int16 => "Int16".to_string(),
                BaseType::Int32 => "Int32".to_string(),
                BaseType::Int64 => "Int64".to_string(),
                BaseType::Struct(_) => "Struct".to_string(),
                BaseType::Void => "".to_string(),
            },
            None => "".to_string(),
        }
    }

    pub fn class_base_type(&self, type_name: String) -> BaseType {
        match type_name.as_str() {
            // "Array" => BaseType::Array(_, _),
            "Byte" => BaseType::Byte,
            "BytePtr" => BaseType::BytePtr,
            "Int" => BaseType::Int,
            "Int16" => BaseType::Int16,
            "Int32" => BaseType::Int32,
            "Int64" => BaseType::Int64,
            "FnRef" => BaseType::FnRef,
            _name => BaseType::Class(_name.to_string()), // BaseType::Void => "".to_string(),
        }
    }
}
