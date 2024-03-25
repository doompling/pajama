use crate::parser::{BaseType, Def, Node, ParserResult};
use crate::{mi_malloc, parser};
use melior::dialect::llvm::attributes::{linkage, Linkage};
use melior::dialect::llvm::AllocaOptions;
use melior::dialect::scf;
use melior::dialect::{index, llvm, memref};
use melior::ir::attribute::{
    ArrayAttribute, DenseElementsAttribute, DenseI32ArrayAttribute, DenseI64ArrayAttribute,
    FlatSymbolRefAttribute, IntegerAttribute,
};
use melior::ir::operation::{OperationBuilder, OperationResult};
use melior::ir::r#type::{IntegerType, MemRefType, RankedTensorType};
use melior::pass::PassManager;
use melior::{
    dialect::{arith, func, func::call, llvm::r#type, DialectRegistry},
    ir::{
        attribute::{StringAttribute, TypeAttribute},
        r#type::FunctionType,
        *,
    },
    utility::register_all_dialects,
    utility::register_all_llvm_translations,
    Context,
};
use melior::{pass, ExecutionEngine, StringRef};
use std::array;
use std::borrow::BorrowMut;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use train_map::TrainMap;

#[no_mangle]
pub fn print_int(int: i32) {
    println!("print_int: {:#?}", int);
}

#[no_mangle]
pub fn print_bytes(bytes: *const u8, len: i64) {
    println!("bytes: {:#?}", bytes);
    println!("len: {:#?}", len);

    let slice = unsafe { std::slice::from_raw_parts(bytes, len as usize) };

    for byte in slice {
        print!("{}", *byte as char);
    }
    // stdout().flush().unwrap(); // Ensure output is displayed
}

#[used]
static EXTERNAL_FNS3: [fn(i32); 1] = [print_int];
#[used]
static EXTERNAL_FNS4: [fn(*const u8, i64); 1] = [print_bytes];

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct LlvmTypes<'c> {
    // pub parser_result: &'a ParserResult,
    pub i8_type: Type<'c>,
    pub i8_ptr_type: Type<'c>,
    pub i8_array_type: Type<'c>,
    pub i8_array_ptr_type: Type<'c>,
    pub i64_type: Type<'c>,
    pub struct_type: Type<'c>,
    pub struct_ptr_type: Type<'c>,
    pub ptr_type: Type<'c>,
}

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct Compiler<'c, 'm> {
    pub parser_result: &'c ParserResult,
    pub context: &'m Context,
    pub module: &'c Module<'m>,
    pub llvm_types: LlvmTypes<'c>,
    pub class_type_index: HashMap<String, Type<'m>>,

    // pub llvm_types: LlvmTypes<'m>,
    // pub class_type_index: HashMap<String, Type<'m>>,

    // pub parser_index: &'a ParserResultIndex,
    // pub class_ids: BTreeMap<String, u64>,
    // pub fn_value_opt: Option<FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub struct ModuleCtx {
    pub global_var_counter: i32,
}

#[derive(Debug)]
pub struct FnCtx<'c, 'a> {
    pub lvars: HashMap<String, Value<'c, 'a>>,
    pub lvar_stores: HashMap<String, Value<'c, 'a>>,
}

impl<'c, 'm> Compiler<'c, 'm> {
    pub fn new(context: &'c Context, module: &'m Module<'c>, parser_result: &'m ParserResult) -> Self {
        let i8_type = IntegerType::new(context, 8).into();
        let i8_ptr_type = llvm::r#type::r#pointer(i8_type, 0);
        let i8_array_type = llvm::r#type::array(i8_type, 5);
        let i8_array_ptr_type = llvm::r#type::r#pointer(i8_array_type, 0);
        let i64_type = IntegerType::new(context, 64);

        let struct_fields = [i8_ptr_type.into(), i64_type.into(), i64_type.into()];
        let struct_type = llvm::r#type::r#struct(context, &struct_fields, false);
        let struct_ptr_type = llvm::r#type::r#pointer(struct_type, 0);
        let ptr_type = llvm::r#type::opaque_pointer(context);

        let llvm_types = LlvmTypes {
            i8_type,
            i8_ptr_type,
            i8_array_type,
            i8_array_ptr_type,
            struct_type,
            struct_ptr_type,
            i64_type: i64_type.into(),
            ptr_type,
        };

        let mut class_type_index = HashMap::new();
        for (_, class) in &parser_result.index.class_index {
            let mut struct_fields = vec![];

            for attribute in &class.attributes {
                if let Node::Attribute(attribute) = attribute {
                    struct_fields.push(match attribute.return_type {
                        BaseType::BytePtr => llvm_types.i8_ptr_type,
                        BaseType::Int => llvm_types.i64_type.into(),
                        BaseType::Void => todo!(),
                        BaseType::Class(_) => llvm_types.ptr_type,
                    });
                }
            }

            let struct_type = llvm::r#type::r#struct(context, &struct_fields, false);
            class_type_index.insert(class.name.clone(), struct_type);
        }

        Self {
            context,
            module,
            parser_result,
            llvm_types,
            class_type_index
        }
    }

    pub fn compile(&mut self) -> Result<(), &'static str> {
        // let registry = DialectRegistry::new();
        // register_all_dialects(&registry);

        // let context = Context::new();
        // context.append_dialect_registry(&registry);
        // context.load_all_available_dialects();
        // register_all_llvm_translations(&context);

        // // temp for development
        // context.attach_diagnostic_handler(|diagnostic| {
        //     eprintln!("{}", diagnostic);
        //     true
        // });

        // let i8_type = IntegerType::new(&self.context, 8).into();
        // let i8_ptr_type = llvm::r#type::r#pointer(i8_type, 0);
        // let i8_array_type = llvm::r#type::array(i8_type, 5);
        // let i8_array_ptr_type = llvm::r#type::r#pointer(i8_array_type, 0);
        // let i64_type = IntegerType::new(&self.context, 64);

        // let struct_fields = [i8_ptr_type.into(), i64_type.into(), i64_type.into()];
        // let struct_type = llvm::r#type::r#struct(&self.context, &struct_fields, false);
        // let struct_ptr_type = llvm::r#type::r#pointer(struct_type, 0);
        // let ptr_type = llvm::r#type::opaque_pointer(&self.context);

        // let llvm_types = LlvmTypes {
        //     i8_type,
        //     i8_ptr_type,
        //     i8_array_type,
        //     i8_array_ptr_type,
        //     struct_type,
        //     struct_ptr_type,
        //     i64_type: i64_type.into(),
        //     ptr_type,
        // };

        // let location = Location::unknown(&context);
        // let mut module = Module::new(location);

        // let mut class_type_index = HashMap::new();
        // for (_, class) in &self.parser_result.index.class_index {
        //     let mut struct_fields = vec![];

        //     for attribute in &class.attributes {
        //         if let Node::Attribute(attribute) = attribute {
        //             struct_fields.push(match attribute.return_type {
        //                 BaseType::BytePtr => self.llvm_types.i8_ptr_type,
        //                 BaseType::Int => self.llvm_types.i64_type.into(),
        //                 BaseType::Void => todo!(),
        //                 BaseType::Class(_) => self.llvm_types.ptr_type,
        //             });
        //         }
        //     }

        //     let struct_type = llvm::r#type::r#struct(&self.context, &struct_fields, false);
        //     class_type_index.insert(class.name.clone(), struct_type);
        // }

        // let mut compiler = Compiler {
        //     parser_result: &parser_result,
        //     context: &context,
        //     module: &module,
        //     llvm_types,
        //     class_type_index,
        // };

        self.compile_ast();

        // println!("PRE VERIFICATION:");
        // println!("{}", self.module.body().to_string());

        // assert!(self.module.as_operation().verify());

        // let pass_manager = PassManager::new(&self.context);
        // pass_manager.add_pass(pass::conversion::create_func_to_llvm());

        // pass_manager
        //     .nested_under("func.func")
        //     .add_pass(pass::conversion::create_arith_to_llvm());
        // pass_manager
        //     .nested_under("func.func")
        //     .add_pass(pass::conversion::create_index_to_llvm());
        // pass_manager.add_pass(pass::conversion::create_scf_to_control_flow());
        // pass_manager.add_pass(pass::conversion::create_control_flow_to_llvm());
        // pass_manager.add_pass(pass::conversion::create_finalize_mem_ref_to_llvm());

        // pass_manager.add_pass(pass::conversion::create_func_to_llvm());

        // pass_manager.run(self.module.borrow_mut()).unwrap();

        // assert!(self.module.as_operation().verify());

        // // let engine = ExecutionEngine::new(&module, 2, &[], false);
        // let engine = ExecutionEngine::new(&self.module, 2, &[], false);

        // // let mut argument = 42;
        // // let mut argument2 = 42;
        // // let mut result = -1;

        // println!("POST:");
        // println!("{}", self.module.body().to_string());

        // // unsafe {
        // //     engine.invoke_packed(
        // //         "add",
        // //         &mut [
        // //             &mut argument as *mut i32 as *mut (),
        // //             &mut argument2 as *mut i32 as *mut (),
        // //             &mut result as *mut i32 as *mut (),
        // //         ],
        // //     ).unwrap();

        // //     println!("result: {:#?}", result);
        // // }

        // let mut status_code = 0;

        // unsafe {
        //     engine
        //         .invoke_packed("main", &mut [&mut status_code as *mut i32 as *mut ()])
        //         .unwrap();
        // }

        Ok(())
    }

    fn compile_ast(&mut self) {
        match &self.parser_result.module {
            Node::Module(module) => {
                self.compile_module(module);
            }
            _ => {
                panic!("Expected module to compile")
            }
        }
    }

    fn compile_module(&mut self, module: &parser::Module) {
        let mut mctx = ModuleCtx {
            global_var_counter: 0,
        };

        for node in module.methods.iter() {
            match &node {
                Node::Def(def) => self.compile_def(def, &mut mctx),
                Node::DefE(def_e) => self.compile_external_fn(def_e),
                Node::Access(_) => todo!(),
                Node::AssignAttribute(_) => todo!(),
                Node::AssignAttributeAccess(_) => todo!(),
                Node::AssignLocalVar(_) => todo!(),
                Node::Attribute(_) => todo!(),
                Node::Binary(_) => todo!(),
                Node::Call(_) => todo!(),
                Node::Class(_) => panic!("Classes are not directly compiled"),
                Node::Const(_) => todo!(),
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
            }
        }
    }

    // fn compile_class(&mut self, class: &parser::Class) {
    //     let (_, last_id) = self.class_ids.iter().last().unwrap_or((&"".to_string(), &0));
    //     let base_class_offset = 2;

    //     let new_id = if *last_id == 0 {
    //         last_id + base_class_offset
    //     } else {
    //         last_id + 1
    //     };

    //     self.class_ids.insert(class.name.clone(), new_id);
    // }

    fn compile_class(&mut self, node: &parser::Class) {
        // :D
    }

    fn compile_def(&mut self, node: &parser::Def, mctx: &mut ModuleCtx) {
        let fn_name = StringAttribute::new(&self.context, node.prototype.name.as_str());
        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match &arg.return_type {
                parser::BaseType::Int => inputs.push(IntegerType::new(&self.context, 64).into()),
                parser::BaseType::Void => todo!(),
                parser::BaseType::BytePtr => {
                    inputs.push(self.llvm_types.i8_ptr_type.clone().into())
                }
                parser::BaseType::Class(class_name) => {
                    let struct_type = self.class_type_index.get(class_name).unwrap();
                    inputs.push(llvm::r#type::r#pointer(*struct_type, 0));
                }
            }
        }

        let fn_signature = if node.main_fn {
            let i32_type = IntegerType::new(&self.context, 32).into();
            TypeAttribute::new(FunctionType::new(&self.context, &inputs, &[i32_type]).into())
        } else {
            let mut results = match &node.prototype.return_type {
                Some(rt) => vec![self.basetype_to_mlir_type(&rt)],
                None => vec![],
            };

            if node.prototype.name.ends_with(".new") {
                // value is returned by sret
                results = vec![];
            }

            TypeAttribute::new(FunctionType::new(&self.context, &inputs, &results).into())
        };

        let region = self.compile_fn_body(node, mctx).unwrap();

        let mut attributes = vec![
            // (
            //     Identifier::new(&self.context, "sym_visibility"),
            //     StringAttribute::new(&self.context, "private").into(),
            // ),
        ];

        if node.main_fn {
            attributes.push((
                Identifier::new(&self.context, "llvm.emit_c_interface"),
                Attribute::parse(&self.context, &format!("unit")).unwrap(),
            ));
        }

        let location = Location::unknown(&self.context);
        let operation = func::func(
            &self.context,
            fn_name,
            fn_signature,
            region,
            &attributes,
            location,
        );

        self.module.body().append_operation(operation);
    }

    fn compile_external_fn(&mut self, node: &parser::DefE) {
        let name = StringAttribute::new(&self.context, &node.prototype.name);
        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match arg.return_type {
                parser::BaseType::Int => inputs.push(IntegerType::new(&self.context, 64).into()),
                parser::BaseType::Void => todo!(),
                parser::BaseType::Class(_) => todo!(),
                parser::BaseType::BytePtr => {
                    inputs.push(self.llvm_types.i8_ptr_type.clone().into())
                }
            }
        }

        let results = match &node.prototype.return_type {
            Some(rt) => vec![self.basetype_to_mlir_type(rt)],
            None => vec![],
        };

        let fn_signature =
            TypeAttribute::new(FunctionType::new(&self.context, &inputs, &results).into());
        let region = Region::new();
        let attributes = &[(
            Identifier::new(&self.context, "sym_visibility"),
            StringAttribute::new(&self.context, "private").into(),
        )];
        let location = Location::unknown(&self.context);
        let operation = func::func(
            &self.context,
            name,
            fn_signature,
            region,
            attributes,
            location,
        );

        self.module.body().append_operation(operation);
    }

    fn compile_fn_body(
        &mut self,
        node: &parser::Def,
        mctx: &mut ModuleCtx,
    ) -> Result<Region<'c>, &'static str> {
        let mut inputs = vec![];
        for arg in node.prototype.args.iter() {
            match &arg.return_type {
                parser::BaseType::Int => inputs.push((
                    IntegerType::new(&self.context, 64).into(),
                    Location::unknown(&self.context),
                )),
                parser::BaseType::Void => todo!(),
                parser::BaseType::BytePtr => inputs.push((
                    self.llvm_types.i8_ptr_type.clone().into(),
                    Location::unknown(&self.context),
                )),
                parser::BaseType::Class(name) => {
                    let struct_type = self.class_type_index.get(name).unwrap();
                    inputs.push((
                        llvm::r#type::r#pointer(*struct_type, 0),
                        Location::unknown(&self.context),
                    ));
                }
            }
        }

        let block = Block::new(&inputs);
        let mut ctx = FnCtx {
            lvars: HashMap::new(),
            lvar_stores: HashMap::new(),
        };

        if node.body.iter().len() == 0 {
            panic!("Empty body not supported")
        }

        for (index, arg) in node.prototype.args.iter().enumerate() {
            match arg.return_type {
                BaseType::BytePtr => {}
                BaseType::Int => {}
                BaseType::Void => {}
                BaseType::Class(_) => {
                    // When a class is the first argument
                    // if index == 0 {
                    let arg_n = block.argument(index).unwrap();
                    ctx.lvars.insert(arg.name.clone(), arg_n.into());
                    continue;
                    // }
                }
            };

            let return_val = block.argument(index).unwrap().into();
            let ptr = self.append_alloca_store(return_val, &block);
            ctx.lvars.insert(arg.name.clone(), ptr);
        }

        let last_op_index = node.body.len();

        for (i, body_node) in node.body.iter().enumerate() {
            let compiled_expr = self.compile_expr(&block, body_node, &mut ctx, mctx);
            let return_val = match compiled_expr {
                Ok(ret_val) => ret_val,
                Err(e) => return Err(e),
            };

            let last_node = i == last_op_index - 1;
            if last_node {
                if node.main_fn {
                    let success_int_value = block
                        .append_operation(arith::constant(
                            &self.context,
                            IntegerAttribute::new(
                                IntegerType::new(&self.context, 32).into(),
                                1 as i64,
                            )
                            .into(),
                            Location::unknown(&self.context),
                        ))
                        .result(0)
                        .unwrap()
                        .into();

                    block.append_operation(func::r#return(
                        &[success_int_value],
                        Location::unknown(&self.context),
                    ));
                } else {
                    match &node.prototype.return_type {
                        Some(rt) => match rt {
                            BaseType::Void => {
                                todo!()
                            }
                            _ => {
                                if node.prototype.name.ends_with(".new") {
                                    block.append_operation(func::r#return(
                                        &[],
                                        Location::unknown(&self.context),
                                    ));
                                } else {
                                    block.append_operation(func::r#return(
                                        &[return_val.unwrap()],
                                        Location::unknown(&self.context),
                                    ));
                                }
                            }
                        },
                        None => {
                            block.append_operation(func::r#return(
                                &[],
                                Location::unknown(&self.context),
                            ));
                        }
                    }
                }
            }
        }

        let region = Region::new();
        region.append_block(block);

        Ok(region)
    }

    fn compile_expr<'a>(
        &self,
        block: &'a Block<'c>,
        expr: &Node,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        match *&expr {
            Node::Access(node) => self.compile_attribute_access(block, node, ctx, mctx),
            Node::AssignAttribute(node) => self.compile_assign_attribute(block, node, ctx, mctx),
            Node::AssignAttributeAccess(node) => {
                self.compile_assign_attribute_access(block, node, ctx, mctx)
            }
            Node::AssignLocalVar(asgn_lvar) => {
                self.compile_assign_local_var(block, asgn_lvar, ctx, mctx)
            }
            Node::Binary(binary) => self.compile_binary(block, binary, ctx, mctx),
            Node::Call(call) => self.compile_call(block, call, ctx, mctx),
            Node::Int(nb) => self.compile_int(block, nb),
            Node::LocalVar(lvar) => self.compile_local_var(block, lvar, ctx, mctx),
            Node::Loop(node) => self.compile_loop(block, node, ctx, mctx),
            Node::Ret(ret) => self.compile_return(block, ret, ctx, mctx),
            Node::SelfRef(lvar) => self.compile_self_ref(block, lvar),
            Node::Send(node) => self.compile_send(block, node, ctx, mctx),
            Node::StringLiteral(string) => self.compile_string_literal(block, string, ctx, mctx),
            Node::Attribute(_) => panic!("Syntax error"),
            Node::Class(_) => panic!("Syntax error"),
            Node::Const(_) => todo!(),
            Node::Def(_) => panic!("Syntax error"),
            Node::DefE(_) => panic!("Syntax error"),
            Node::Impl(_) => panic!("Syntax error"),
            Node::Module(_) => panic!("Syntax error"),
            Node::Trait(_) => panic!("Syntax error"),
        }
    }

    fn compile_attribute_access<'a>(
        &self,
        block: &'a Block<'c>,
        access: &parser::Access,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let value = match &*access.receiver {
            Node::LocalVar(lvar) => {
                let lvar_value = ctx.lvars.get(&lvar.name).unwrap();
                let attribute_index = access.index;

                match &lvar.return_type {
                    Some(rt) => match rt {
                        BaseType::BytePtr => todo!(),
                        BaseType::Int => todo!(),
                        BaseType::Void => todo!(),
                        BaseType::Class(_lvar_type_name) => {
                            // Load the attribute access
                            let result_type = match &access.return_type {
                                Some(rt) => match rt {
                                    BaseType::BytePtr => self.llvm_types.i8_ptr_type,
                                    BaseType::Int => self.llvm_types.i64_type,
                                    BaseType::Void => todo!(),
                                    BaseType::Class(attribute_type_name) => {
                                        let struct_type = *self
                                            .class_type_index
                                            .get(attribute_type_name)
                                            .unwrap();
                                        llvm::r#type::r#pointer(struct_type, 0)
                                    }
                                },
                                None => todo!(),
                            };

                            let gep = block.append_operation(llvm::get_element_ptr(
                                &self.context,
                                *lvar_value,
                                DenseI32ArrayAttribute::new(&self.context, &[0, attribute_index]),
                                llvm::r#type::r#pointer(result_type, 0),
                                Location::unknown(&self.context),
                            ));

                            let load_op = block.append_operation(llvm::load(
                                &self.context,
                                gep.result(0).unwrap().into(),
                                result_type,
                                Location::unknown(&self.context),
                                Default::default(),
                            ));

                            load_op
                        }
                    },
                    None => todo!(),
                }
            }
            Node::SelfRef(self_ref) => {
                let lvar_value = ctx.lvars.get("sret").unwrap();
                let attribute_index = access.index;

                match &self_ref.return_type {
                    BaseType::BytePtr => todo!(),
                    BaseType::Int => todo!(),
                    BaseType::Void => todo!(),
                    BaseType::Class(self_type_name) => {
                        // Load the attribute access
                        let result_type = match &access.return_type {
                            Some(rt) => match rt {
                                BaseType::BytePtr => self.llvm_types.i8_ptr_type,
                                BaseType::Int => self.llvm_types.i64_type,
                                BaseType::Void => todo!(),
                                BaseType::Class(attribute_type_name) => {
                                    let struct_type =
                                        self.class_type_index.get(attribute_type_name).unwrap();
                                    llvm::r#type::r#pointer(*struct_type, 0)
                                }
                            },
                            None => todo!(),
                        };

                        let gep = block.append_operation(llvm::get_element_ptr(
                            &self.context,
                            *lvar_value,
                            DenseI32ArrayAttribute::new(&self.context, &[0, attribute_index]),
                            llvm::r#type::r#pointer(result_type, 0),
                            Location::unknown(&self.context),
                        ));

                        let load_op = block.append_operation(llvm::load(
                            &self.context,
                            gep.result(0).unwrap().into(),
                            result_type,
                            Location::unknown(&self.context),
                            Default::default(),
                        ));

                        load_op
                    }
                }
            }
            _ => todo!(),
        };

        Ok(Some(value.result(0).unwrap().into()))
    }

    fn compile_send<'a>(
        &self,
        block: &'a Block<'c>,
        send_node: &parser::Send,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let call_node = match send_node.message.as_ref() {
            Node::Call(call_node) => call_node,
            _ => return Err("Expected send_node message to be a Call"),
        };

        let (class_name, value) = match send_node.receiver.as_ref() {
            Node::LocalVar(local_var) => match &local_var.return_type {
                Some(rt) => match rt {
                    BaseType::Int => todo!(),
                    BaseType::Void => todo!(),
                    BaseType::BytePtr => todo!(),
                    BaseType::Class(class_name) => {
                        let value = ctx.lvars.get(&local_var.name).unwrap();
                        // let value = self.compile_local_var(block, local_var, ctx, mctx);
                        (class_name.clone(), Ok(Some(*value)))
                    }
                },
                None => todo!(),
            },
            Node::Const(const_node) => {
                // Instantiating a class requires a sret
                let value = if call_node.fn_name.ends_with(".new") {
                    let class_type = self.class_type_index.get(&const_node.name).unwrap();
                    // add sret
                    self.append_alloca_class(class_type.clone(), block)
                } else {
                    // Class methods
                    todo!()
                };

                (const_node.name.clone(), Ok(Some(value)))
            }
            Node::Access(access) => (
                nilla_class_name(&access.return_type.clone().unwrap()),
                self.compile_attribute_access(block, access, ctx, mctx),
            ),
            _ => return Err("Send only implements LocalVar so far"),
        };

        // let scoped_fn_name = format!("{}.{}", class_name, call_node.fn_name);
        let receiver_value = value.unwrap().unwrap();

        let mut inputs = vec![receiver_value.r#type()];

        for arg in &call_node.args {
            let arg_return_type = match &arg {
                Node::Access(access_node) => {
                    self.basetype_to_mlir_type(&access_node.return_type.as_ref().unwrap())
                }
                Node::SelfRef(selfref_node) => todo!(),
                Node::Binary(binary_node) => todo!(),
                Node::Call(call_node) => {
                    self.basetype_to_mlir_type(&call_node.return_type.as_ref().unwrap())
                }
                Node::Send(send_node) => {
                    self.basetype_to_mlir_type(&send_node.return_type.as_ref().unwrap())
                }
                Node::Int(int_node) => self.basetype_to_mlir_type(&BaseType::Int),
                Node::StringLiteral(interpolablestring_node) => {
                    self.basetype_to_mlir_type(&BaseType::Class("Str".to_string()))
                }
                Node::LocalVar(localvar_node) => {
                    self.basetype_to_mlir_type(&localvar_node.return_type.as_ref().unwrap())
                }
                _ => return Err("Syntax Error: Invalid argument type"),
            };

            inputs.push(arg_return_type);
        }

        let location = Location::unknown(&self.context);
        let mut result = match &send_node.return_type {
            Some(base_type) => vec![self.basetype_to_mlir_type(&base_type)],
            None => vec![],
        };

        if call_node.fn_name.ends_with(".new") {
            result = vec![];
        }

        let function_type = FunctionType::new(&self.context, &inputs, &result);

        let function = block.append_operation(func::constant(
            &self.context,
            FlatSymbolRefAttribute::new(&self.context, &call_node.fn_name.as_str()),
            function_type,
            location,
        ));

        let mut compiled_args = vec![receiver_value];

        for arg in &call_node.args {
            let arg_value = self.compile_expr(block, &arg, ctx, mctx).unwrap();
            compiled_args.push(arg_value.unwrap());
        }

        if let Some(_) = &send_node.return_type {
            if call_node.fn_name.ends_with(".new") {
                block.append_operation(func::call_indirect(
                    function.result(0).unwrap().into(),
                    &compiled_args,
                    &function_type.result(0).into_iter().collect::<Vec<_>>(),
                    location,
                ));

                value
            } else {
                let value = block
                    .append_operation(func::call_indirect(
                        function.result(0).unwrap().into(),
                        &compiled_args,
                        &function_type.result(0).into_iter().collect::<Vec<_>>(),
                        location,
                    ))
                    .result(0)
                    .unwrap()
                    .into();
                Ok(Some(value))
            }
        } else {
            block.append_operation(func::call_indirect(
                function.result(0).unwrap().into(),
                &compiled_args,
                &function_type.result(0).into_iter().collect::<Vec<_>>(),
                location,
            ));

            Ok(None)
        }
    }

    fn compile_self_ref<'a>(
        &self,
        block: &'a Block<'c>,
        lvar: &parser::SelfRef,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        todo!()
    }

    fn compile_call<'a>(
        &self,
        block: &'a Block<'c>,
        call: &parser::Call,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let location = Location::unknown(&self.context);

        let mut inputs = vec![];
        for arg in &call.args {
            let arg_return_type = match &arg {
                Node::Access(access_node) => {
                    self.basetype_to_mlir_type(&access_node.return_type.as_ref().unwrap())
                }
                Node::SelfRef(selfref_node) => todo!(),
                Node::Binary(binary_node) => todo!(),
                Node::Call(call_node) => {
                    self.basetype_to_mlir_type(&call_node.return_type.as_ref().unwrap())
                }
                Node::Send(send_node) => {
                    self.basetype_to_mlir_type(&send_node.return_type.as_ref().unwrap())
                }
                Node::Int(int_node) => self.basetype_to_mlir_type(&BaseType::Int),
                Node::StringLiteral(interpolablestring_node) => {
                    self.basetype_to_mlir_type(&BaseType::Class("Str".to_string()))
                }
                Node::LocalVar(localvar_node) => {
                    self.basetype_to_mlir_type(&localvar_node.return_type.as_ref().unwrap())
                }
                _ => return Err("Syntax Error: Invalid argument type"),
            };

            inputs.push(arg_return_type);
        }

        let result = match &call.return_type {
            Some(base_type) => vec![self.basetype_to_mlir_type(&base_type)],
            None => vec![],
        };

        let function_type = FunctionType::new(&self.context, &inputs, &result);

        let function = block.append_operation(func::constant(
            &self.context,
            FlatSymbolRefAttribute::new(&self.context, call.fn_name.as_str()),
            function_type,
            location,
        ));

        let mut compiled_args = vec![];

        for arg in &call.args {
            let value = self.compile_expr(block, &arg, ctx, mctx).unwrap();
            compiled_args.push(value.unwrap());
        }

        if let Some(_) = &call.return_type {
            let value = block
                .append_operation(func::call_indirect(
                    function.result(0).unwrap().into(),
                    &compiled_args,
                    &function_type.result(0).into_iter().collect::<Vec<_>>(),
                    location,
                ))
                .result(0)
                .unwrap()
                .into();

            Ok(Some(value))
        } else {
            block.append_operation(func::call_indirect(
                function.result(0).unwrap().into(),
                &compiled_args,
                &function_type.result(0).into_iter().collect::<Vec<_>>(),
                location,
            ));

            Ok(None)
        }
    }

    fn compile_int<'a>(
        &self,
        block: &'a Block<'c>,
        nb: &parser::Int,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let value = block
            .append_operation(arith::constant(
                &self.context,
                IntegerAttribute::new(IntegerType::new(&self.context, 64).into(), nb.value as i64)
                    .into(),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        Ok(Some(value))
    }

    fn compile_string_literal<'a>(
        &self,
        block: &'a Block<'c>,
        string: &parser::StringLiteral,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let string_attr = StringAttribute::new(&self.context, &string.value);
        let i8_array_type = llvm::r#type::array(self.llvm_types.i8_type, string.value.len() as u32);

        let region = Region::new();
        let temp_name = mctx.global_var_counter.to_string();

        self.module.body().append_operation(llvm::global(
            &self.context,
            StringAttribute::new(&self.context, temp_name.as_str()),
            Some(string_attr.into()),
            i8_array_type,
            region,
            Location::unknown(&self.context),
        ));

        let region = Region::new();
        let string_block = Block::new(&[]);

        let addressof_op = string_block
            .append_operation(llvm::addressof(
                &self.context,
                temp_name.as_str(),
                llvm::r#type::pointer(i8_array_type, 0),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        mctx.global_var_counter += 1;

        let buffer_gep = string_block
            .append_operation(llvm::get_element_ptr(
                &self.context,
                addressof_op,
                DenseI32ArrayAttribute::new(&self.context, &[0, 0]),
                self.llvm_types.i8_ptr_type,
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let undef_struct = string_block
            .append_operation(llvm::undef(
                self.llvm_types.struct_type,
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let prev_result = string_block.append_operation(llvm::insert_value(
            &self.context,
            undef_struct,
            DenseI64ArrayAttribute::new(&self.context, &[0]),
            buffer_gep,
            Location::unknown(&self.context),
        ));

        let string_length = string.value.len() as i64;

        let length_const = string_block
            .append_operation(arith::constant(
                &self.context,
                IntegerAttribute::new(self.llvm_types.i64_type, string_length).into(),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let max_length_const = string_block
            .append_operation(arith::constant(
                &self.context,
                IntegerAttribute::new(self.llvm_types.i64_type, string_length).into(),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let prev_result = string_block.append_operation(llvm::insert_value(
            &self.context,
            prev_result.result(0).unwrap().into(),
            DenseI64ArrayAttribute::new(&self.context, &[1]),
            length_const,
            Location::unknown(&self.context),
        ));

        let last_undef_op = string_block.append_operation(llvm::insert_value(
            &self.context,
            prev_result.result(0).unwrap().into(),
            DenseI64ArrayAttribute::new(&self.context, &[2]),
            max_length_const,
            Location::unknown(&self.context),
        ));

        string_block.append_operation(llvm::r#return(
            Some(last_undef_op.result(0).unwrap().into()),
            Location::unknown(&self.context),
        ));

        region.append_block(string_block);

        let temp_name = mctx.global_var_counter.to_string();

        self.module.body().append_operation(llvm::global(
            &self.context,
            StringAttribute::new(&self.context, temp_name.as_str()),
            None,
            self.llvm_types.struct_type,
            region,
            Location::unknown(&self.context),
        ));

        let struct_addressof_op = block
            .append_operation(llvm::addressof(
                &self.context,
                temp_name.as_str(),
                self.llvm_types.struct_ptr_type,
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        mctx.global_var_counter += 1;

        return Ok(Some(struct_addressof_op));
    }

    fn compile_binary<'a>(
        &self,
        block: &'a Block<'c>,
        binary: &parser::Binary,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        todo!()
    }

    fn compile_local_var<'a>(
        &self,
        block: &'a Block<'c>,
        lvar: &parser::LocalVar,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        match ctx.lvars.get(&lvar.name) {
            Some(value) => return Ok(Some(*value)),
            None => todo!(),
        }

        let loaded_val = block
            .append_operation(llvm::load(
                &self.context,
                *ctx.lvars.get(&lvar.name).unwrap(),
                self.llvm_types.struct_ptr_type,
                Location::unknown(&self.context),
                Default::default(),
            ))
            .result(0)
            .unwrap()
            .into();

        Ok(Some(loaded_val))
    }

    fn compile_loop<'a>(
        &self,
        block: &'a Block<'c>,
        loop_node: &parser::Loop,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let location = Location::unknown(&self.context);
        let index_type = Type::index(&self.context);

        let initial = block.append_operation(arith::constant(
            &self.context,
            // IntegerAttribute::new(index_type, 1).into(),
            IntegerAttribute::new(IntegerType::new(&self.context, 1).into(), 1).into(),
            location,
        ));

        block.append_operation(scf::r#while(
            &[],
            &[],
            {
                let before_block = Block::new(&[]);

                // let condition = block.append_operation(arith::constant(
                //     &self.context,
                //     IntegerAttribute::new(IntegerType::new(&self.context, 1).into(), 0)
                //         .into(),
                //     location,
                // ));

                // let result = block.append_operation(arith::constant(
                //     &self.context,
                //     IntegerAttribute::new(Type::index(&self.context), 42).into(),
                //     location,
                // ));

                before_block.append_operation(scf::condition(
                    // arith::constant( &self.context, IntegerAttribute::new(IntegerType::new(&self.context, 1).into(), 0)).into()
                    initial.result(0).unwrap().into(),
                    &[],
                    location,
                ));

                let region = Region::new();
                region.append_block(before_block);
                region
            },
            {
                self.compile_block(&loop_node.body, mctx)?
            },
            location,
        ));

        Ok(None)
    }

    fn compile_block(
        &self,
        nodes: &Vec<Node>,
        // ctx: &mut FnCtx<'c, 'm>,
        mctx: &mut ModuleCtx,
    ) -> Result<Region<'c>, &'static str> {
        let builder = Block::new(&[]);

        let mut block_ctx = FnCtx {
            lvars: HashMap::new(),
            lvar_stores: HashMap::new(),
        };

        nodes.iter().for_each(|node| {
            self.compile_expr(&builder, node, &mut block_ctx, mctx);
        });

        builder.append_operation(scf::r#yield(
            &[],
            Location::unknown(&self.context),
        ));

        let region = Region::new();
        region.append_block(builder);
        Ok(region)
    }

// let builder = Block::new(&[]);
        // let mut variables = variables.fork();

        // let r#type =
            // self.compile_statements(&builder, &block.stmts, function_scope, &mut variables)?;

            // let after_block = Block::new(&[]);

            // // for node in loop_node.body.iter() {
            // //     self.compile_expr(&after_block, node, ctx, mctx);
            // // }

            // nodes.iter().for_each(|node| {
            //     self.compile_expr(&after_block, node, ctx, mctx);
            // });


            // //     match self.compile_expr(&after_block, node, &mut ctx, mctx) {
            // //     Ok(ret_val) => ret_val,
            // //     // Err(e) => return Err(e),
            // //     Err(e) => panic!("Waaah"),
            // // });

            // // after_block.append_operation(scf::r#yield(
            // //     &[],
            // //     location,
            // // ));

            // let region = Region::new();
            // region.append_block(after_block);
            // Ok(region)
    // }

    fn compile_assign_attribute_access<'a>(
        &self,
        block: &'a Block<'c>,
        assignment: &parser::AssignAttributeAccess,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let return_val = match self.compile_expr(&block, &assignment.value, ctx, mctx) {
            Ok(ret_val) => ret_val,
            Err(e) => return Err(e),
        };

        let class_ptr_ref = match assignment.access.receiver.as_ref() {
            Node::Access(_) => todo!(),
            Node::AssignLocalVar(_) => todo!(),
            Node::Attribute(_) => todo!(),
            Node::Binary(_) => todo!(),
            Node::Call(_) => todo!(),
            Node::Class(_) => todo!(),
            Node::Def(_) => todo!(),
            Node::DefE(_) => todo!(),
            Node::Impl(_) => todo!(),
            Node::Int(_) => todo!(),
            Node::StringLiteral(_) => todo!(),
            Node::LocalVar(lvar) => {
                ctx.lvars.get(&lvar.name).unwrap()

                // match ctx.lvar_stores.get(&lvar.name) {
                //     Some(_) => {},
                //     None => {
                //         ctx.lvars.get(&lvar.name).unwrap()
                //     },
                // }
            }
            Node::Module(_) => todo!(),
            Node::Ret(_) => todo!(),
            Node::SelfRef(_) => todo!(),
            Node::Send(_) => todo!(),
            Node::Trait(_) => todo!(),
            Node::AssignAttribute(_) => todo!(),
            Node::AssignAttributeAccess(_) => todo!(),
            Node::Const(_) => todo!(),
            Node::Loop(_) => todo!(),
        };

        // let sret_value = ctx.lvar_stores.get(&asgn_attr.name);

        let gep = block
            .append_operation(llvm::get_element_ptr(
                &self.context,
                *class_ptr_ref,
                DenseI32ArrayAttribute::new(&self.context, &[0, assignment.access.index]),
                llvm::r#type::r#pointer(return_val.unwrap().r#type(), 0),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        block.append_operation(llvm::store(
            &self.context,
            return_val.unwrap(),
            gep,
            Location::unknown(&self.context),
            Default::default(),
        ));

        return Ok(return_val);

        // match asgn_attr.value.as_ref() {
        //     Node::Access(_) => todo!(),
        //     Node::AssignLocalVar(_) => todo!(),
        //     Node::Attribute(_) => todo!(),
        //     Node::Binary(_) => todo!(),
        //     Node::Call(_) => todo!(),
        //     Node::Class(_) => todo!(),
        //     Node::Def(_) => todo!(),
        //     Node::DefE(_) => todo!(),
        //     Node::Impl(_) => todo!(),
        //     Node::Int(_) => todo!(),
        //     Node::StringLiteral(_) => todo!(),
        //     Node::LocalVar(lvar) => {
        //         match ctx.lvar_stores.get(&lvar.name) {
        //             Some(_) => {},
        //             None => {
        //                 let lvar_value = ctx.lvars.get(&lvar.name).unwrap();
        //                 let ptr = self.append_alloca_store(*lvar_value, block);
        //                 ctx.lvars.insert(lvar.name.clone(), ptr);
        //                 ctx.lvar_stores.insert(lvar.name.clone(), ptr);
        //             },
        //         }
        //     },
        //     Node::Module(_) => todo!(),
        //     Node::Ret(_) => todo!(),
        //     Node::SelfRef(_) => todo!(),
        //     Node::Send(_) => todo!(),
        //     Node::Trait(_) => todo!(),
        //     Node::AssignAttribute(_) => todo!(),
        //     Node::Const(_) => todo!(),
        //     Node::AssignAttributeAccess(_) => todo!(),
        // }

        // let return_val = match self.compile_expr(&block, &asgn_attr.value, ctx) {
        //     Ok(ret_val) => ret_val,
        //     Err(e) => return Err(e),
        // };

        // let sret_value = ctx.lvars.get("sret").unwrap();

        // let gep = block
        //     .append_operation(llvm::get_element_ptr(
        //         &self.context,
        //         *sret_value,
        //         DenseI32ArrayAttribute::new(&self.context, &[0, asgn_attr.index]),
        //         llvm::r#type::r#pointer(return_val.unwrap().r#type(), 0),
        //         Location::unknown(&self.context),
        //     ))
        //     .result(0)
        //     .unwrap()
        //     .into();

        // block.append_operation(llvm::store(
        //     &self.context,
        //     return_val.unwrap(),
        //     gep,
        //     Location::unknown(&self.context),
        //     Default::default(),
        // ));

        // Ok(return_val)

        // let return_type = match asgn_lvar.value.as_ref() {
        //     Node::Access(_) => todo!(),
        //     Node::AssignLocalVar(_) => todo!(),
        //     Node::Attribute(_) => todo!(),
        //     Node::Binary(_) => todo!(),
        //     Node::Call(_) => todo!(),
        //     Node::Class(_) => todo!(),
        //     Node::Def(_) => todo!(),
        //     Node::DefE(_) => todo!(),
        //     Node::Impl(_) => todo!(),
        //     Node::Int(_) => todo!(),
        //     Node::StringLiteral(_) => Some(BaseType::Class("Str".to_string())),
        //     Node::LocalVar(_) => todo!(),
        //     Node::Module(_) => todo!(),
        //     Node::Ret(_) => todo!(),
        //     Node::SelfRef(_) => todo!(),
        //     Node::Send(send_node) => send_node.return_type.clone(),
        //     Node::Trait(_) => todo!(),
        //     Node::AssignAttribute(_) => todo!(),
        //     Node::Const(_) => todo!(),
        // };

        // match &return_type {
        //     Some(base_type) => {
        //         match base_type {
        //             BaseType::BytePtr => {}
        //             BaseType::Int => {}
        //             BaseType::Void => {}
        //             BaseType::Class(class_name) => {
        //                 if class_name == "Str" {
        //                     // Todo: new .new for strings
        //                 } else {
        //                     ctx.lvars
        //                         .insert(asgn_lvar.name.clone(), return_val.unwrap());
        //                     return Ok(return_val);
        //                 }
        //             }
        //         }
        //     }
        //     None => todo!(),
        // }

        // let ptr = self.append_alloca_store(return_val.unwrap(), block);
        // ctx.lvars.insert(asgn_lvar.name.clone(), ptr);

        // Ok(return_val)

        todo!()
    }

    fn compile_assign_attribute<'a>(
        &self,
        block: &'a Block<'c>,
        asgn_attr: &parser::AssignAttribute,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        // let sret_value = ctx.lvar_stores.get(&asgn_attr.name);

        let return_val =
            match asgn_attr.value.as_ref() {
                Node::LocalVar(lvar) => match ctx.lvar_stores.get(&lvar.name) {
                    Some(value) => *value,
                    None => {
                        let lvar_value = ctx.lvars.get(&lvar.name).unwrap();
                        let return_type = lvar_value.r#type();
                        let ptr = self.append_alloca_store(*lvar_value, block);
                        ctx.lvars.insert(lvar.name.clone(), ptr);
                        ctx.lvar_stores.insert(lvar.name.clone(), ptr);

                        block.append_operation(llvm::load(
                            &self.context,
                            ptr,
                            return_type,
                            Location::unknown(&self.context),
                            Default::default(),
                        )).result(0).unwrap().into()
                    }
                },
                _ => {
                    match self.compile_expr(&block, &asgn_attr.value, ctx, mctx) {
                        Ok(ret_val) => ret_val.unwrap(),
                        Err(e) => return Err(e),
                    }
                }
            };

        let sret_value = ctx.lvars.get("sret").unwrap();

        let gep = block
            .append_operation(llvm::get_element_ptr(
                &self.context,
                *sret_value,
                DenseI32ArrayAttribute::new(&self.context, &[0, asgn_attr.index]),
                llvm::r#type::r#pointer(return_val.r#type(), 0),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        block.append_operation(llvm::store(
            &self.context,
            return_val,
            gep,
            Location::unknown(&self.context),
            Default::default(),
        ));

        Ok(Some(return_val))
    }

    fn compile_assign_local_var<'a>(
        &self,
        block: &'a Block<'c>,
        asgn_lvar: &parser::AssignLocalVar,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let return_val = match self.compile_expr(&block, &asgn_lvar.value, ctx, mctx) {
            Ok(ret_val) => ret_val,
            Err(e) => return Err(e),
        };

        let return_type = match asgn_lvar.value.as_ref() {
            Node::Send(send_node) => send_node.return_type.clone(),
            Node::StringLiteral(_) => Some(BaseType::Class("Str".to_string())),
            Node::Access(_) => todo!(),
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
            Node::Trait(_) => todo!(),
        };

        match &return_type {
            Some(base_type) => {
                match base_type {
                    BaseType::BytePtr => {}
                    BaseType::Int => {}
                    BaseType::Void => {}
                    BaseType::Class(class_name) => {
                        if class_name == "Str" {
                            // Todo: new .new for strings
                        } else {
                            ctx.lvars
                                .insert(asgn_lvar.name.clone(), return_val.unwrap());
                            // ctx.lvar_stores.insert(asgn_lvar.name.clone(), return_val.unwrap());
                            return Ok(return_val);
                        }
                    }
                }
            }
            None => todo!(),
        }

        let ptr = self.append_alloca_store(return_val.unwrap(), block);
        ctx.lvars.insert(asgn_lvar.name.clone(), ptr);
        ctx.lvar_stores.insert(asgn_lvar.name.clone(), ptr);

        Ok(return_val)
    }

    fn append_alloca_store<'a>(
        &self,
        value: Value<'m, '_>,
        block: &'a Block<'c>,
    ) -> Value<'c, 'a> {
        let size = block
            .append_operation(arith::constant(
                &self.context,
                IntegerAttribute::new(self.llvm_types.i64_type.clone(), 1).into(),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let ptr_type = r#type::pointer(value.r#type(), 0);
        let ptr = block
            .append_operation(llvm::alloca(
                &self.context,
                size,
                ptr_type,
                Location::unknown(&self.context),
                Default::default(),
                // AllocaOptions::new().elem_type(Some(TypeAttribute::new(value.r#type()))),
            ))
            .result(0)
            .unwrap()
            .into();

        block.append_operation(llvm::store(
            &self.context,
            value,
            ptr,
            Location::unknown(&self.context),
            Default::default(),
        ));

        ptr
    }

    fn append_alloca_class<'a>(
        &self,
        class_type: Type<'m>,
        block: &'a Block<'c>,
    ) -> Value<'c, 'a> {
        let size = block
            .append_operation(arith::constant(
                &self.context,
                IntegerAttribute::new(self.llvm_types.i64_type.clone(), 1).into(),
                Location::unknown(&self.context),
            ))
            .result(0)
            .unwrap()
            .into();

        let ptr_type = r#type::pointer(class_type, 0);

        block
            .append_operation(llvm::alloca(
                &self.context,
                size,
                ptr_type,
                Location::unknown(&self.context),
                Default::default(),
                // AllocaOptions::new().elem_type(Some(TypeAttribute::new(value.r#type()))),
            ))
            .result(0)
            .unwrap()
            .into()
    }

    fn compile_return<'a>(
        &self,
        block: &'a Block<'c>,
        ret: &parser::Ret,
        ctx: &mut FnCtx<'c, 'a>,
        mctx: &mut ModuleCtx,
    ) -> Result<Option<Value<'c, 'a>>, &'static str> {
        let return_val = match self.compile_expr(&block, &ret.value, ctx, mctx) {
            Ok(ret_val) => ret_val,
            Err(e) => return Err(e),
        };

        Ok(return_val)
    }

    fn basetype_to_mlir_type(&self, return_type: &BaseType) -> Type<'c> {
        match return_type {
            BaseType::Int => IntegerType::new(&self.context, 64).into(),
            BaseType::Void => todo!(),
            BaseType::Class(name) => {
                let struct_type = self.class_type_index.get(name).unwrap();
                llvm::r#type::r#pointer(*struct_type, 0)
            }
            BaseType::BytePtr => self.llvm_types.i8_ptr_type.clone().into(),
        }
    }
}

pub fn nilla_class_name(base_type: &BaseType) -> String {
    match base_type {
        BaseType::BytePtr => "BytePtr".to_string(),
        BaseType::Int => "Int".to_string(),
        BaseType::Void => "".to_string(),
        BaseType::Class(class_name) => class_name.clone(),
    }
}
