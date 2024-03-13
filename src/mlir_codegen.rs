use crate::parser::{BaseType, Def, Node, Parser, ParserResult, ParserResultIndex};
use crate::{mi_malloc, parser};
use melior::dialect::llvm::attributes::{linkage, Linkage};
use melior::dialect::llvm::AllocaOptions;
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
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

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

// #[derive(Debug)]
// pub enum ReturnValue<'a> {
//     IntValue(IntValue<'a>),
//     ArrayPtrValue(PointerValue<'a>),
//     StructPtrValue(PointerValue<'a>),
//     StructValue(StructValue<'a>),
//     VoidValue,
// }

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct LlvmTypes<'ctx> {
    // pub parser_result: &'a ParserResult,
    pub i8_type: Type<'ctx>,
    pub i8_ptr_type: Type<'ctx>,
    pub i8_array_type: Type<'ctx>,
    pub i8_array_ptr_type: Type<'ctx>,
    pub i64_type: Type<'ctx>,
    pub struct_type: Type<'ctx>,
    pub struct_ptr_type: Type<'ctx>,
}

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct Compiler<'a, 'ctx> {
    pub parser_result: &'a ParserResult,
    pub context: &'ctx Context,
    pub mlir_module: &'a Module<'ctx>,
    pub llvm_types: LlvmTypes<'ctx>,
    pub parser_index: &'a ParserResultIndex,
    // pub class_ids: BTreeMap<String, u64>,
    // pub fn_value_opt: Option<FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub struct Ctx<'ctx, 'a> {
    pub lvars: HashMap<String, Value<'ctx, 'a>>,
    pub global_var_counter: i32,
}

// #[derive(Debug)]
// pub struct LocalVarRef<'a> {
//     alloca: LLVMValue<'a>,
//     return_type: LLVMType<'a>,
// }

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(parser_result: &ParserResult, parser: &Parser) {
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);

        let context = Context::new();
        context.append_dialect_registry(&registry);
        context.load_all_available_dialects();
        register_all_llvm_translations(&context);

        // temp for development
        context.attach_diagnostic_handler(|diagnostic| {
            eprintln!("{}", diagnostic);
            true
        });

        let i8_type = IntegerType::new(&context, 8).into();
        let i8_ptr_type = llvm::r#type::r#pointer(i8_type, 0);
        let i8_array_type = llvm::r#type::array(i8_type, 5);
        let i8_array_ptr_type = llvm::r#type::r#pointer(i8_array_type, 0);
        let i64_type = IntegerType::new(&context, 64);

        let struct_fields = [i8_ptr_type.into(), i64_type.into(), i64_type.into()];
        let struct_type = llvm::r#type::r#struct(&context, &struct_fields, false);
        let struct_ptr_type = llvm::r#type::r#pointer(struct_type, 0);

        let llvm_types = LlvmTypes {
            i8_type,
            i8_ptr_type,
            i8_array_type,
            i8_array_ptr_type,
            struct_type,
            struct_ptr_type,
            i64_type: i64_type.into(),
        };

        let location = Location::unknown(&context);
        let mut mlir_module = Module::new(location);

        let mut compiler = Compiler {
            parser_result: &parser_result,
            context: &context,
            mlir_module: &mlir_module,
            llvm_types,
            parser_index: &parser.index,
        };

        compiler.compile_ast();

        println!("PRE VERIFICATION:");
        println!("{}", mlir_module.body().to_string());

        assert!(mlir_module.as_operation().verify());

        let pass_manager = PassManager::new(&context);
        pass_manager.add_pass(pass::conversion::create_func_to_llvm());

        pass_manager
            .nested_under("func.func")
            .add_pass(pass::conversion::create_arith_to_llvm());
        pass_manager
            .nested_under("func.func")
            .add_pass(pass::conversion::create_index_to_llvm());
        pass_manager.add_pass(pass::conversion::create_scf_to_control_flow());
        pass_manager.add_pass(pass::conversion::create_control_flow_to_llvm());
        pass_manager.add_pass(pass::conversion::create_finalize_mem_ref_to_llvm());

        pass_manager.add_pass(pass::conversion::create_func_to_llvm());

        pass_manager.run(&mut mlir_module).unwrap();

        assert!(mlir_module.as_operation().verify());

        let engine = ExecutionEngine::new(&mlir_module, 2, &[], false);

        // let mut argument = 42;
        // let mut argument2 = 42;
        // let mut result = -1;

        println!("POST:");
        println!("{}", mlir_module.body().to_string());

        // unsafe {
        //     engine.invoke_packed(
        //         "add",
        //         &mut [
        //             &mut argument as *mut i32 as *mut (),
        //             &mut argument2 as *mut i32 as *mut (),
        //             &mut result as *mut i32 as *mut (),
        //         ],
        //     ).unwrap();

        //     println!("result: {:#?}", result);
        // }

        let mut status_code = 0;

        unsafe {
            engine
                .invoke_packed("main", &mut [&mut status_code as *mut i32 as *mut ()])
                .unwrap();
        }
    }

    fn compile_ast(&mut self) {
        match &self.parser_result.ast {
            Node::Module(module) => {
                self.compile_module(module);
            }
            _ => {
                panic!("Expected module to compile")
            }
        }
    }

    fn compile_module(&mut self, module: &parser::Module) {
        for node in module.body.iter() {
            match &node {
                Node::Class(class) => self.compile_class(class),
                Node::Def(def) => self.compile_fn(def),
                Node::DefE(def_e) => self.compile_external_fn(def_e),
                Node::Ret(ret) => todo!(),
                Node::Access(_) => todo!(),
                Node::AssignLocalVar(_) => todo!(),
                Node::Attribute(_) => todo!(),
                Node::Binary(_) => todo!(),
                Node::Call(_) => todo!(),
                Node::Impl(_) => todo!(),
                Node::Int(_) => todo!(),
                Node::InterpolableString(_) => todo!(),
                Node::LocalVar(_) => todo!(),
                Node::Module(_) => todo!(),
                Node::SelfRef(_) => todo!(),
                Node::Send(_) => todo!(),
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

    fn compile_fn(&mut self, node: &parser::Def) {
        let name = StringAttribute::new(&self.context, &node.prototype.name);

        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match arg.return_type {
                parser::BaseType::Int => inputs.push(IntegerType::new(&self.context, 64).into()),
                parser::BaseType::StringType => {
                    inputs.push(self.llvm_types.struct_ptr_type.clone().into())
                }
                parser::BaseType::Void => todo!(),
                parser::BaseType::Undef(_) => todo!(),
                parser::BaseType::BytePtr => {
                    inputs.push(self.llvm_types.i8_ptr_type.clone().into())
                }
            }
        }

        let fn_signature = if node.main_fn {
            let i32_type = IntegerType::new(&self.context, 32).into();
            TypeAttribute::new(FunctionType::new(&self.context, &inputs, &[i32_type]).into())
        } else {
            let results = match &node.prototype.return_type {
                Some(rt) => vec![self.basetype_to_mlir_type(&rt)],
                None => vec![],
            };
            // let void_type = llvm::r#type::void(&self.context);
            TypeAttribute::new(FunctionType::new(&self.context, &inputs, &results).into())
        };

        let region = self.compile_fn_body(node).unwrap();

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
            name,
            fn_signature,
            region,
            &attributes,
            location,
        );

        self.mlir_module.body().append_operation(operation);
    }

    fn compile_external_fn(&mut self, node: &parser::DefE) {
        let name = StringAttribute::new(&self.context, &node.prototype.name);

        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match arg.return_type {
                parser::BaseType::Int => inputs.push(IntegerType::new(&self.context, 64).into()),
                parser::BaseType::StringType => todo!(),
                parser::BaseType::Void => todo!(),
                parser::BaseType::Undef(_) => todo!(),
                parser::BaseType::BytePtr => {
                    inputs.push(self.llvm_types.i8_ptr_type.clone().into())
                }
            }
        }

        let results = match &node.prototype.return_type {
            Some(rt) => vec![self.basetype_to_mlir_type(rt)],
            None => {
                // let void_type = llvm::r#type::void(&self.context);
                // vec![void_type]
                vec![]
            }
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

        self.mlir_module.body().append_operation(operation);
    }

    fn compile_fn_body(&mut self, node: &parser::Def) -> Result<Region<'ctx>, &'static str> {
        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match arg.return_type {
                parser::BaseType::Int => inputs.push((
                    IntegerType::new(&self.context, 64).into(),
                    Location::unknown(&self.context),
                )),
                parser::BaseType::StringType => inputs.push((
                    self.llvm_types.struct_ptr_type.clone().into(),
                    Location::unknown(&self.context),
                )),
                parser::BaseType::Void => todo!(),
                parser::BaseType::Undef(_) => todo!(),
                parser::BaseType::BytePtr => inputs.push((
                    self.llvm_types.i8_ptr_type.clone().into(),
                    Location::unknown(&self.context),
                )),
            }
        }

        let block = Block::new(&inputs);
        let region = Region::new();
        let mut ctx = Ctx {
            lvars: HashMap::new(),
            global_var_counter: 0,
            // local_vars: HashMap<String, Value<'ctx, '_>> = HashMap::new(),
        };

        if node.body.iter().len() == 0 {
            panic!("Empty body not supported")
        }

        for (index, arg) in node.prototype.args.iter().enumerate() {
            let return_val = block.argument(index).unwrap().into();
            let ptr = self.append_alloca_store(return_val, &block);
            ctx.lvars.insert(arg.name.clone(), ptr);
        }

        let last_op_index = node.body.len();

        for (i, body_node) in node.body.iter().enumerate() {
            let return_val = match self.compile_expr(&block, body_node, &mut ctx) {
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
                                block.append_operation(func::r#return(
                                    &[return_val.unwrap()],
                                    Location::unknown(&self.context),
                                ));
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

        region.append_block(block);

        Ok(region)
    }

    fn compile_expr(
        &self,
        block: &'a Block<'ctx>,
        expr: &Node,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        match *&expr {
            Node::Access(node) => self.compile_attribute_access(block, node, ctx),
            Node::AssignLocalVar(asgn_lvar) => {
                self.compile_assign_local_var(block, asgn_lvar, ctx)
            }
            Node::Binary(binary) => self.compile_binary(block, binary, ctx),
            Node::Call(call) => self.compile_call(block, call, ctx),
            Node::Int(nb) => self.compile_int(block, nb),
            Node::InterpolableString(string) => self.compile_interpolable_string(block, string, ctx),
            Node::LocalVar(lvar) => self.compile_local_var(block, lvar, ctx),
            Node::Ret(ret) => self.compile_return(block, ret, ctx),
            Node::SelfRef(lvar) => self.compile_self_ref(block, lvar),
            Node::Send(node) => self.compile_send(block, node),
            Node::Attribute(_) => panic!("Syntax error"),
            Node::Class(_) => panic!("Syntax error"),
            Node::Def(_) => panic!("Syntax error"),
            Node::DefE(_) => panic!("Syntax error"),
            Node::Impl(_) => panic!("Syntax error"),
            Node::Module(_) => panic!("Syntax error"),
            Node::Trait(_) => panic!("Syntax error"),
        }
    }

    fn compile_attribute_access(
        &self,
        block: &'a Block<'ctx>,
        node: &parser::Access,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        let value = match &*node.receiver {
            Node::LocalVar(lvar) => {
                let lvar_value = ctx.lvars.get(&lvar.name).unwrap();
                let attribute_index = node.index;

                match &lvar.return_type {
                    Some(rt) => match rt {
                        BaseType::BytePtr => todo!(),
                        BaseType::Int => todo!(),
                        BaseType::StringType => todo!(),
                        BaseType::Void => todo!(),
                        BaseType::Undef(name) => match name.as_str() {
                            "Str" => {
                                let result_type = match &node.return_type {
                                    Some(rt) => match rt {
                                        BaseType::BytePtr => self.llvm_types.i8_ptr_type,
                                        BaseType::Int => self.llvm_types.i64_type,
                                        BaseType::StringType => todo!(),
                                        BaseType::Void => todo!(),
                                        BaseType::Undef(_) => todo!(),
                                    },
                                    None => todo!(),
                                };

                                let loaded_val = block.append_operation(llvm::load(
                                    &self.context,
                                    *lvar_value,
                                    self.llvm_types.struct_ptr_type,
                                    Location::unknown(&self.context),
                                    Default::default(),
                                ));

                                let gep = block.append_operation(llvm::get_element_ptr(
                                    &self.context,
                                    loaded_val.result(0).unwrap().into(),
                                    DenseI32ArrayAttribute::new(
                                        &self.context,
                                        &[0, attribute_index],
                                    ),
                                    // integer_type,
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
                            _ => todo!(),
                        },
                    },
                    None => todo!(),
                }
            }
            _ => todo!(),
        };

        Ok(Some(value.result(0).unwrap().into()))
    }

    fn compile_send(
        &self,
        block: &'a Block<'ctx>,
        node: &parser::Send,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        todo!()
    }

    fn compile_self_ref(
        &self,
        block: &'a Block<'ctx>,
        lvar: &parser::SelfRef,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        todo!()
    }

    fn compile_call(
        &self,
        block: &'a Block<'ctx>,
        call: &parser::Call,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
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
                Node::InterpolableString(interpolablestring_node) => {
                    self.basetype_to_mlir_type(&BaseType::StringType)
                }
                Node::LocalVar(localvar_node) => {
                    self.basetype_to_mlir_type(&localvar_node.return_type.as_ref().unwrap())
                }
                _ => return Err("Syntax Error: Invalid argument type"),
            };

            inputs.push(arg_return_type);
        }

        let void_type = llvm::r#type::void(&self.context);

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
            let value = self.compile_expr(block, &arg, ctx).unwrap();
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

    fn compile_int(
        &self,
        block: &'a Block<'ctx>,
        nb: &parser::Int,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
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

    fn compile_interpolable_string(
        &self,
        block: &'a Block<'ctx>,
        string: &parser::InterpolableString,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        let string_attr = StringAttribute::new(&self.context, &string.value);
        let i8_array_type = llvm::r#type::array(self.llvm_types.i8_type, string.value.len() as u32);

        let region = Region::new();
        let temp_name = ctx.global_var_counter.to_string();

        self.mlir_module.body().append_operation(llvm::global(
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

        ctx.global_var_counter += 1;

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

        let temp_name = ctx.global_var_counter.to_string();

        self.mlir_module.body().append_operation(llvm::global(
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

        ctx.global_var_counter += 1;

        return Ok(Some(struct_addressof_op));
    }

    fn compile_binary(
        &self,
        block: &'a Block<'ctx>,
        binary: &parser::Binary,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        todo!()
    }

    fn compile_local_var(
        &self,
        block: &'a Block<'ctx>,
        lvar: &parser::LocalVar,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
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

    fn compile_assign_local_var(
        &self,
        block: &'a Block<'ctx>,
        asgn_lvar: &parser::AssignLocalVar,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        let return_val = match self.compile_expr(&block, &asgn_lvar.value, ctx) {
            Ok(ret_val) => ret_val,
            Err(e) => return Err(e),
        };

        let ptr = self.append_alloca_store(return_val.unwrap(), block);
        ctx.lvars.insert(asgn_lvar.name.clone(), ptr);
        // local_vars.insert(asgn_lvar.name.clone(), return_val.unwrap());

        Ok(return_val)
    }

    fn append_alloca_store(
        &self,
        value: Value<'ctx, '_>,
        block: &'a Block<'ctx>,
    ) -> Value<'ctx, 'a> {
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

    fn compile_return(
        &self,
        block: &'a Block<'ctx>,
        ret: &parser::Ret,
        ctx: &mut Ctx<'ctx, 'a>,
    ) -> Result<Option<Value<'ctx, 'a>>, &'static str> {
        let return_val = match self.compile_expr(&block, &ret.value, ctx) {
            Ok(ret_val) => ret_val,
            Err(e) => return Err(e),
        };

        Ok(return_val)
    }

    fn basetype_to_mlir_type(&self, return_type: &BaseType) -> Type<'ctx> {
        match return_type {
            BaseType::Int => IntegerType::new(&self.context, 64).into(),
            BaseType::StringType => self.llvm_types.struct_ptr_type,
            BaseType::Void => todo!(),
            BaseType::Undef(name) => match name.as_str() {
                "Str" => self.llvm_types.struct_ptr_type,
                _ => todo!(),
            },
            BaseType::BytePtr => self.llvm_types.i8_ptr_type.clone().into(),
        }
    }
}
