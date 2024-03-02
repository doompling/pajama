use crate::{mi_malloc, parser};
use crate::parser::{ParserResult, ParserResultIndex, Parser, Node};
use std::array;
use std::collections::{HashMap, BTreeMap};

// #[derive(Debug, Clone)]
// pub enum LLVMType<'a> {
//     StructType(StructType<'a>),
//     IntType(IntType<'a>),
//     VoidType(VoidType<'a>),
// }

// #[derive(Debug, Clone)]
// pub enum LLVMValue<'a> {
//     StructValue(StructValue<'a>),
//     PointerValue(PointerValue<'a>),
//     IntType(IntValue<'a>),
// }

#[derive(Debug)]
#[repr(C)]
pub struct NillaString {
    id: i32,
    buffer: *mut u8,
    length: i32,
    max_length: i32,
}

impl NillaString {
    #[no_mangle]
    fn sys_allocate_string(bytes: *const u8, length: i32) -> *mut NillaString {
        let ptr: *mut u8 = unsafe { mi_malloc(length as usize).cast() };

        unsafe {
            core::ptr::copy(bytes, ptr, length as usize);
        }

        let nilla_string = NillaString {
            id: 1,
            buffer: ptr,
            length,
            max_length: length,
        };

        println!("{:#?}", "Allocated:");
        println!("{:#?}", nilla_string);

        let size = core::mem::size_of::<NillaString>();
        let ptr: *mut NillaString = unsafe { mi_malloc(size).cast() };

        if !ptr.is_null() {
            unsafe { ptr.write(nilla_string) };
        }

        ptr
    }
}

// #[no_mangle]
// pub fn base_print(nilla_string: *const NillaString) {
//     if nilla_string.is_null() {
//         println!("Null pointer passed to print function");
//         return;
//     }

//     let string = unsafe { &*nilla_string };

//     if string.buffer.is_null() {
//         println!("Null buffer pointer passed to print function");
//         return;
//     }

//     let slice = unsafe { std::slice::from_raw_parts(string.buffer, string.length as usize) };
//     let str_value = std::str::from_utf8(slice);

//     match str_value {
//         Ok(value) => {
//             println!("");
//             println!("Message: {}", value);
//             println!("");
//         }
//         Err(_) => {
//             println!("Invalid UTF-8 sequence");
//         }
//     }
// }

#[no_mangle]
pub fn sys_print(string: NillaString) {
    println!("{:#?}", string);

    let slice = unsafe { std::slice::from_raw_parts(string.buffer, string.length as usize) };
    let str_value = std::str::from_utf8(slice);

    match str_value {
        Ok(value) => {
            println!("");
            println!("Message: {}", value);
            println!("");
        }
        Err(_) => {
            println!("Invalid UTF-8 sequence");
        }
    }
}




// #[no_mangle]
// pub fn print_int(num: i32) {
//     println!("{:#?}", num);
// }

#[used]
static EXTERNAL_FNS1: [fn(NillaString); 1] = [sys_print];

#[used]
static EXTERNAL_FNS2: [fn(bytes: *const u8, initial_length: i32) -> *mut NillaString; 1] =
    [NillaString::sys_allocate_string];


#[no_mangle]
pub fn print_int(int: i32) {
    println!("print_int: {:#?}", int);
}

#[used]
static EXTERNAL_FNS3: [fn(i32); 1] = [print_int];

// #[derive(Debug)]
// pub enum ReturnValue<'a> {
//     IntValue(IntValue<'a>),
//     ArrayPtrValue(PointerValue<'a>),
//     StructPtrValue(PointerValue<'a>),
//     StructValue(StructValue<'a>),
//     VoidValue,
// }

// #[derive(Debug, Clone)]
// pub struct NillaLLVMTypes<'a> {
//     int_type: LLVMType<'a>,
//     array_ptr_type: LLVMType<'a>,
//     struct_ptr_type: LLVMType<'a>,
//     raw_array_ptr_type: StructType<'a>,
//     raw_struct_ptr_type: StructType<'a>,
// }

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct Compiler<'a, 'ctx> {
    pub parser_result: &'a ParserResult,
    pub context: &'ctx Context,
    pub mlir_module: &'a Module<'ctx>,
    // pub builder: &'a Builder<'ctx>,
    // pub llvm_module: &'a Module<'ctx>,
    // pub llvm_types: &'a NillaLLVMTypes<'ctx>,

    pub parser_index: &'a ParserResultIndex,
    pub class_ids: BTreeMap<String, u64>,
    // pub local_variables: HashMap<String, LocalVarRef<'ctx>>,
    // pub fn_value_opt: Option<FunctionValue<'ctx>>,
}

// #[derive(Debug)]
// pub struct LocalVarRef<'a> {
//     alloca: LLVMValue<'a>,
//     return_type: LLVMType<'a>,
// }

use melior::dialect::{index, llvm};
use melior::ir::attribute::{IntegerAttribute, FlatSymbolRefAttribute};
use melior::ir::operation::OperationResult;
use melior::ir::r#type::IntegerType;
use melior::pass::PassManager;
use melior::{ExecutionEngine, pass};
use melior::{
    Context,
    dialect::{arith, DialectRegistry, func, func::call, llvm::r#type},
    ir::{*, attribute::{StringAttribute, TypeAttribute}, r#type::FunctionType},
    utility::register_all_dialects,
    utility::register_all_llvm_translations,
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(parser_result: &ParserResult, parser: &Parser) {
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);

        let context = Context::new();
        context.append_dialect_registry(&registry);
        context.load_all_available_dialects();
        register_all_llvm_translations(&context);

        // let registry = DialectRegistry::new();
        // let context = Context::new();

        // register_all_dialects(&registry);
        // register_all_llvm_translations(&context);

        // context.load_all_available_dialects();
        // context.append_dialect_registry(&registry);

        // temp for development
        context.attach_diagnostic_handler(|diagnostic| {
            eprintln!("{}", diagnostic);
            true
        });


        let location = Location::unknown(&context);
        let mut mlir_module = Module::new(location);

        let mut compiler = Compiler {
            parser_result: &parser_result,
            context: &context,
            mlir_module: &mlir_module,
            // builder: &builder,
            // llvm_module: &module,
            // fn_value_opt: None,
            // local_variables: HashMap::new(),
            // llvm_types: &llvm_types,
            class_ids: BTreeMap::new(),
            parser_index: &parser.index,
        };

        compiler.compile_ast();


        // let location = Location::unknown(&context);
        // let mut module = Module::new(location);

        // let index_type = Type::index(&context);

        // module.body().append_operation(func::func(
        //     &context,
        //     StringAttribute::new(&context, "add"),
        //     // TypeAttribute::new(FunctionType::new(&context, &[index_type, index_type], &[index_type]).into()),
        //     TypeAttribute::new(FunctionType::new(&context, &[index_type, index_type], &[index_type]).into()),
        //     {
        //         let block = Block::new(&[(index_type, location), (index_type, location)]);
        //         // let block = Block::new(&[(index_type, location)]);

        //         let sum = block.append_operation(arith::addi(
        //             block.argument(0).unwrap().into(),
        //             block.argument(1).unwrap().into(),
        //             location
        //         ));

        //         block.append_operation(func::r#return( &[sum.result(0).unwrap().into()], location));
        //         // block.append_operation(func::r#return( &[block.argument(0).unwrap().into()], location));

        //         // let operation = block.append_operation(
        //         //     operation::OperationBuilder::new("func.return", Location::unknown(&context))
        //         //         .build()
        //         //         .unwrap(),
        //         // );

        //         let region = Region::new();
        //         region.append_block(block);
        //         region
        //     },
        //     &[(
        //         // Identifier::new(&context, "linkage"),
        //         // melior::dialect::llvm::attributes::linkage(&context, melior::dialect::llvm::attributes::Linkage::External),
        //         Identifier::new(&context, "llvm.emit_c_interface"),
        //         Attribute::parse(&context, &format!("unit")).unwrap()
        //         // melior::dialect::llvm::attributes::linkage(&context, melior::dialect::llvm::attributes::Linkage::External),
        //     )],
        //     location,
        // ));

        // println!("PRE:");
        // println!("{}", mlir_module.body().to_string());

        assert!(mlir_module.as_operation().verify());

        println!("PRE:");
        println!("{}", mlir_module.body().to_string());

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
            engine.invoke_packed("main", &mut [
                &mut status_code as *mut i32 as *mut ()
            ]).unwrap();
        }







        // Target::initialize_all(&InitializationConfig::default());

        // let target_triple = TargetMachine::get_default_triple();
        // let target = Target::from_triple(&target_triple).unwrap();
        // let target_machine = target
        //     .create_target_machine(
        //         &target_triple,
        //         TargetMachine::get_host_cpu_name().to_string().as_str(),
        //         TargetMachine::get_host_cpu_features().to_string().as_str(),
        //         // OptimizationLevel::Default,
        //         OptimizationLevel::None,
        //         // RelocMode::Default,
        //         RelocMode::PIC,
        //         CodeModel::Default,
        //     )
        //     .unwrap();

        // let passes: &[&str] = &[
        //     "instcombine",
        //     "reassociate",
        //     "gvn",
        //     "simplifycfg",
        //     // "basic-aa",
        //     "mem2reg",
        // ];

        // let context = Context::create();
        // let module = context.create_module("nilla");
        // let builder = context.create_builder();

        // // let data_layout = &target_machine.get_target_data().get_data_layout();

        // // module.set_data_layout(data_layout);
        // // module.set_triple(&target_triple);

        // // Create FPM
        // // let fpm = PassManager::create(&module);

        // // fpm.add_instruction_combining_pass();
        // // fpm.add_reassociate_pass();
        // // fpm.add_gvn_pass();
        // // fpm.add_cfg_simplification_pass();
        // // fpm.add_basic_alias_analysis_pass();
        // // fpm.add_promote_memory_to_register_pass();
        // // fpm.add_instruction_combining_pass();
        // // fpm.add_reassociate_pass();

        // // fpm.initialize();

        // // // Add print fn
        // // let print_args = &[struct_ptr_type.into()];
        // // let print_function_type = context.void_type().fn_type(print_args, false);
        // // module.add_function("base_print", print_function_type, None);

        // // let print_num_args = &[context.i32_type().into()];
        // // let print_function_type = context.void_type().fn_type(print_num_args, false);
        // // module.add_function("print_int", print_function_type, None);

        // let raw_array_ptr_type = context.struct_type(
        //     &[
        //         context.i32_type().into(),
        //         context
        //             .i8_type()
        //             .ptr_type(AddressSpace::default())
        //             .into(),
        //         context.i32_type().into(),
        //         context.i32_type().into(),
        //     ],
        //     false,
        // );

        // let raw_struct_ptr_type = context.struct_type(
        //     &[
        //         context.i32_type().into(),
        //     ],
        //     false,
        // );

        // let llvm_types = NillaLLVMTypes {
        //     int_type: LLVMType::IntType(
        //         context.i32_type()
        //     ),
        //     array_ptr_type: LLVMType::StructType(raw_array_ptr_type),
        //     struct_ptr_type: LLVMType::StructType(
        //         context.struct_type(
        //             &[
        //                 context.i32_type().into(),
        //             ],
        //             false,
        //         ),
        //     ),
        //     raw_array_ptr_type,
        //     raw_struct_ptr_type,
        // };

        // // Define the function type
        // let struct_ptr_type = llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default());
        // let bytes_ptr_type = context.i8_type().ptr_type(AddressSpace::default());
        // let length_type = context.i32_type();

        // let function_type =
        //     // struct_ptr_type.fn_type(&[bytes_ptr_type.into(), length_type.into()], false);
        //     llvm_types.raw_array_ptr_type.fn_type(&[bytes_ptr_type.into(), length_type.into()], false);

        // module.add_function("allocate_string", function_type, None);

        // let mut compiler = Compiler {
        //     parser_result: &parser_result,
        //     context: &context,
        //     builder: &builder,
        //     llvm_module: &module,
        //     fn_value_opt: None,
        //     local_variables: HashMap::new(),
        //     llvm_types: &llvm_types,
        //     class_ids: BTreeMap::new(),
        //     parser_index: &parser.index,
        // };

        // compiler.compile_ast();

        // println!("\n{}", module.print_to_string().to_string());
        // println!("###################");

        // module
        //     .run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create())
        //     .unwrap();

        // let ee = module
        //     .create_jit_execution_engine(OptimizationLevel::None)
        //     .unwrap();

        // let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>("main") };
        // let compiled_fn = match maybe_fn {
        //     Ok(f) => f,
        //     Err(err) => {
        //         println!("!> Error during execution: {:?}", err);
        //         std::process::exit(1);
        //     }
        // };

        // unsafe {
        //     println!("=> {}", compiled_fn.call());
        // }
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
        // for node in module.body.iter() {
        //     match &node {
        //         Node::Def(def) => {
        //             self.compile_prototype(&def);
        //         },
        //         Node::Class(class) => {
        //             self.compile_class(class);
        //         }
        //         _ => panic!("Unable to add prototype, only functions are currently supported at the top level")
        //     }
        // }

        // println!("{:#?}", "new1");
        // println!("{:#?}", module);

        for node in module.body.iter() {
            match &node {
                Node::Class(_) => {},
                Node::AssignLocalVar(_) => todo!(),
                Node::Binary(_) => todo!(),
                Node::Call(_) => todo!(),
                Node::Def(def) => { self.compile_fn(def) }
                Node::Int(_) => todo!(),
                Node::InterpolableString(_) => todo!(),
                Node::LocalVar(_) => todo!(),
                Node::Module(_) => todo!(),
                Node::Impl(_) => todo!(),
                Node::Trait(_) => todo!(),
                Node::SelfRef(_) => todo!(),
                Node::Send(_) => todo!(),
                Node::DefE(def_e) => self.compile_external_fn(def_e),
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

    fn compile_fn(&mut self, node: &parser::Def) {
        let name = StringAttribute::new(&self.context, &node.prototype.name);

        let fn_signature = if node.main_fn {
            let i32_type = IntegerType::new(&self.context, 32).into();
            TypeAttribute::new(FunctionType::new(&self.context, &[], &[i32_type]).into())
        } else {
            let void_type = llvm::r#type::void(&self.context);
            TypeAttribute::new(FunctionType::new(&self.context, &[], &[void_type]).into())
        };

        let region = self.compile_fn_body(node).unwrap();

        let mut attributes = vec![
            // (
            //     Identifier::new(&self.context, "sym_visibility"),
            //     StringAttribute::new(&self.context, "private").into(),
            // ),
        ];

        if node.main_fn {
            attributes.push(
                (
                    Identifier::new(&self.context, "llvm.emit_c_interface"),
                    Attribute::parse(&self.context, &format!("unit")).unwrap()
                )
            );
        }

        let location = Location::unknown(&self.context);
        let operation = func::func(&self.context, name, fn_signature, region, &attributes, location);

        self.mlir_module.body().append_operation(operation);
    }

    fn compile_external_fn(&mut self, node: &parser::DefE) {
        let name = StringAttribute::new(&self.context, &node.prototype.name);

        let mut inputs = vec![];

        for arg in &node.prototype.args {
            match arg.return_type {
                parser::BaseType::Int => inputs.push(IntegerType::new(&self.context, 32).into()),
                parser::BaseType::StringType => todo!(),
                parser::BaseType::Void => todo!(),
                parser::BaseType::Undef(_) => todo!(),
            }
        }

        let void_type = llvm::r#type::void(&self.context);
        let results = [void_type];

        let fn_signature = TypeAttribute::new(FunctionType::new(&self.context, &inputs, &results).into());
        let region = Region::new();
        let attributes = &[(
            Identifier::new(&self.context, "sym_visibility"),
            StringAttribute::new(&self.context, "private").into(),
        )];
        let location = Location::unknown(&self.context);
        let operation = func::func(&self.context, name, fn_signature, region, attributes, location);

        self.mlir_module.body().append_operation(operation);
    }

    fn compile_fn_body(&mut self, node: &parser::Def) -> Result<Region<'ctx>, &'static str> {
        let arguments = &[];
        let block = Block::new(arguments);
        let region = Region::new();

        if node.body.iter().len() == 0 {
            panic!("Empty body not supported")
        }

        let last_op_index = node.body.len();

        for (i, body_node) in node.body.iter().enumerate() {
            let return_val = self.compile_expr(&block, body_node).unwrap();

            let last_node = i == last_op_index - 1;
            if last_node {
                if node.main_fn {
                    let success_int_value = block.append_operation(arith::constant(
                        &self.context,
                        IntegerAttribute::new(1 as i64, IntegerType::new(&self.context, 32).into()).into(),
                        Location::unknown(&self.context),
                    )).result(0).unwrap().into();

                    block.append_operation(func::r#return( &[success_int_value], Location::unknown(&self.context)));
                } else {
                    block.append_operation(func::r#return( &[return_val], Location::unknown(&self.context)));
                }
            }
        }

        region.append_block(block);


        // match &node.prototype.return_type {
        //     Some(rt) => match rt {
        //         BaseType::Int => {
        //             match last_body {
        //                 Some(ret_type) => match ret_type {
        //                     ReturnValue::IntValue(value) => self.builder.build_return(Some(&value)),
        //                     _ => panic!("Expected Int"),
        //                 },
        //                 None => self.builder.build_return(None),
        //             };
        //         }
        //         BaseType::StringType => {
        //             match last_body {
        //                 Some(ret_type) => match ret_type {
        //                     // ReturnValue::ArrayPtrValue(value) => {
        //                     //     self.builder.build_return(Some(&value))
        //                     // }
        //                     // ReturnValue::StructPtrValue(value) => {
        //                     //     self.builder.build_return(Some(&value))
        //                     // }
        //                     ReturnValue::ArrayPtrValue(value) => {
        //                         // let loaded_value = self.builder.build_load(self.llvm_types.array_ptr_type.clone(), value, "loaded_value");
        //                         // let loaded_value = self.builder.build_load(self.llvm_types.array_ptr_type.clone(), value, "loaded_value");
        //                         let loaded_value = self.builder.build_load(value.get_type(), value, "loaded_value");
        //                         self.builder.build_return(Some(&loaded_value.unwrap()))
        //                     }
        //                     ReturnValue::StructPtrValue(value) => {
        //                         let loaded_value = self.builder.build_load(value.get_type(), value, "loaded_value");
        //                         self.builder.build_return(Some(&loaded_value.unwrap()))
        //                     }
        //                     ReturnValue::IntValue(_) => todo!(),
        //                     ReturnValue::StructValue(value) => {
        //                         self.builder.build_return(Some(&value))
        //                     },
        //                     ReturnValue::VoidValue => todo!(),

        //                     // _ => panic!("Expected String"),
        //                 },
        //                 None => self.builder.build_return(None),
        //             };
        //         }
        //         BaseType::Void => {
        //             self.builder.build_return(None);
        //         }
        //         BaseType::Undef(name) => todo!(),
        //     },
        //     None => {
        //         self.builder.build_return(None);
        //     }
        // }

        Ok(region)
    }

    fn compile_expr(&self, block: &'a Block<'ctx>, expr: &Node) -> Result<Value<'ctx, '_>, &'static str> {
        match *&expr {
            Node::AssignLocalVar(asgn_lvar) => { self.compile_assign_local_var(block, asgn_lvar) },
            Node::LocalVar(lvar) => self.compile_local_var(block, lvar),
            Node::Binary(binary) => self.compile_binary(block, binary),
            Node::InterpolableString(string) => self.compile_interpolable_string(block, string),
            Node::Int(nb) => self.compile_int(block, nb),
            Node::Call(call) => self.compile_call(block, call),
            Node::SelfRef(lvar) => self.compile_self_ref(block, lvar),
            Node::Send(node) => self.compile_send(block, node),
            Node::Module(_) => panic!("Syntax error"),
            Node::Def(_) => panic!("Syntax error"),
            Node::DefE(_) => panic!("Syntax error"),
            Node::Impl(_) => panic!("Syntax error"),
            Node::Class(_) => panic!("Syntax error"),
            Node::Trait(_) => panic!("Syntax error"),
        }
    }

    fn compile_send(&self, block: &'a Block<'ctx>, node: &parser::Send) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    fn compile_self_ref(&self, block: &'a Block<'ctx>, lvar: &parser::SelfRef) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    fn compile_call(&self, block: &'a Block<'ctx>, call: &parser::Call) -> Result<Value<'ctx, '_>, &'static str> {
        let void_type = llvm::r#type::void(&self.context);
        let location = Location::unknown(&self.context);

        let i32_type = IntegerType::new(&self.context, 32).into();
        let function_type = FunctionType::new(&self.context, &[i32_type], &[void_type]);

        let function = block.append_operation(func::constant(
            &self.context,
            FlatSymbolRefAttribute::new(&self.context, call.fn_name.as_str()),
            function_type,
            location,
        ));

        // let function = FlatSymbolRefAttribute::new(&self.context, call.fn_name.as_str());
        // let arguments = &[block.argument(0).unwrap().into()];
        let mut compiled_args = vec![];

        for arg in &call.args {
            let value = self.compile_expr(block, &arg).unwrap();
            compiled_args.push(value);
        }

        // let result_types = &[];
        let value = block
            .append_operation(func::call_indirect(
                // &self.context,
                function.result(0).unwrap().into(), // stopped here, store/lookup function
                &compiled_args,
                // result_types,
                &function_type
                    .result(0)
                    .into_iter()
                    .collect::<Vec<_>>(),
                location,
            ))
            // .result(0);
            // .map(Into::into)
            // .ok();
            .result(0)
            .unwrap()
            .into();

        Ok(value)
        // Err("Wut")


        // let location = Location::unknown(&self.context);


        //         // let sum = block.append_operation(arith::addi(
        //         //     block.argument(0).unwrap().into(),
        //         //     block.argument(1).unwrap().into(),
        //         //     location
        //         // ));

        //         let one = block.append_operation(arith::constant(
        //             &self.context,
        //             IntegerAttribute::new(1, Type::index(&self.context)).into(),
        //             location,
        //         ));

        //         block.append_operation(func::r#return( &[], location));
        //         // block.append_operation(func::r#return( &[block.argument(0).unwrap().into()], location));

        //         // let operation = block.append_operation(
        //         //     operation::OperationBuilder::new("func.return", Location::unknown(&context))
        //         //         .build()
        //         //         .unwrap(),
        //         // );

        // Ok(block)





        // Node::Call(call) => match self.get_function(call.fn_name.as_str()) {
        //     Some(fun) => {
        //         let mut compiled_args = Vec::with_capacity(call.args.len());



        //         // let mut arg_return_types = vec![];
        //         // arg_return_types.reserve(call.args.len());

        //         for arg in &call.args {
        //             println!("{:#?}", "AAAAAAAAAAAARG");
        //             println!("{:#?}", arg);

        //             println!("{:#?}", call);

        //             // match arg {
        //             //     Node::Send(node) => {
        //             //         match node.return_type.unwrap() {
        //             //             BaseType::Int => todo!(),
        //             //             BaseType::StringType => {
        //             //                 arg_return_types.push(
        //             //                     self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
        //             //                 );
        //             //             },
        //             //             BaseType::Void => todo!(),
        //             //             BaseType::Undef(undef) => {
        //             //                 println!("NAMENAME: {:#?}", undef);

        //             //                 arg_return_types.push(
        //             //                     self.llvm_types.raw.ptr_type(AddressSpace::default())
        //             //                 );
        //             //             },
        //             //         }
        //             //     },
        //             //     _ => BaseType::Void,
        //             // };

        //             compiled_args.push(self.compile_expr(&arg)?);
        //         }

        //         let mut i = 0;

        //         let argsv: Vec<BasicMetadataValueEnum> = compiled_args
        //             .iter()
        //             .map(|val| match *val {
        //                 Value::IntValue(float_value) => float_value.into(),
        //                 // Value::ArrayPtrValue(string_ptr) => string_ptr.into(),
        //                 Value::ArrayPtrValue(string_ptr) => {

        //                     // stopped here, maybe an index to see if we need to cast
        //                     // call.args[]

        //                     println!("{:#?}", "INDEX:");
        //                     println!("{:#?}", self.parser_index.fn_index);

        //                     let fn_arg_types = self.parser_index.fn_index.get(fun.get_name().to_str().unwrap()).unwrap();

        //                     let struct_type = match fn_arg_types[i] {
        //                         BaseType::StringType => {
        //                             // self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
        //                             self.llvm_types.raw_array_ptr_type
        //                         },
        //                         BaseType::Undef(_) => {
        //                             // self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default())
        //                             self.llvm_types.raw_struct_ptr_type
        //                         },
        //                         _ => panic!("shouldnt reach here")
        //                     };

        //                     i += 1;

        //                     // let struct_type = self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default());
        //                     self.builder.build_bitcast(string_ptr, struct_type, "bitcasted_val").unwrap().into()

        //                     // todo: pass other args

        //                     // self.builder.build_call(class_fn, &[bitcasted_val.into()], "result")
        //                     // self.builder.build_call(class_fn, &arg_return_types, "result")
        //                 },
        //                 Value::StructPtrValue(struct_ptr) => {
        //                     let fn_arg_types = self.parser_index.fn_index.get(fun.get_name().to_str().unwrap()).unwrap();

        //                     let struct_type = match fn_arg_types[i] {
        //                         BaseType::StringType => {
        //                             self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
        //                         },
        //                         BaseType::Undef(_) => {
        //                             self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default())
        //                         },
        //                         _ => panic!("shouldnt reach here")
        //                     };

        //                     i += 1;

        //                     // let struct_type = self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default());
        //                     self.builder.build_bitcast(struct_ptr.as_basic_value_enum(), struct_type, "bitcasted_val").unwrap().into()

        //                     // todo: pass other args

        //                     // self.builder.build_call(class_fn, &[bitcasted_val.into()], "result")
        //                     // self.builder.build_call(class_fn, &arg_return_types, "result")

        //                     // println!("{}", self.llvm_module.print_to_string().to_string());
        //                     // println!("{:#?}", node);
        //                     // todo!()},
        //                 },
        //                 Value::StructValue(struct_value) => {
        //                     struct_value.into()
        //                 },
        //                 Value::VoidValue => todo!(),

        //                 // _ => todo!(),
        //             })
        //             .collect();

        //         match self
        //             .builder
        //             .build_call(fun, argsv.as_slice(), "tmp")
        //             .unwrap()
        //             .try_as_basic_value()
        //             .left()
        //         {
        //             Some(value) => match value {
        //                 inkwell::values::BasicValueEnum::PointerValue(value) => {
        //                     Ok(Value::ArrayPtrValue(value))
        //                 }
        //                 inkwell::values::BasicValueEnum::IntValue(value) => {
        //                     Ok(Value::IntValue(value))
        //                 }
        //                 _ => todo!(),
        //             },
        //             None => Ok(Value::VoidValue),
        //         }
        //     }
        //     None => Err("Unknown function."),
        // }
    }

    fn compile_int(&self, block: &'a Block<'ctx>, nb: &parser::Int) -> Result<Value<'ctx, '_>, &'static str> {
        let value = block.append_operation(arith::constant(
            &self.context,
            IntegerAttribute::new(nb.value as i64, IntegerType::new(&self.context, 32).into()).into(),
            Location::unknown(&self.context),
        )).result(0).unwrap().into();

        Ok(value)
    }

    fn compile_interpolable_string(&self, block: &'a Block<'ctx>, string: &parser::InterpolableString) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    fn compile_binary(&self, block: &'a Block<'ctx>, binary: &parser::Binary) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    fn compile_local_var(&self, block: &'a Block<'ctx>, lvar: &parser::LocalVar) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    fn compile_assign_local_var(&self, block: &'a Block<'ctx>, asgn_lvar: &parser::AssignLocalVar) -> Result<Value<'ctx, '_>, &'static str> {
        todo!()
    }

    // fn compile_fn(&mut self, node: &parser::Def) -> Result<FunctionValue<'ctx>, &'static str> {
    //     let proto = &node.prototype;

    //     let fn_name = if !node.class_name.is_empty() {
    //         if !node.impl_name.is_empty() {
    //             format!("{}:{}:{}", &node.class_name, &node.impl_name, &proto.name)
    //         } else {
    //             format!("{}:{}", &node.class_name, &proto.name)
    //         }
    //     } else if !node.trait_name.is_empty() {
    //         format!("{}:{}", &node.trait_name, &proto.name)
    //     } else {
    //         proto.name.clone()
    //     };

    //     let function = self.get_function(&fn_name).unwrap();

    //     if !node.trait_name.is_empty() {
    //         if node.body.is_empty() {
    //             self.fn_value_opt = Some(function);

    //             // let mut arg_return_types = vec![];

    //             // for (i, arg) in function.get_param_iter().enumerate() {
    //             //     let return_type = match arg {
    //             //         inkwell::values::BasicValueEnum::IntValue(_) => {
    //             //             arg.into_int_value().as_basic_value_enum().into()
    //             //         },
    //             //         inkwell::values::BasicValueEnum::PointerValue(_) => {
    //             //             arg.into_pointer_value().as_basic_value_enum().into()
    //             //         },
    //             //         inkwell::values::BasicValueEnum::ArrayValue(_) => todo!(),
    //             //         inkwell::values::BasicValueEnum::FloatValue(_) => todo!(),
    //             //         inkwell::values::BasicValueEnum::StructValue(_) => todo!(),
    //             //         inkwell::values::BasicValueEnum::VectorValue(_) => todo!(),
    //             //     };

    //             //     // let arg_data = &proto.args[i];
    //             //     // let return_type = match &arg_data.return_type {
    //             //     //     BaseType::StringType => arg.into_pointer_value().as_basic_value_enum().into(),
    //             //     //     BaseType::Int => arg.into_int_value().as_basic_value_enum().into(),
    //             //     //     BaseType::Undef(name) => arg.into_pointer_value().as_basic_value_enum().into(),
    //             //     //     BaseType::Void => todo!(),
    //             //     // };

    //             //     arg_return_types.push(return_type);
    //             // }

    //             let entry = self.context.append_basic_block(function, "entry");
    //             self.builder.position_at_end(entry);

    //             println!("{:#?}", function);

    //             // o.o
    //             let self_param = function.get_first_param().unwrap();

    //             let i32_val = match self_param {
    //                 inkwell::values::BasicValueEnum::PointerValue(value) => {
    //                     let i32_ptr = self.builder.build_struct_gep(value.get_type(), value, 0, "i32_ptr");
    //                     // self.builder.build_load(self.llvm_types.int_type.into(), i32_ptr.unwrap(), "i32_val").into_int_value()
    //                     self.builder.build_load(value.get_type(), i32_ptr.unwrap(), "i32_val")
    //                 }
    //                 inkwell::values::BasicValueEnum::IntValue(value) => {
    //                     Ok(self.context.i32_type().const_int(0, false).as_basic_value_enum())
    //                 }
    //                 _ => todo!(),

    //                 // inkwell::values::BasicValueEnum::ArrayValue(_) => todo!(),
    //                 // inkwell::values::BasicValueEnum::IntValue(_) => todo!(),
    //                 // inkwell::values::BasicValueEnum::FloatValue(_) => todo!(),
    //                 // inkwell::values::BasicValueEnum::PointerValue(_) => todo!(),
    //                 // inkwell::values::BasicValueEnum::StructValue(_) => todo!(),
    //                 // inkwell::values::BasicValueEnum::VectorValue(_) => todo!(),
    //             };

    //             println!("trait_name: {:#?}", node.trait_name);
    //             println!("{:#?}", self.parser_index.trait_index);

    //             // let class_nodes = self.parser_result.index.trait_index
    //             let class_nodes = self.parser_index.trait_index
    //                 .get(&node.trait_name)
    //                 .unwrap();

    //             let mut cases = vec![];

    //             for class_node in class_nodes {
    //                 let class_fn_name = format!("{}:{}:{}", &class_node.name, &node.trait_name, &proto.name);
    //                 let class_fn = self.get_function(&class_fn_name).unwrap();

    //                 let block = self.context.append_basic_block(function, &class_node.name);
    //                 self.builder.position_at_end(block);

    //                 let self_param = function.get_first_param().unwrap();
    //                 let result = match self_param {
    //                     inkwell::values::BasicValueEnum::PointerValue(value) => {
    //                         let struct_type = self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default());
    //                         let bitcasted_arg0 = self.builder.build_bitcast(value, struct_type, "bitcasted_self");

    //                         // todo: pass other args

    //                         self.builder.build_call(class_fn, &[bitcasted_arg0.unwrap().into()], "result")
    //                         // self.builder.build_call(class_fn, &arg_return_types, "result")
    //                     }
    //                     // inkwell::values::BasicValueEnum::IntValue(value) => {
    //                     //     self.llvm_types.int_type;
    //                     // }
    //                     _ => todo!(),
    //                 };

    //                 match &node.prototype.return_type {
    //                     Some(rt) => match rt {
    //                         BaseType::Int => {
    //                             match result.unwrap().try_as_basic_value().left() {
    //                                 Some(value) => self.builder.build_return(Some(&value)),
    //                                 None => todo!(),
    //                             };
    //                         }
    //                         BaseType::StringType => {
    //                             match result.unwrap().try_as_basic_value().left() {
    //                                 Some(value) => self.builder.build_return(Some(&value)),
    //                                 None => todo!(),
    //                             };
    //                         }
    //                         BaseType::Void => {
    //                             self.builder.build_return(None);
    //                         }
    //                         BaseType::Undef(name) => todo!(),
    //                     },
    //                     None => {
    //                         self.builder.build_return(None);
    //                     }
    //                 };

    //                 let class_id = self.class_ids.get(&class_node.name).unwrap();
    //                 let int_value = self.context.i32_type().const_int(*class_id, false);

    //                 cases.push((int_value, block))
    //             }

    //             self.builder.position_at_end(entry);

    //             let default_block = self.context.append_basic_block(function, "default");
    //             let switch_inst = self.builder.build_switch(
    //                 i32_val.unwrap().into_int_value(),
    //                 default_block,
    //                 &cases.into_boxed_slice()
    //             );

    //             // # find classes that implement  ToString
    //             // # translate to calls in a case statement
    //         } else {
    //             // compile body for to support default functions
    //         }
    //     }

    //     // todo: trait things
    //     // got external function, returning only compiled prototype
    //     if node.body.is_empty() {
    //         return Ok(function);
    //     }

    //     let entry = self.context.append_basic_block(function, "entry");

    //     self.builder.position_at_end(entry);

    //     // update fn field
    //     self.fn_value_opt = Some(function);

    //     // build variables map
    //     self.local_variables.reserve(proto.args.len());

    //     // todo: account for self param: done?
    //     for (i, arg) in function.get_param_iter().enumerate() {
    //         let self_arg;
    //         let arg_data;

    //         arg_data = if i == 0 && !node.class_name.is_empty() {
    //             let type_name = if !node.impl_name.is_empty() {
    //                 node.impl_name.clone()
    //             } else {
    //                 node.class_name.clone()
    //             };

    //             // todo: temp hardcode to string
    //             let return_type = if node.class_name == "Str" {
    //                 BaseType::StringType
    //             } else {
    //                 BaseType::Undef(node.class_name.clone())
    //             };

    //             self_arg = Arg {
    //                 name: "self".to_string(),
    //                 // return_type: BaseType::Undef(type_name),
    //                 return_type,
    //             };

    //             &self_arg
    //         } else {
    //             &proto.args[i]
    //         };

    //         match &arg_data.return_type {
    //             BaseType::StringType => {
    //                 let return_type = LLVMType::StructType(self.llvm_types.raw_array_ptr_type.clone());
    //                 // let alloca = self.create_entry_block_alloca(&arg_data.name, &return_type);
    //                 // let loaded_val = self
    //                 //     .builder
    //                 //     .build_load(arg.into_pointer_value(), &arg_data.name);

    //                 // self.builder.build_store(alloca, loaded_val);

    //                 // let return_type = LLVMType::StructType(()) self.llvm_types.raw_array_ptr_type.clone();
    //                 // let alloca = self.create_entry_block_alloca(&arg_data.name, &return_type);
    //                 // let loaded_val = self.builder.build_load(arg.into_pointer_value(), &arg_data.name);

    //                 // self.builder.build_store(alloca, loaded_val);
    //                 // self.builder.build_store(alloca, arg.into_pointer_value());

    //                 // (alloca, ret_type)


    //                 self.local_variables.insert(
    //                     arg_data.name.clone(),
    //                     LocalVarRef {
    //                         alloca: LLVMValue::StructValue(arg.into_struct_value()),
    //                         // alloca,
    //                         return_type,
    //                     },
    //                 );
    //             }
    //             BaseType::Int => {
    //                 let return_type = self.llvm_types.int_type.clone();
    //                 // let alloca = self.create_entry_block_alloca(&arg_data.name, &return_type);
    //                 // let loaded_val = self.builder.build_load(arg.into_pointer_value(), &arg_data.name);
    //                 // let loaded_val = arg.into_int_value();

    //                 // self.builder.build_store(alloca, loaded_val);

    //                 todo!("int vs pointer value");

    //                 // self.local_variables.insert(
    //                 //     arg_data.name.clone(),
    //                 //     LocalVarRef {
    //                 //         alloca: arg.into_pointer_value(),
    //                 //         return_type,
    //                 //     },
    //                 // );
    //             }
    //             BaseType::Undef(name) => {
    //                 let return_type = LLVMType::StructType(self.llvm_types.raw_array_ptr_type.clone());
    //                 // let return_type = self.llvm_types.struct_ptr_type.clone();
    //                 // let alloca = self.create_entry_block_alloca(&arg_data.name, &return_type);
    //                 // let loaded_val = self
    //                 //     .builder
    //                 //     .build_load(arg.into_pointer_value(), &arg_data.name);

    //                 // self.builder.build_store(alloca, loaded_val);
    //                 // self.builder.build_store(alloca, arg.into_struct_value());

    //                 self.local_variables.insert(
    //                     arg_data.name.clone(),
    //                     LocalVarRef {
    //                         alloca: LLVMValue::StructValue(arg.into_struct_value()),
    //                         // alloca,
    //                         return_type,
    //                     },
    //                 );
    //             },
    //             BaseType::Void => todo!(),

    //         };

    //         // let alloca = self.create_entry_block_alloca(&arg_data.name, &return_type);
    //         // let loaded_val = self.builder.build_load(arg.into_pointer_value(), &arg_data.name);

    //         // self.builder.build_store(alloca, loaded_val);

    //         // self.local_variables.insert(
    //         //     arg_data.name.clone(),
    //         //     LocalVarRef { alloca, return_type },
    //         // );
    //     }

    //     // temperary solution until `ret` keyword is implemented
    //     let mut last_body = None;

    //     // compile body
    //     for node in node.body.iter() {
    //         last_body = Some(self.compile_expr(node)?);
    //     }

    //     match &node.prototype.return_type {
    //         Some(rt) => match rt {
    //             BaseType::Int => {
    //                 match last_body {
    //                     Some(ret_type) => match ret_type {
    //                         ReturnValue::IntValue(value) => self.builder.build_return(Some(&value)),
    //                         _ => panic!("Expected Int"),
    //                     },
    //                     None => self.builder.build_return(None),
    //                 };
    //             }
    //             BaseType::StringType => {
    //                 match last_body {
    //                     Some(ret_type) => match ret_type {
    //                         // ReturnValue::ArrayPtrValue(value) => {
    //                         //     self.builder.build_return(Some(&value))
    //                         // }
    //                         // ReturnValue::StructPtrValue(value) => {
    //                         //     self.builder.build_return(Some(&value))
    //                         // }
    //                         ReturnValue::ArrayPtrValue(value) => {
    //                             // let loaded_value = self.builder.build_load(self.llvm_types.array_ptr_type.clone(), value, "loaded_value");
    //                             // let loaded_value = self.builder.build_load(self.llvm_types.array_ptr_type.clone(), value, "loaded_value");
    //                             let loaded_value = self.builder.build_load(value.get_type(), value, "loaded_value");
    //                             self.builder.build_return(Some(&loaded_value.unwrap()))
    //                         }
    //                         ReturnValue::StructPtrValue(value) => {
    //                             let loaded_value = self.builder.build_load(value.get_type(), value, "loaded_value");
    //                             self.builder.build_return(Some(&loaded_value.unwrap()))
    //                         }
    //                         ReturnValue::IntValue(_) => todo!(),
    //                         ReturnValue::StructValue(value) => {
    //                             self.builder.build_return(Some(&value))
    //                         },
    //                         ReturnValue::VoidValue => todo!(),

    //                         // _ => panic!("Expected String"),
    //                     },
    //                     None => self.builder.build_return(None),
    //                 };
    //             }
    //             BaseType::Void => {
    //                 self.builder.build_return(None);
    //             }
    //             BaseType::Undef(name) => todo!(),
    //         },
    //         None => {
    //             self.builder.build_return(None);
    //         }
    //     }

    //     if function.verify(true) {
    //         Ok(function)
    //     } else {
    //         println!("---------------");
    //         println!("{:#?}", function);
    //         println!("---------------");

    //         println!("{}", self.llvm_module.print_to_string().to_string());

    //         unsafe {
    //             function.delete();
    //         }

    //         Err("Invalid generated function.")
    //     }
    // }

    // fn compile_prototype(&self, def: &Def) -> Result<FunctionValue<'ctx>, &'static str> {
    //     let proto = &def.prototype;
    //     let mut args_types: Vec<inkwell::types::BasicMetadataTypeEnum> = vec![];

    //     let fn_name = if !def.class_name.is_empty() {
    //         // add self as the first argument for instance methods
    //         // let struct_type = self.llvm_types.raw_struct_ptr_type;

    //         // todo: temp hardcode to strings
    //         let struct_type = if def.class_name == "Str" {
    //             self.llvm_types.raw_array_ptr_type
    //         } else {
    //             self.llvm_types.raw_struct_ptr_type
    //         };

    //         // let ptr = struct_type.ptr_type(AddressSpace::default()).into();
    //         // args_types.push(ptr);
    //         args_types.push(inkwell::types::BasicMetadataTypeEnum::StructType(struct_type));

    //         if !def.impl_name.is_empty() {
    //             format!("{}:{}:{}", &def.class_name, &def.impl_name, &proto.name)
    //         } else {
    //             format!("{}:{}", &def.class_name, &proto.name)
    //         }
    //     } else if !def.trait_name.is_empty() {
    //         // add self as the first argument for instance methods
    //         // let struct_type = self.llvm_types.raw_struct_ptr_type;

    //         // todo: temp hardcode to strings
    //         let struct_type = if def.class_name == "Str" {
    //             self.llvm_types.raw_array_ptr_type
    //         } else {
    //             self.llvm_types.raw_struct_ptr_type
    //         };

    //         // let ptr = struct_type.ptr_type(AddressSpace::default()).into();
    //         // args_types.push(ptr);
    //         args_types.push(inkwell::types::BasicMetadataTypeEnum::StructType(struct_type));

    //         format!("{}:{}", &def.trait_name, &proto.name)
    //     } else {
    //         proto.name.clone()
    //     };

    //     for arg in &proto.args {
    //         match &arg.return_type {
    //             BaseType::StringType => {
    //                 let struct_type = self.llvm_types.raw_array_ptr_type;
    //                 // let ptr = struct_type.ptr_type(AddressSpace::default()).into();
    //                 // let ptr = struct_type.ptr_type(AddressSpace::default()).into();

    //                 // args_types.push(ptr);
    //                 args_types.push(struct_type.into());
    //             }
    //             BaseType::Int => {
    //                 todo!();
    //                 // args_types.push(self.context.i32_type().into());
    //             }
    //             BaseType::Void => todo!(),
    //             BaseType::Undef(_name) => {
    //                 let struct_type = self.llvm_types.raw_struct_ptr_type;
    //                 // let ptr = struct_type.ptr_type(AddressSpace::default()).into();

    //                 args_types.push(struct_type.into());
    //             },
    //         };
    //     }

    //     let args_types = args_types.as_slice();

    //     let fn_type = match &proto.return_type {
    //         Some(ret_type) => match ret_type {
    //             BaseType::StringType => {
    //                 let struct_type = self.llvm_types.raw_array_ptr_type;
    //                 // struct_type.ptr_type(AddressSpace::default()).fn_type(args_types, false)
    //                 struct_type.fn_type(args_types, false)
    //             }
    //             BaseType::Int => {
    //                 self.context.i32_type().fn_type(args_types, false)
    //             }
    //             BaseType::Void => todo!(),
    //             BaseType::Undef(_name) => {
    //                 let struct_type = self.llvm_types.raw_struct_ptr_type;
    //                 // struct_type.ptr_type(AddressSpace::default()).fn_type(args_types, false)
    //                 struct_type.fn_type(args_types, false)
    //             },
    //         },
    //         None => self.context.void_type().fn_type(args_types, false),
    //     };

    //     let fn_val = self
    //         .llvm_module
    //         .add_function(fn_name.as_str(), fn_type, None);

    //     // set arguments names
    //     // for (i, arg) in fn_val.get_param_iter().enumerate() {
    //     //     arg.into_float_value().set_name(proto.args[i].name.as_str());
    //     // }

    //     // finally return built prototype
    //     Ok(fn_val)
    // }

    // /// Gets a defined function given its name.
    // #[inline]
    // fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
    //     self.llvm_module.get_function(name)
    // }

    // /// Returns the `FunctionValue` representing the function being compiled.
    // #[inline]
    // fn fn_value(&self) -> FunctionValue<'ctx> {
    //     self.fn_value_opt.unwrap()
    // }

    // /// Creates a new stack allocation instruction in the entry block of the function.
    // fn create_entry_block_alloca(
    //     &mut self,
    //     arg_name: &String,
    //     return_type: &LLVMType<'ctx>,
    // ) -> PointerValue<'ctx> {
    // // ) -> LLVMValue<'ctx> {
    //     let builder = self.context.create_builder();
    //     let entry = self.fn_value().get_first_basic_block().unwrap();

    //     match entry.get_first_instruction() {
    //         Some(first_instr) => builder.position_before(&first_instr),
    //         None => builder.position_at_end(entry),
    //     }

    //     match *return_type {
    //         LLVMType::StructType(llvm_type) => builder.build_alloca(llvm_type, arg_name.as_str()).unwrap(),
    //         LLVMType::IntType(llvm_type) => builder.build_alloca(llvm_type, arg_name.as_str()).unwrap(),
    //         LLVMType::VoidType(_llvm_type) => {
    //             panic!("void shouldnt be assigned")
    //         }
    //     }
    // }

    // /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    // fn compile_expr(&mut self, expr: &Node) -> Result<ReturnValue<'ctx>, &'static str> {
    //     match *&expr {
    //         Node::Module(module) => Err("module todo"),

    //         Node::Def(def) => Err("def todo"),

    //         Node::AssignLocalVar(asgn_lvar) => {
    //             let lvar_name = &asgn_lvar.name;
    //             let initial_value = self.compile_expr(&asgn_lvar.value)?;
    //             let (alloca, return_type) = match &initial_value {
    //                 ReturnValue::IntValue(fv) => {
    //                     let ret_type = self.llvm_types.int_type.clone();
    //                     let alloca = self.create_entry_block_alloca(lvar_name, &ret_type);

    //                     self.builder.build_store(alloca, *fv);

    //                     (LLVMValue::IntType(*fv), ret_type)
    //                 }
    //                 ReturnValue::ArrayPtrValue(pv) => {
    //                     let ret_type = self.llvm_types.array_ptr_type.clone();
    //                     let alloca = self.create_entry_block_alloca(lvar_name, &ret_type);
    //                     let loaded_val = self.builder.build_load(pv.get_type(), *pv, lvar_name).unwrap();

    //                     self.builder.build_store(alloca, loaded_val);

    //                     (LLVMValue::PointerValue(*pv), ret_type)
    //                 }
    //                 ReturnValue::StructPtrValue(pv) => {
    //                     let ret_type = self.llvm_types.struct_ptr_type.clone();
    //                     let alloca = self.create_entry_block_alloca(lvar_name, &ret_type);

    //                     self.builder.build_store(alloca, *pv);

    //                     (LLVMValue::PointerValue(*pv), ret_type)
    //                 },
    //                 ReturnValue::StructValue(sv) => {
    //                     let ret_type = LLVMType::StructType(self.llvm_types.raw_array_ptr_type.clone());
    //                     // let alloca = self.create_entry_block_alloca(lvar_name, &ret_type);

    //                     // self.builder.build_store(alloca, *sv);

    //                     (LLVMValue::StructValue(*sv), ret_type)
    //                 },
    //                 ReturnValue::VoidValue => todo!(),
    //             };

    //             let value_ref = LocalVarRef {
    //                 alloca,
    //                 return_type,
    //             };
    //             self.local_variables.insert(lvar_name.clone(), value_ref);

    //             Ok(initial_value)
    //         }

    //         Node::LocalVar(lvar) => {
    //             match self.local_variables.get(lvar.name.as_str()) {
    //                 Some(var) => {
    //                     match var.return_type {
    //                         LLVMType::StructType(_) => {
    //                             match &lvar.return_type {
    //                                 Some(bt) => match bt {
    //                                     BaseType::StringType => {
    //                                         match var.alloca {
    //                                             LLVMValue::StructValue(val) => Ok(ReturnValue::StructValue(val.as_basic_value_enum().into_struct_value())),
    //                                             LLVMValue::PointerValue(val) => Ok(ReturnValue::ArrayPtrValue(val.as_basic_value_enum().into_pointer_value())),
    //                                             LLVMValue::IntType(val) => Ok(ReturnValue::IntValue(val.as_basic_value_enum().into_int_value())),
    //                                         }
    //                                     },
    //                                     BaseType::Int => todo!(),
    //                                     BaseType::Undef(name) => {
    //                                         // let ret_type = if name == "Str" {
    //                                         //     ReturnValue::ArrayPtrValue
    //                                         // } else {
    //                                         //     ReturnValue::StructPtrValue
    //                                         // };

    //                                         let ret_type = ReturnValue::StructValue;

    //                                         match var.alloca {
    //                                             LLVMValue::StructValue(val) => Ok(ret_type(val.as_basic_value_enum().into_struct_value())),
    //                                             LLVMValue::PointerValue(val) => Ok(ReturnValue::ArrayPtrValue(val.as_basic_value_enum().into_pointer_value())),
    //                                             LLVMValue::IntType(val) => Ok(ReturnValue::IntValue(val.as_basic_value_enum().into_int_value())),
    //                                         }

    //                                         // Ok(ReturnValue::StructPtrValue(var.alloca.as_basic_value_enum().into_pointer_value()))
    //                                     },
    //                                     BaseType::Void => todo!(),
    //                                 },
    //                                 None => todo!(),
    //                             }
    //                         },
    //                         LLVMType::IntType(_) => {
    //                             todo!();
    //                             // let load_inst =
    //                             //     self.builder.build_load(var.alloca, lvar.name.as_str());
    //                             // Ok(ReturnValue::IntValue(load_inst.into_int_value()))
    //                         }
    //                         LLVMType::VoidType(_) => Ok(ReturnValue::VoidValue),
    //                     }

    //                     // let load_inst = self.builder.build_load(var.alloca, lvar.name.as_str());
    //                 }
    //                 None => Err("Could not find a matching variable."),
    //             }
    //         }

    //         Node::Binary(binary) => match binary.op {
    //             _ => Err("binary operations not supported yet"),
    //         },

    //         Node::InterpolableString(string) => {
    //             let i8_type = self.context.i8_type();
    //             let i8_array_type = i8_type.array_type(20);

    //             let hello_world_str = self.context.const_string(string.value.as_bytes(), false);
    //             let global_str = self.llvm_module.add_global(i8_array_type, None, "0");

    //             global_str.set_initializer(&hello_world_str);

    //             let malloc_string_fn = self.llvm_module.get_function("allocate_string").unwrap();
    //             let i32_type = self.context.i32_type();

    //             // Get a pointer to the first element of the array
    //             let zero = self.context.i32_type().const_int(0, false);
    //             let indices = [zero, zero];

    //             let element_pointer = unsafe {
    //                 self.builder
    //                     .build_gep(i8_array_type, global_str.as_pointer_value(), &indices, "element_ptr")
    //             }.unwrap();

    //             let args = &[
    //                 element_pointer.into(),
    //                 i32_type
    //                     .const_int(string.value.len() as u64, false)
    //                     .into(),
    //             ];

    //             let nilla_str_ptr = self
    //                 .builder
    //                 .build_call(malloc_string_fn, args, "tmp").unwrap()
    //                 .try_as_basic_value()
    //                 .left()
    //                 .unwrap();

    //             // Ok(ReturnValue::ArrayPtrValue(global_str.as_pointer_value()))
    //             Ok(ReturnValue::StructValue(
    //                 nilla_str_ptr.as_basic_value_enum().into_struct_value(),
    //             ))
    //         }

    //         Node::Int(nb) => Ok(ReturnValue::IntValue(
    //             // self.context.f64_type().const_float(nb.value),
    //             self.context.i32_type().const_int(nb.value, false),
    //         )),

    //         // Node::VarIn {
    //         //     ref variables,
    //         //     ref body,
    //         // } => {
    //         //     let mut old_bindings = Vec::new();

    //         //     for &(ref var_name, ref initializer) in variables {
    //         //         let var_name = var_name.as_str();

    //         //         let initial_val = match *initializer {
    //         //             Some(ref init) => self.compile_expr(init)?,
    //         //             None => self.context.f64_type().const_float(0.),
    //         //         };

    //         //         let alloca = self.create_entry_block_alloca(var_name);

    //         //         self.builder.build_store(alloca, initial_val);

    //         //         if let Some(old_binding) = self.variables.remove(var_name) {
    //         //             old_bindings.push(old_binding);
    //         //         }

    //         //         self.variables.insert(var_name.to_string(), alloca);
    //         //     }

    //         //     let body = self.compile_expr(body)?;

    //         //     for binding in old_bindings {
    //         //         self.variables
    //         //             .insert(binding.get_name().to_str().unwrap().to_string(), binding);
    //         //     }

    //         //     Ok(body)
    //         // },
    //         Node::Call(call) => match self.get_function(call.fn_name.as_str()) {
    //             Some(fun) => {
    //                 let mut compiled_args = Vec::with_capacity(call.args.len());



    //                 // let mut arg_return_types = vec![];
    //                 // arg_return_types.reserve(call.args.len());

    //                 for arg in &call.args {
    //                     println!("{:#?}", "AAAAAAAAAAAARG");
    //                     println!("{:#?}", arg);

    //                     println!("{:#?}", call);

    //                     // match arg {
    //                     //     Node::Send(node) => {
    //                     //         match node.return_type.unwrap() {
    //                     //             BaseType::Int => todo!(),
    //                     //             BaseType::StringType => {
    //                     //                 arg_return_types.push(
    //                     //                     self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
    //                     //                 );
    //                     //             },
    //                     //             BaseType::Void => todo!(),
    //                     //             BaseType::Undef(undef) => {
    //                     //                 println!("NAMENAME: {:#?}", undef);

    //                     //                 arg_return_types.push(
    //                     //                     self.llvm_types.raw.ptr_type(AddressSpace::default())
    //                     //                 );
    //                     //             },
    //                     //         }
    //                     //     },
    //                     //     _ => BaseType::Void,
    //                     // };

    //                     compiled_args.push(self.compile_expr(&arg)?);
    //                 }

    //                 let mut i = 0;

    //                 let argsv: Vec<BasicMetadataValueEnum> = compiled_args
    //                     .iter()
    //                     .map(|val| match *val {
    //                         ReturnValue::IntValue(float_value) => float_value.into(),
    //                         // ReturnValue::ArrayPtrValue(string_ptr) => string_ptr.into(),
    //                         ReturnValue::ArrayPtrValue(string_ptr) => {

    //                             // stopped here, maybe an index to see if we need to cast
    //                             // call.args[]

    //                             println!("{:#?}", "INDEX:");
    //                             println!("{:#?}", self.parser_index.fn_index);

    //                             let fn_arg_types = self.parser_index.fn_index.get(fun.get_name().to_str().unwrap()).unwrap();

    //                             let struct_type = match fn_arg_types[i] {
    //                                 BaseType::StringType => {
    //                                     // self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
    //                                     self.llvm_types.raw_array_ptr_type
    //                                 },
    //                                 BaseType::Undef(_) => {
    //                                     // self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default())
    //                                     self.llvm_types.raw_struct_ptr_type
    //                                 },
    //                                 _ => panic!("shouldnt reach here")
    //                             };

    //                             i += 1;

    //                             // let struct_type = self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default());
    //                             self.builder.build_bitcast(string_ptr, struct_type, "bitcasted_val").unwrap().into()

    //                             // todo: pass other args

    //                             // self.builder.build_call(class_fn, &[bitcasted_val.into()], "result")
    //                             // self.builder.build_call(class_fn, &arg_return_types, "result")
    //                         },
    //                         ReturnValue::StructPtrValue(struct_ptr) => {
    //                             let fn_arg_types = self.parser_index.fn_index.get(fun.get_name().to_str().unwrap()).unwrap();

    //                             let struct_type = match fn_arg_types[i] {
    //                                 BaseType::StringType => {
    //                                     self.llvm_types.raw_array_ptr_type.ptr_type(AddressSpace::default())
    //                                 },
    //                                 BaseType::Undef(_) => {
    //                                     self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default())
    //                                 },
    //                                 _ => panic!("shouldnt reach here")
    //                             };

    //                             i += 1;

    //                             // let struct_type = self.llvm_types.raw_struct_ptr_type.ptr_type(AddressSpace::default());
    //                             self.builder.build_bitcast(struct_ptr.as_basic_value_enum(), struct_type, "bitcasted_val").unwrap().into()

    //                             // todo: pass other args

    //                             // self.builder.build_call(class_fn, &[bitcasted_val.into()], "result")
    //                             // self.builder.build_call(class_fn, &arg_return_types, "result")

    //                             // println!("{}", self.llvm_module.print_to_string().to_string());
    //                             // println!("{:#?}", node);
    //                             // todo!()},
    //                         },
    //                         ReturnValue::StructValue(struct_value) => {
    //                             struct_value.into()
    //                         },
    //                         ReturnValue::VoidValue => todo!(),

    //                         // _ => todo!(),
    //                     })
    //                     .collect();

    //                 match self
    //                     .builder
    //                     .build_call(fun, argsv.as_slice(), "tmp")
    //                     .unwrap()
    //                     .try_as_basic_value()
    //                     .left()
    //                 {
    //                     Some(value) => match value {
    //                         inkwell::values::BasicValueEnum::PointerValue(value) => {
    //                             Ok(ReturnValue::ArrayPtrValue(value))
    //                         }
    //                         inkwell::values::BasicValueEnum::IntValue(value) => {
    //                             Ok(ReturnValue::IntValue(value))
    //                         }
    //                         _ => todo!(),
    //                     },
    //                     None => Ok(ReturnValue::VoidValue),
    //                 }
    //             }
    //             None => Err("Unknown function."),
    //         },

    //         Node::Impl(_) => todo!(),
    //         Node::Class(_) => todo!(),
    //         Node::Trait(_) => todo!(),
    //         Node::SelfRef(lvar) => {
    //             match self.local_variables.get("self") {
    //                 Some(var) => {
    //                     match var.return_type {
    //                         LLVMType::StructType(_) => {
    //                             match &lvar.return_type {
    //                                 BaseType::StringType => {
    //                                     match var.alloca {
    //                                         LLVMValue::StructValue(val) => Ok(ReturnValue::StructValue(val.as_basic_value_enum().into_struct_value())),
    //                                         LLVMValue::PointerValue(val) => Ok(ReturnValue::ArrayPtrValue(val.as_basic_value_enum().into_pointer_value())),
    //                                         LLVMValue::IntType(val) => Ok(ReturnValue::IntValue(val.as_basic_value_enum().into_int_value())),
    //                                     }
    //                                 },
    //                                 BaseType::Int => todo!(),
    //                                 // BaseType::Undef(name) => Ok(ReturnValue::StructPtrValue(var.alloca.as_basic_value_enum().into_pointer_value())),
    //                                 // todo: temp hardcode to strings
    //                                 BaseType::Undef(name) => {
    //                                     match var.alloca {
    //                                         LLVMValue::StructValue(val) => Ok(ReturnValue::StructValue(val.as_basic_value_enum().into_struct_value())),
    //                                         LLVMValue::PointerValue(val) => Ok(ReturnValue::ArrayPtrValue(val.as_basic_value_enum().into_pointer_value())),
    //                                         LLVMValue::IntType(val) => Ok(ReturnValue::IntValue(val.as_basic_value_enum().into_int_value())),
    //                                     }
    //                                 },
    //                                 BaseType::Void => todo!(),
    //                             }
    //                         },
    //                         LLVMType::IntType(_) => {
    //                             todo!()
    //                             // let load_inst =
    //                             //     self.builder.build_load(var.alloca, "self");
    //                             // Ok(ReturnValue::IntValue(load_inst.into_int_value()))
    //                         }
    //                         LLVMType::VoidType(_) => Ok(ReturnValue::VoidValue),
    //                     }

    //                     // let load_inst = self.builder.build_load(var.alloca, lvar.name.as_str());
    //                 }
    //                 None => Err("Could not find a matching variable."),
    //             }
    //         }
    //         Node::Send(node) => {
    //             let call_node = match node.message.as_ref() {
    //                 Node::Call(call_node) => {
    //                     // (call_node.fn_name, call_node.args.len())
    //                     call_node
    //                 },
    //                 _ => return Err("Expected send_node message to be a Call"),
    //             };

    //             let scoped_fn_name = match node.receiver.as_ref() {
    //                 Node::LocalVar(local_var) => match &local_var.return_type {
    //                     Some(rt) => {
    //                         match rt {
    //                             BaseType::Int => todo!(),
    //                             BaseType::StringType => todo!(),
    //                             BaseType::Void => todo!(),
    //                             BaseType::Undef(class_name) => format!("{}:{}", class_name, call_node.fn_name),
    //                         }
    //                     },
    //                     None => todo!(),
    //                 },
    //                 _ => return Err("Send only implements LocalVar so far")
    //             };

    //             match self.get_function(scoped_fn_name.as_str()) {
    //                 Some(fun) => {
    //                     let mut compiled_args = Vec::with_capacity(1 + call_node.args.len());

    //                     compiled_args.push(self.compile_expr(&node.receiver));

    //                     for arg in &call_node.args {
    //                         // println!("{:#?}", arg);
    //                         compiled_args.push(self.compile_expr(&arg));
    //                     }

    //                     let argsv: Vec<BasicMetadataValueEnum> = compiled_args
    //                         .iter()
    //                         .map(|val| match val.as_ref().unwrap() {
    //                             ReturnValue::IntValue(float_value) => (*float_value).into(),
    //                             ReturnValue::ArrayPtrValue(string_ptr) => (*string_ptr).into(),
    //                             ReturnValue::StructPtrValue(struct_ptr) => (*struct_ptr).into(),
    //                             ReturnValue::VoidValue => todo!(),
    //                             ReturnValue::StructValue(struct_value) => (*struct_value).into(),
    //                         })
    //                         .collect();

    //                     match self
    //                         .builder
    //                         .build_call(fun, argsv.as_slice(), "tmp")
    //                         .unwrap()
    //                         .try_as_basic_value()
    //                         .left()
    //                     {
    //                         Some(value) => match value {
    //                             inkwell::values::BasicValueEnum::PointerValue(value) => {
    //                                 match &node.return_type {
    //                                     Some(rt) => match rt {
    //                                         BaseType::StringType => Ok(ReturnValue::ArrayPtrValue(value)),
    //                                         BaseType::Undef(_) => Ok(ReturnValue::StructPtrValue(value)),
    //                                         _ => panic!("Only struct types should be here")
    //                                     },
    //                                     None => todo!(),
    //                                 }
    //                             }
    //                             inkwell::values::BasicValueEnum::IntValue(value) => {
    //                                 Ok(ReturnValue::IntValue(value))
    //                             }
    //                             inkwell::values::BasicValueEnum::StructValue(value) => {
    //                                 Ok(ReturnValue::StructValue(value))
    //                             },
    //                             inkwell::values::BasicValueEnum::ArrayValue(_) => todo!(),
    //                             inkwell::values::BasicValueEnum::FloatValue(_) => todo!(),
    //                             inkwell::values::BasicValueEnum::VectorValue(_) => todo!(),
    //                         },
    //                         None => Ok(ReturnValue::VoidValue),
    //                     }
    //                 }
    //                 None => Err("Unknown function."),
    //             }


    //         },
    //         // Node::Conditional {
    //         //     ref cond,
    //         //     ref consequence,
    //         //     ref alternative,
    //         // } => {
    //         //     let parent = self.fn_value();
    //         //     let zero_const = self.context.f64_type().const_float(0.0);

    //         //     // create condition by comparing without 0.0 and returning an int
    //         //     let cond = self.compile_expr(cond)?;
    //         //     let cond = self
    //         //         .builder
    //         //         .build_float_compare(FloatPredicate::ONE, cond, zero_const, "ifcond");

    //         //     // build branch
    //         //     let then_bb = self.context.append_basic_block(parent, "then");
    //         //     let else_bb = self.context.append_basic_block(parent, "else");
    //         //     let cont_bb = self.context.append_basic_block(parent, "ifcont");

    //         //     self.builder.build_conditional_branch(cond, then_bb, else_bb);

    //         //     // build then block
    //         //     self.builder.position_at_end(then_bb);
    //         //     let then_val = self.compile_expr(consequence)?;
    //         //     self.builder.build_unconditional_branch(cont_bb);

    //         //     let then_bb = self.builder.get_insert_block().unwrap();

    //         //     // build else block
    //         //     self.builder.position_at_end(else_bb);
    //         //     let else_val = self.compile_expr(alternative)?;
    //         //     self.builder.build_unconditional_branch(cont_bb);

    //         //     let else_bb = self.builder.get_insert_block().unwrap();

    //         //     // emit merge block
    //         //     self.builder.position_at_end(cont_bb);

    //         //     let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

    //         //     phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

    //         //     Ok(phi.as_basic_value().into_float_value())
    //         // },

    //         // Node::For {
    //         //     ref var_name,
    //         //     ref start,
    //         //     ref end,
    //         //     ref step,
    //         //     ref body,
    //         // } => {
    //         //     let parent = self.fn_value();

    //         //     let start_alloca = self.create_entry_block_alloca(var_name);
    //         //     let start = self.compile_expr(start)?;

    //         //     self.builder.build_store(start_alloca, start);

    //         //     // go from current block to loop block
    //         //     let loop_bb = self.context.append_basic_block(parent, "loop");

    //         //     self.builder.build_unconditional_branch(loop_bb);
    //         //     self.builder.position_at_end(loop_bb);

    //         //     let old_val = self.variables.remove(var_name.as_str());

    //         //     self.variables.insert(var_name.to_owned(), start_alloca);

    //         //     // emit body
    //         //     self.compile_expr(body)?;

    //         //     // emit step
    //         //     let step = match *step {
    //         //         Some(ref step) => self.compile_expr(step)?,
    //         //         None => self.context.f64_type().const_float(1.0),
    //         //     };

    //         //     // compile end condition
    //         //     let end_cond = self.compile_expr(end)?;

    //         //     let curr_var = self.builder.build_load(start_alloca, var_name);
    //         //     let next_var = self
    //         //         .builder
    //         //         .build_float_add(curr_var.into_float_value(), step, "nextvar");

    //         //     self.builder.build_store(start_alloca, next_var);

    //         //     let end_cond = self.builder.build_float_compare(
    //         //         FloatPredicate::ONE,
    //         //         end_cond,
    //         //         self.context.f64_type().const_float(0.0),
    //         //         "loopcond",
    //         //     );
    //         //     let after_bb = self.context.append_basic_block(parent, "afterloop");

    //         //     self.builder.build_conditional_branch(end_cond, loop_bb, after_bb);
    //         //     self.builder.position_at_end(after_bb);

    //         //     self.variables.remove(var_name);

    //         //     if let Some(val) = old_val {
    //         //         self.variables.insert(var_name.to_owned(), val);
    //         //     }

    //         //     Ok(self.context.f64_type().const_float(0.0))
    //         // },
    //     }
    // }
}
