use crate::mi_malloc;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{StructType, VoidType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::parser::{self, *};

#[repr(C)]
pub struct NillaString {
    buffer: *mut u8,
    length: i32,
    max_length: i32,
}

impl NillaString {
    #[no_mangle]
    fn allocate_string(bytes: *const u8, length: i32) -> *mut NillaString {
        let ptr: *mut u8 = unsafe { mi_malloc(length as usize).cast() };

        unsafe {
            core::ptr::copy(bytes, ptr, length as usize);
        }

        let nilla_string = NillaString {
            buffer: ptr,
            length,
            max_length: length,
        };

        let size = core::mem::size_of::<NillaString>();
        let ptr: *mut NillaString = unsafe { mi_malloc(size).cast() };

        if !ptr.is_null() {
            unsafe { ptr.write(nilla_string) };
        }

        ptr
    }
}

#[no_mangle]
pub fn print(nilla_string: *const NillaString) {
    if nilla_string.is_null() {
        println!("Null pointer passed to print function");
        return;
    }

    let string = unsafe { &*nilla_string };

    if string.buffer.is_null() {
        println!("Null buffer pointer passed to print function");
        return;
    }

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

#[used]
static EXTERNAL_FNS1: [fn(*const NillaString); 1] = [print];

#[used]
static EXTERNAL_FNS2: [fn(bytes: *const u8, initial_length: i32) -> *mut NillaString; 1] =
    [NillaString::allocate_string];

#[derive(Debug)]
pub enum ReturnValue<'a> {
    FloatValue(FloatValue<'a>),
    ArrayPtrValue(PointerValue<'a>),
    VoidValue,
}

/// Defines the `Expr` compiler.
#[derive(Debug)]
pub struct Compiler<'a, 'ctx> {
    pub parser_result: &'a ParserResult,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub llvm_module: &'a Module<'ctx>,

    pub local_variables: HashMap<String, LocalVarRef<'ctx>>,
    pub fn_value_opt: Option<FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub struct LocalVarRef<'a> {
    alloca: PointerValue<'a>,
    return_type: StructType<'a>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(parser_result: ParserResult) {
        Target::initialize_all(&InitializationConfig::default());

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                TargetMachine::get_host_cpu_name().to_string().as_str(),
                TargetMachine::get_host_cpu_features().to_string().as_str(),
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        let context = Context::create();
        let module = context.create_module("nilla");
        let builder = context.create_builder();

        let data_layout = &target_machine.get_target_data().get_data_layout();

        module.set_data_layout(data_layout);
        module.set_triple(&target_triple);

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        // NillaString struct type
        let string_struct_type = context.struct_type(
            &[
                context.i8_type().ptr_type(AddressSpace::default()).into(),
                context.i32_type().into(),
                context.i32_type().into(),
            ],
            false,
        );

        // Define the function type
        let struct_ptr_type = string_struct_type.ptr_type(AddressSpace::default());
        let bytes_ptr_type = context.i8_type().ptr_type(AddressSpace::default());
        let length_type = context.i32_type();

        let function_type =
            struct_ptr_type.fn_type(&[bytes_ptr_type.into(), length_type.into()], false);

        module.add_function("allocate_string", function_type, None);

        // Add print fn
        let print_args = &[struct_ptr_type.into()];
        let print_function_type = context.void_type().fn_type(print_args, false);
        module.add_function("print", print_function_type, None);

        let mut compiler = Compiler {
            parser_result: &parser_result,
            context: &context,
            builder: &builder,
            fpm: &fpm,
            llvm_module: &module,
            fn_value_opt: None,
            local_variables: HashMap::new(),
        };

        compiler.compile_ast();

        println!("\n{}", module.print_to_string().to_string());
        println!("###################");

        let ee = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>("main") };
        let compiled_fn = match maybe_fn {
            Ok(f) => f,
            Err(err) => {
                println!("!> Error during execution: {:?}", err);
                std::process::exit(1);
            }
        };

        unsafe {
            println!("=> {}", compiled_fn.call());
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
                Node::Def(def) => {
                    self.compile_prototype(&def.prototype);
                    // self.compile_fn(def).unwrap();
                },
                _ => panic!("Unable to add prototype, only functions are currently supported at the top level")
            }
        }

        println!("{:#?}", module);

        for node in module.body.iter() {
            match &node {
                Node::AssignLocalVar(_) => todo!(),
                Node::Binary(_) => todo!(),
                Node::Call(_) => todo!(),
                Node::Def(def) => {
                    self.compile_fn(def).unwrap();
                }
                Node::Int(_) => todo!(),
                Node::InterpolableString(_) => todo!(),
                Node::LocalVar(_) => todo!(),
                Node::Module(_) => todo!(),
            }
        }
    }

    fn compile_fn(&mut self, node: &parser::Def) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &node.prototype;

        let function = self.get_function(&node.prototype.name).unwrap();

        // got external function, returning only compiled prototype
        if node.body.is_empty() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.local_variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_data = &proto.args[i];
            let return_type = match arg_data.return_type {
                BaseType::StringType => {
                    self.context.struct_type(
                        &[
                            self.context
                                .i8_type()
                                .ptr_type(AddressSpace::default())
                                .into(),
                            self.context.i32_type().into(),
                            self.context.i32_type().into(),
                        ],
                        false,
                    )
                },
                _ => { todo!("aaaaaaaahhhhh") }
            };

            let alloca = self.create_entry_block_alloca(&arg_data.name, return_type);
            let loaded_val = self.builder.build_load(arg.into_pointer_value(), &arg_data.name);

            self.builder.build_store(alloca, loaded_val);
            self.local_variables.insert(
                arg_data.name.clone(),
                LocalVarRef { alloca, return_type },
            );
        }

        // temperary solution until `ret` keyword is implemented
        let mut last_body = None;

        // compile body
        for node in node.body.iter() {
            last_body = Some(self.compile_expr(node)?);
        }

        match last_body {
            Some(ret_type) => {
                match ret_type {
                    ReturnValue::FloatValue(value) => self.builder.build_return(Some(&value)),
                    ReturnValue::ArrayPtrValue(value) => self.builder.build_return(Some(&value)),
                    ReturnValue::VoidValue => self.builder.build_return(None),
                }
            },
            None => self.builder.build_return(None)
        };

        // println!("{}", self.llvm_module.print_to_string().to_string());

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

    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut args_types = vec![];

        for arg in &proto.args {
            match &arg.return_type {
                BaseType::StringType => {
                    let struct_type = self.context.struct_type(
                        &[
                            self.context
                                .i8_type()
                                .ptr_type(AddressSpace::default())
                                .into(),
                            self.context.i32_type().into(),
                            self.context.i32_type().into(),
                        ],
                        false,
                    );
                    let ptr = struct_type.ptr_type(AddressSpace::default()).into();

                    args_types.push(ptr);
                }
                _ => todo!(),
            };
        }

        let args_types = args_types.as_slice();

        let fn_type = match &proto.return_type {
            Some(ret_type) => match ret_type {
                BaseType::StringType => {
                    let struct_type = self.context.struct_type(
                        &[
                            self.context
                                .i8_type()
                                .ptr_type(AddressSpace::default())
                                .into(),
                            self.context.i32_type().into(),
                            self.context.i32_type().into(),
                        ],
                        false,
                    );
                    struct_type
                        .ptr_type(AddressSpace::default())
                        .fn_type(args_types, false)
                }
                // BaseType::Void => self.context.void_type().fn_type(args_types, false),
                _ => todo!(),
            },
            None => self.context.void_type().fn_type(args_types, false),
        };

        let fn_val = self
            .llvm_module
            .add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        // for (i, arg) in fn_val.get_param_iter().enumerate() {
        //     arg.into_float_value().set_name(proto.args[i].name.as_str());
        // }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.llvm_module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&mut self, arg_name: &String, return_type: StructType<'ctx>) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(return_type, arg_name.as_str())
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &Node) -> Result<ReturnValue<'ctx>, &'static str> {
        match *&expr {
            Node::Module(module) => Err("module todo"),

            Node::Def(def) => Err("def todo"),

            Node::AssignLocalVar(asgn_lvar) => {
                let lvar_name = &asgn_lvar.name;
                let initial_value = self.compile_expr(&asgn_lvar.value)?;
                let (alloca, return_type) = match &initial_value {
                    ReturnValue::FloatValue(_) => todo!(),
                    ReturnValue::ArrayPtrValue(pv) => {
                        let ret_type = self.context.struct_type(
                            &[
                                self.context
                                    .i8_type()
                                    .ptr_type(AddressSpace::default())
                                    .into(),
                                self.context.i32_type().into(),
                                self.context.i32_type().into(),
                            ],
                            false,
                        );

                        let alloca = self.create_entry_block_alloca(lvar_name, ret_type);
                        let loaded_val = self.builder.build_load(*pv, lvar_name);

                        self.builder.build_store(alloca, loaded_val);

                        (alloca, ret_type)
                    },
                    ReturnValue::VoidValue => todo!(),
                };

                let value_ref = LocalVarRef { alloca, return_type };
                self.local_variables.insert(lvar_name.clone(), value_ref);

                Ok(initial_value)
            }

            Node::LocalVar(lvar) => {
                match self.local_variables.get(lvar.name.as_str()) {
                    Some(var) => {
                        // let load_inst = self.builder.build_load(var.alloca, lvar.name.as_str());
                        Ok(ReturnValue::ArrayPtrValue(var.alloca.as_basic_value_enum().into_pointer_value()))
                    },
                    None => Err("Could not find a matching variable."),
                }
            }

            Node::Binary(binary) => {
                match binary.op {
                    _ => {
                        Err("binary operations not supported yet")
                    }
                }
            },

            Node::InterpolableString(string) => {
                let i8_type = self.context.i8_type();
                let i8_array_type = i8_type.array_type(20);

                let hello_world_str = self.context.const_string(string.value.as_bytes(), false);
                let global_str = self.llvm_module.add_global(i8_array_type, None, "0");

                global_str.set_initializer(&hello_world_str);

                let malloc_string_fn = self.llvm_module.get_function("allocate_string").unwrap();
                let i32_type = self.context.i32_type();

                // Get a pointer to the first element of the array
                let zero = self.context.i32_type().const_int(0, false);
                let indices = [zero, zero];

                let element_pointer = unsafe {
                    self.builder
                        .build_gep(global_str.as_pointer_value(), &indices, "element_ptr")
                };

                let args = &[
                    element_pointer.into(),
                    i32_type
                        .const_int(string.value.len() as u64, false)
                        .as_basic_value_enum()
                        .into(),
                ];

                let nilla_str_ptr = self
                    .builder
                    .build_call(malloc_string_fn, args, "tmp")
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                // Ok(ReturnValue::ArrayPtrValue(global_str.as_pointer_value()))
                Ok(ReturnValue::ArrayPtrValue(
                    nilla_str_ptr.as_basic_value_enum().into_pointer_value(),
                ))
            }

            Node::Int(nb) => Ok(ReturnValue::FloatValue(
                self.context.f64_type().const_float(nb.value),
            )),

            // Node::VarIn {
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

            Node::Call(call) => match self.get_function(call.fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(call.args.len());

                    for arg in &call.args {
                        compiled_args.push(self.compile_expr(&arg)?);
                    }

                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                        .iter()
                        .map(|val| match *val {
                            ReturnValue::FloatValue(float_value) => float_value.into(),
                            ReturnValue::ArrayPtrValue(string_ptr) => string_ptr.into(),
                            _ => todo!(),
                        })
                        .collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => match value {
                            inkwell::values::BasicValueEnum::PointerValue(value) => {
                                Ok(ReturnValue::ArrayPtrValue(value))
                            }
                            inkwell::values::BasicValueEnum::FloatValue(value) => {
                                Ok(ReturnValue::FloatValue(value))
                            }
                            _ => todo!(),
                        },
                        None => Ok(ReturnValue::VoidValue),
                    }
                }
                None => Err("Unknown function."),
            },
            // Node::Conditional {
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

            // Node::For {
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
}
