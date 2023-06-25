use std::collections::HashMap;
use std::io::Write;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::OptimizationLevel;
use inkwell::targets::{TargetMachine, InitializationConfig, Target, RelocMode, CodeModel};

#[cfg(not(any(feature = "llvm15-0")))]
mod implementation_typed_pointers;

// #[llvm_versions(4.0..=14.0)]
use crate::implementation_typed_pointers::*;

// ======================================================================================
// PROGRAM ==============================================================================
// ======================================================================================

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

use mimalloc_rust::GlobalMiMalloc;
use mimalloc_rust::raw::basic_allocation::*;

#[global_allocator]
static GLOBAL_MIMALLOC: GlobalMiMalloc = GlobalMiMalloc;

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

        unsafe { core::ptr::copy(bytes, ptr, length as usize); }

        let nilla_string = NillaString {
            buffer: ptr,
            length,
            max_length: length
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
pub extern "Rust" fn print(nilla_string: *const NillaString) {
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
static EXTERNAL_FNS4: [extern "Rust" fn(*const NillaString); 1] = [print];

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

#[used]
static EXTERNAL_FNS2: [extern "Rust" fn(bytes: *const u8, initial_length: i32) -> *mut NillaString; 1] = [NillaString::allocate_string];

/// Entry point of the program; acts as a REPL.
// #[llvm_versions(4.0..=14.0)]
pub fn main() {
    // use self::inkwell::support::add_symbol;
    let mut display_lexer_output = false;
    let mut display_parser_output = false;
    let mut display_compiler_output = false;

    for arg in std::env::args() {
        match arg.as_str() {
            "--dl" => display_lexer_output = true,
            "--dp" => display_parser_output = true,
            "--dc" => display_compiler_output = true,
            _ => (),
        }
    }

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
    let module = context.create_module("repl");
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

    let mut previous_exprs = Vec::new();

    // Read input from stdin
    let input = std::fs::read_to_string("dev.nla").unwrap();

    // Build precedence map
    let mut prec = HashMap::with_capacity(6);

    prec.insert('=', 2);
    prec.insert('<', 10);
    prec.insert('+', 20);
    prec.insert('-', 20);
    prec.insert('*', 40);
    prec.insert('/', 40);

    // Parse and (optionally) display input
    if display_lexer_output {
        println!(
            "-> Attempting to parse lexed input: \n{:?}\n",
            Lexer::new(input.as_str()).collect::<Vec<Token>>()
        );
    }

    // make module
    let module = context.create_module("tmp");


    // NillaString struct type
    let string_struct_type = context.struct_type(&[context.i8_type().ptr_type(AddressSpace::default()).into(), context.i32_type().into(), context.i32_type().into()], false);

    // Define the function type
    // let struct_type = context.struct_type(&[], false);
    let struct_ptr_type = string_struct_type.ptr_type(AddressSpace::default());
    let bytes_ptr_type = context.i8_type().ptr_type(AddressSpace::default());
    let length_type = context.i32_type();

    let function_type = struct_ptr_type.fn_type(
        &[bytes_ptr_type.into(), length_type.into()],
        // args,
        false
    );

    module.add_function("allocate_string", function_type, None);

    // println!("{:#?}", module.print_to_string());

    // // recompile every previously parsed function into the new module
    // for prev in &previous_exprs {
    //     Compiler::compile(&context, &builder, &fpm, &module, prev)
    //         .expect("Cannot re-add previously compiled function.");
    // }

     for line in input.split(";") {
         if line.trim().is_empty() {
            continue;
         }

         let (name, is_anonymous) = match Parser::new(line.to_string(), &mut prec).parse() {
             Ok(fun) => {
                 let is_anon = fun.is_anon;

                 if display_parser_output {
                     if is_anon {
                         println!("-> Expression parsed: \n{:?}\n", fun.body);
                     } else {
                         println!("-> Function parsed: \n{:?}\n", fun);
                     }
                 }

                 match Compiler::compile(&context, &builder, &fpm, &module, &fun) {
                     Ok(function) => {
                         if display_compiler_output {
                             // Not printing a new line since LLVM automatically
                             // prefixes the generated string with one
                             print_flush!("-> Expression compiled to IR:");
                             function.print_to_stderr();
                         }

                         if !is_anon {
                             // only add it now to ensure it is correct
                             previous_exprs.push(fun);
                         }

                         (function.get_name().to_str().unwrap().to_string(), is_anon)
                     },
                     Err(err) => {
                         println!("!> Error compiling function: {}", err);
                         std::process::exit(1);
                     },
                 }
             },
             Err(err) => {
                 println!("!> Error parsing expression: {}", err);
                 std::process::exit(1);
             },
         };

         if is_anonymous {
             // let path = Path::new("./first_try");

             // target_machine. write_to_file(&module, targets::FileType::Object, path).unwrap();

             // module.write_bitcode_to_path(path);

             println!("###################");
             println!("{}", module.print_to_string().to_string());
             println!("###################");


             let ee = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

             let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str()) };
             let compiled_fn = match maybe_fn {
                 Ok(f) => f,
                 Err(err) => {
                     println!("!> Error during execution: {:?}", err);
                     std::process::exit(1);
                 },
             };

             unsafe {
                 println!("=> {}", compiled_fn.call());
             }
         }
     }
}
