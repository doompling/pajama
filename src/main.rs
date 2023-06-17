use std::collections::HashMap;
use std::io::Write;

use libc::printf;
use libc::c_char;

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

#[no_mangle]
pub extern "C" fn print(message: *const c_char) {
    unsafe {
        printf(message);
    }
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

#[used]
// static EXTERNAL_FNS2: [extern "Rust" fn(&[u8; 19]); 1] = [print];
static EXTERNAL_FNS2: [extern "C" fn(*const c_char); 1] = [print];

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
