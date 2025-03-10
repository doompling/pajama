use std::collections::HashMap;

use melior::dialect::DialectRegistry;
use melior::ir::{Location, Module};
use melior::pass::{conversion, PassManager};
use melior::utility::{register_all_dialects, register_all_llvm_translations};
use melior::{pass, Context, ExecutionEngine};

use crate::codegen::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::semantic_analyzer::SemanticAnalyzer;

pub struct PajamaCompiler {}

impl PajamaCompiler {
    pub fn compile_to_string(input: &str) -> String {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        println!("{:#?}", tokens);

        let mut precedence_map = PajamaCompiler::build_op_precedence_map();
        let mut parser_result = Parser::start_parse(tokens, &mut precedence_map);

        SemanticAnalyzer::run(&mut parser_result);

        println!("ParserResult after analysis: ######");
        println!("{:#?}", parser_result);

        let mlir_context = PajamaCompiler::create_mlir_context();
        let location = Location::unknown(&mlir_context);
        let mut mlir_module = Module::new(location);
        let mut compiler = Compiler::new(&mlir_context, &mlir_module, &parser_result);

        compiler.compile();

        //

        println!("PRE VERIFICATION:");
        println!("{}", mlir_module.body().to_string());

        assert!(mlir_module.as_operation().verify());

        let pass_manager = PassManager::new(&mlir_context);
        pass_manager.add_pass(conversion::create_func_to_llvm());

        pass_manager
            .nested_under("llvm.func")
            .add_pass(conversion::create_arith_to_llvm());
        pass_manager
            .nested_under("llvm.func")
            .add_pass(conversion::create_index_to_llvm());
        pass_manager.add_pass(conversion::create_scf_to_control_flow());
        pass_manager.add_pass(conversion::create_control_flow_to_llvm());
        pass_manager.add_pass(conversion::create_finalize_mem_ref_to_llvm());

        pass_manager.add_pass(conversion::create_func_to_llvm());

        pass_manager.run(&mut mlir_module).unwrap();

        assert!(mlir_module.as_operation().verify());

        println!("POST VERIFICATION:");
        println!("{}", mlir_module.body().to_string());

        let body = mlir_module.body().to_string();

        // For some reason spaces are being added to the ends of some lines
        // Maybe because of the extra spaces from `macro_rules! build_test_fn {` ?
        body.replace(" \n", "\n")

        // PajamaCompiler::invoke(&mlir_module);
    }

    pub fn compile_and_invoke(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        println!("{:#?}", tokens);

        let mut precedence_map = PajamaCompiler::build_op_precedence_map();
        let mut parser_result = Parser::start_parse(tokens, &mut precedence_map);

        SemanticAnalyzer::run(&mut parser_result);

        println!("ParserResult after analysis: ######");
        println!("{:#?}", parser_result);

        let mlir_context = PajamaCompiler::create_mlir_context();
        let location = Location::unknown(&mlir_context);
        let mut mlir_module = Module::new(location);
        let mut compiler = Compiler::new(&mlir_context, &mlir_module, &parser_result);

        compiler.compile();

        //

        println!("PRE VERIFICATION:");
        println!("{}", mlir_module.body().to_string());

        assert!(mlir_module.as_operation().verify());

        let pass_manager = PassManager::new(&mlir_context);
        pass_manager.add_pass(conversion::create_func_to_llvm());

        pass_manager
            .nested_under("llvm.func")
            .add_pass(conversion::create_arith_to_llvm());
        pass_manager
            .nested_under("llvm.func")
            .add_pass(conversion::create_index_to_llvm());
        pass_manager.add_pass(conversion::create_scf_to_control_flow());
        pass_manager.add_pass(conversion::create_control_flow_to_llvm());
        pass_manager.add_pass(conversion::create_finalize_mem_ref_to_llvm());

        pass_manager.add_pass(conversion::create_func_to_llvm());

        pass_manager.run(&mut mlir_module).unwrap();

        assert!(mlir_module.as_operation().verify());

        println!("POST VERIFICATION:");
        println!("{}", mlir_module.body().to_string());

        PajamaCompiler::invoke(&mlir_module);
    }

    pub fn invoke(mlir_module: &Module) {
        let engine = ExecutionEngine::new(mlir_module, 2, &[], false);

        unsafe {
            engine
                // .invoke_packed("main", &mut [&mut status_code as *mut i32 as *mut ()])
                .invoke_packed("main", &mut [])
                .unwrap();
        }
    }

    fn create_mlir_context() -> Context {
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

        context
    }

    fn build_op_precedence_map() -> HashMap<[char; 4], i32> {
        let mut op_precedence_map = HashMap::with_capacity(6);

        op_precedence_map.insert(['<', '\0', '\0', '\0'], 10); // Example precedence for '<'
        op_precedence_map.insert(['+', '\0', '\0', '\0'], 20); // Addition
        op_precedence_map.insert(['-', '\0', '\0', '\0'], 20); // Subtraction
        op_precedence_map.insert(['*', '\0', '\0', '\0'], 40); // Multiplication
        op_precedence_map.insert(['/', '\0', '\0', '\0'], 40); // Division
        op_precedence_map.insert(['%', '\0', '\0', '\0'], 40); // Modulo
        op_precedence_map.insert(['^', '\0', '\0', '\0'], 50); // Exponentiation (e.g., '**')

        op_precedence_map
    }
}
