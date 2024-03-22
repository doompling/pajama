use std::collections::HashMap;

use crate::lexer::Lexer;
use crate::mlir_codegen::Compiler;
use crate::parser::Parser;
use crate::semantic_analyzer::SemanticAnalyzer;

pub struct NillaCompiler {}

impl NillaCompiler {
    pub fn compile(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let mut precedence_map = NillaCompiler::build_op_precedence_map();
        let mut parser_result = Parser::start_parse(tokens, &mut precedence_map);

        SemanticAnalyzer::run(&mut parser_result);

        println!("RESULTS: ######");
        println!("{:#?}", parser_result);

        Compiler::compile(&parser_result);
    }

    fn build_op_precedence_map() -> HashMap<char, i32> {
        let mut op_precedence_map = HashMap::with_capacity(6);

        op_precedence_map.insert('<', 10);
        op_precedence_map.insert('+', 20);
        op_precedence_map.insert('-', 20);
        op_precedence_map.insert('*', 40);
        op_precedence_map.insert('/', 40);

        op_precedence_map
    }
}
