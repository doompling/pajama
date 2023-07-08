use std::collections::HashMap;

use crate::codegen::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub struct NillaCompiler {}

impl NillaCompiler {
    pub fn compile(input: &str) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();

        let mut precedence_map = NillaCompiler::build_op_precedence_map();
        let results = Parser::new(tokens, &mut precedence_map).parse();

        println!("{:#?}", results);
        println!("\nCompiling....");
        println!("\n###################");

        Compiler::compile(results.unwrap());
    }

    fn build_op_precedence_map() -> HashMap<char, i32> {
        let mut op_precedence_map = HashMap::with_capacity(6);

        op_precedence_map.insert('=', 2);
        op_precedence_map.insert('<', 10);
        op_precedence_map.insert('+', 20);
        op_precedence_map.insert('-', 20);
        op_precedence_map.insert('*', 40);
        op_precedence_map.insert('/', 40);

        op_precedence_map
    }
}
