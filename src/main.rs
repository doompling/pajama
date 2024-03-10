mod codegen;
mod lexer;
mod mlir_codegen;
mod nilla_compiler;
mod parser;
mod semantic_analyzer;

use nilla_compiler::NillaCompiler;

use mimalloc_rust::raw::basic_allocation::*;
use mimalloc_rust::GlobalMiMalloc;

#[global_allocator]
static GLOBAL_MIMALLOC: GlobalMiMalloc = GlobalMiMalloc;

pub fn main() {
    let input = std::fs::read_to_string("dev.nla").unwrap();
    NillaCompiler::compile(&input);
}
