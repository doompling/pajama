mod codegen;
mod lexer;
mod pajama_compiler;
mod pajama_lib;
mod parser;
mod semantic_analyzer;

use pajama_compiler::PajamaCompiler;

use mimalloc_rust::raw::basic_allocation::*;
use mimalloc_rust::GlobalMiMalloc;

#[global_allocator]
static GLOBAL_MIMALLOC: GlobalMiMalloc = GlobalMiMalloc;

pub fn main() {
    let input = std::fs::read_to_string("dev.pjs").unwrap();
    PajamaCompiler::compile(&input);
}
