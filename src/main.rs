mod nilla_compiler;
mod parser;
mod lexer;
mod codegen;

use nilla_compiler::NillaCompiler;

use mimalloc_rust::GlobalMiMalloc;
use mimalloc_rust::raw::basic_allocation::*;

#[global_allocator]
static GLOBAL_MIMALLOC: GlobalMiMalloc = GlobalMiMalloc;

pub fn main() {
    let input = std::fs::read_to_string("dev.nla").unwrap();
    NillaCompiler::compile(&input);
}
