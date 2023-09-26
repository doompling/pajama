extern crate inkwell;

use inkwell::context::Context;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;

fn main() {
    let context = Context::create();
    let module = context.create_module("hello_world");
    let builder = context.create_builder();
    let i8_type = context.i8_type();
    let i32_type = context.i32_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);
    let i8_array_type = i8_type.array_type(20);
    
    // Declare the external "puts" function
    let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
    module.add_function("puts", puts_type, None);
    
    // Define the global string constant
    let hello_world_str = context.const_string(b"Hello LLVM-C world!", false);
    let global_str = module.add_global(i8_array_type, None, "0");
    global_str.set_initializer(&hello_world_str);
    global_str.set_linkage(inkwell::module::Linkage::Internal);
    
    // Define the "sayHelloWorld" function
    let say_hello_world_type = i32_type.fn_type(&[], false);
    let say_hello_world_func = module.add_function("sayHelloWorld", say_hello_world_type, None);
    let basic_block = context.append_basic_block(&say_hello_world_func, "aName");
    
    builder.position_at_end(&basic_block);
    
    let zero = context.i32_type().const_int(0, false);
    let hello_world_gep = unsafe {
        builder.build_gep(global_str.as_pointer_value(), &[zero, zero], "helloWorldGEP")
    };
    
    let puts_func = module.get_function("puts").unwrap().as_global_value().as_pointer_value();
    let puts_call = builder.build_call(puts_func, &[hello_world_gep.into()], "putsCall");
    
    builder.build_return(None);
    
    // Verify the module and print the LLVM IR
    module.verify().unwrap();
    module.print_to_stderr();
}
