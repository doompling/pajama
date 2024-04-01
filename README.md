# Pajama Programming Language

Projammers?

Toolchain: pj
extension: .pj

## Design Notes

pub struct PajamaString {
    buffer: *mut u8,
    length: i32,
    max_length: i32,
}

type MyClass_attributes {
    i32
    PajamaString
    i32
}

type OtherClass_attributes {
    i32
}

%OpaquePtr = type opaque

type Class {
    i32 ;; class identifier
    %OpaquePtr ;; attributes
}

melior references:

https://github.com/raviqqe/autophagy/blob/64f371abb8b74f8918b0f9a6fe31b2b841286ad6/mlir/src/lib.rs#L16
https://github.com/0xmbcode/cairo_native/blob/fb932683adf6a6b16c265abd71c83fd2676dfba9/src/jit_runner.rs#L26
https://github.com/rrx/rust-lang-lower/blob/15a515a6833f611cdd7cf5a441ececbc56f3bdeb/lower/src/compile.rs
