## Design Notes

pub struct NillaString {
    buffer: *mut u8,
    length: i32,
    max_length: i32,
}

type MyClass_attributes {
    i32
    NillaString
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
