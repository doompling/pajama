use pajama::pajama_compiler::PajamaCompiler;

use indoc::indoc;

macro_rules! build_test_fn {
    ($name:ident, $input:expr, $expected_output:expr) => {
        #[test]
        fn $name() {
            // Use `format!` so that $input is properly inserted into the string
            let input = format!(
                "def _mlir_ciface_main
                {}
                end",
                $input
            );

            let compiler_output = PajamaCompiler::compile_to_string(&input);
            let expected_output = $expected_output;

            assert_eq!(compiler_output, expected_output);
        }
    };
}


//
// Constant
//

build_test_fn! {
  int_constant,
  "1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(1 : i64) : i64
        llvm.return
      }
  "}
}

build_test_fn! {
  string_constant,
  "\"string_constant\"",
  indoc! {"
    ^bb0:
      llvm.mlir.global internal constant @\"0\"(\"string_constant\") {addr_space = 0 : i32}
      llvm.mlir.global internal constant @\"1\"() {addr_space = 0 : i32} : !llvm.struct<(ptr<i8>, i64, i64)> {
        %0 = llvm.mlir.addressof @\"0\" : !llvm.ptr<array<15 x i8>>
        %1 = llvm.getelementptr %0[0, 0] : (!llvm.ptr<array<15 x i8>>) -> !llvm.ptr<i8>
        %2 = llvm.mlir.undef : !llvm.struct<(ptr<i8>, i64, i64)>
        %3 = llvm.insertvalue %1, %2[0] : !llvm.struct<(ptr<i8>, i64, i64)>
        %4 = llvm.mlir.constant(15 : i64) : i64
        %5 = llvm.mlir.constant(15 : i64) : i64
        %6 = llvm.insertvalue %4, %3[1] : !llvm.struct<(ptr<i8>, i64, i64)>
        %7 = llvm.insertvalue %5, %6[2] : !llvm.struct<(ptr<i8>, i64, i64)>
        llvm.return %7 : !llvm.struct<(ptr<i8>, i64, i64)>
      }
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.addressof @\"1\" : !llvm.ptr<struct<(ptr<i8>, i64, i64)>>
        llvm.return
      }
  "}
}

//
// Assignment
//

build_test_fn! {
  int_assignment,
  "a = 1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(1 : i64) : i64
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.alloca %1 x i64 : (i64) -> !llvm.ptr<i64>
        llvm.store %0, %2 : !llvm.ptr<i64>
        llvm.return
      }
  "}
}

build_test_fn! {
  string_assignment,
  "a = \"string_assignment\"",
  indoc! {"
    ^bb0:
      llvm.mlir.global internal constant @\"0\"(\"string_assignment\") {addr_space = 0 : i32}
      llvm.mlir.global internal constant @\"1\"() {addr_space = 0 : i32} : !llvm.struct<(ptr<i8>, i64, i64)> {
        %0 = llvm.mlir.addressof @\"0\" : !llvm.ptr<array<17 x i8>>
        %1 = llvm.getelementptr %0[0, 0] : (!llvm.ptr<array<17 x i8>>) -> !llvm.ptr<i8>
        %2 = llvm.mlir.undef : !llvm.struct<(ptr<i8>, i64, i64)>
        %3 = llvm.insertvalue %1, %2[0] : !llvm.struct<(ptr<i8>, i64, i64)>
        %4 = llvm.mlir.constant(17 : i64) : i64
        %5 = llvm.mlir.constant(17 : i64) : i64
        %6 = llvm.insertvalue %4, %3[1] : !llvm.struct<(ptr<i8>, i64, i64)>
        %7 = llvm.insertvalue %5, %6[2] : !llvm.struct<(ptr<i8>, i64, i64)>
        llvm.return %7 : !llvm.struct<(ptr<i8>, i64, i64)>
      }
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.addressof @\"1\" : !llvm.ptr<struct<(ptr<i8>, i64, i64)>>
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.alloca %1 x !llvm.ptr<struct<(ptr<i8>, i64, i64)>> : (i64) -> !llvm.ptr<ptr<struct<(ptr<i8>, i64, i64)>>>
        llvm.store %0, %2 : !llvm.ptr<ptr<struct<(ptr<i8>, i64, i64)>>>
        llvm.return
      }
  "}
}

build_test_fn! {
  sum_assignment,
  "a = 1 + 1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(1 : i64) : i64
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.mlir.constant(2 : i64) : i64
        %3 = llvm.mlir.constant(1 : i64) : i64
        %4 = llvm.alloca %3 x i64 : (i64) -> !llvm.ptr<i64>
        llvm.store %2, %4 : !llvm.ptr<i64>
        llvm.return
      }
  "}
}

build_test_fn! {
  sub_assignment,
  "a = 2 - 1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(2 : i64) : i64
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.mlir.constant(1 : i64) : i64
        %3 = llvm.mlir.constant(1 : i64) : i64
        %4 = llvm.alloca %3 x i64 : (i64) -> !llvm.ptr<i64>
        llvm.store %2, %4 : !llvm.ptr<i64>
        llvm.return
      }
  "}
}


//
// Operators
//

build_test_fn! {
  sum_op,
  "1 + 1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(1 : i64) : i64
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.mlir.constant(2 : i64) : i64
        llvm.return
      }
  "}
}

build_test_fn! {
  sub_op,
  "2 - 1",
  indoc! {"
    ^bb0:
      llvm.func @_mlir_ciface_main() {
        %0 = llvm.mlir.constant(2 : i64) : i64
        %1 = llvm.mlir.constant(1 : i64) : i64
        %2 = llvm.mlir.constant(1 : i64) : i64
        llvm.return
      }
  "}
}
