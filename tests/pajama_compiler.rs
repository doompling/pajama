// macro_rules! req {
//   ($name:ident, $input:expr, $expected_output:expr) => (
//       req! {$input, $expected_output, Ok(Status::Complete($buf.len())), |$arg| $body }
//   );
//   ($name:ident, $input:expr, $expected_output:expr) => (
//   #[test]
//   fn $name() {
//       let mut headers = [EMPTY_HEADER; NUM_OF_HEADERS];
//       let mut req = Request::new(&mut headers[..]);
//       let status = req.parse($buf.as_ref());
//       assert_eq!(status, $len);
//       closure(req);

//       fn closure($arg: Request<'_, '_>) {
//           $body
//       }
//   }
//   )
// }

// req! {
//   urltest_001,
//   b"GET /bar;par?b HTTP/1.1\r\nHost: foo\r\n\r\n",
//   |req| {
//       assert_eq!(req.method.unwrap(), "GET");
//       assert_eq!(req.path.unwrap(), "/bar;par?b");
//       assert_eq!(req.version.unwrap(), 1);
//       assert_eq!(req.headers.len(), 1);
//       assert_eq!(req.headers[0].name, "Host");
//       assert_eq!(req.headers[0].value, b"foo");
//   }
// }

// #[test]
// fn int_constant() {
//     let input = "
//         def _mlir_ciface_main
//             a = 1
//         end
//     ";
//     let output = PajamaCompiler::compile_to_string(&input);
//     let expected_output = indoc! {"
//         ^bb0:
//           llvm.func @_mlir_ciface_main() {
//             %0 = llvm.mlir.constant(1 : i64) : i64
//             %1 = llvm.mlir.constant(1 : i64) : i64
//             %2 = llvm.alloca %1 x i64 : (i64) -> !llvm.ptr<i64>
//             llvm.store %0, %2 : !llvm.ptr<i64>
//             llvm.return
//           }
//     "};

//     assert_eq!(output, expected_output);
// }

use pajama::pajama_compiler::PajamaCompiler;

use indoc::indoc;

// macro_rules! build_test_fn {
//     ($name:ident, $input:expr, $expected_output:expr) => {
//         #[test]
//         fn $name() {
//             let input = "
//                 def _mlir_ciface_main
//                     $input
//                 end
//             ";
//             let compiler_output = PajamaCompiler::compile_to_string(&input);
//             let expected_output = indoc! {"
//                 ^bb0:
//                   llvm.func @_mlir_ciface_main() {
//                     %0 = llvm.mlir.constant(1 : i64) : i64
//                     %1 = llvm.mlir.constant(1 : i64) : i64
//                     %2 = llvm.alloca %1 x i64 : (i64) -> !llvm.ptr<i64>
//                     llvm.store %0, %2 : !llvm.ptr<i64>
//                     llvm.return
//                   }
//             "};

//             assert_eq!(compiler_output, expected_output);
//         }
//     }
// }

// build_test_fn! {
//     int_constant,
//     "a = 1",
//     indoc! {"
//       ^bb0:
//         llvm.func @_mlir_ciface_main() {
//           %0 = llvm.mlir.constant(1 : i64) : i64
//           %1 = llvm.mlir.constant(1 : i64) : i64
//           %2 = llvm.alloca %1 x i64 : (i64) -> !llvm.ptr<i64>
//           llvm.store %0, %2 : !llvm.ptr<i64>
//           llvm.return
//         }
//     "}
// }

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
