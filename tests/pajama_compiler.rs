// macro_rules! req {
//   ($name:ident, $buf:expr, |$arg:ident| $body:expr) => (
//       req! {$name, $buf, Ok(Status::Complete($buf.len())), |$arg| $body }
//   );
//   ($name:ident, $buf:expr, $len:expr, |$arg:ident| $body:expr) => (
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

use pajama::pajama_compiler::PajamaCompiler;

use indoc::indoc;

#[test]
fn int_constant() {
    let input = "
        def _mlir_ciface_main
            a = 1
        end
    ";
    let output = PajamaCompiler::compile_to_string(&input);
    let expected_output = indoc! {"
        ^bb0:
          llvm.func @_mlir_ciface_main() {
            %0 = llvm.mlir.constant(1 : i64) : i64
            %1 = llvm.mlir.constant(1 : i64) : i64
            %2 = llvm.alloca %1 x i64 : (i64) -> !llvm.ptr<i64>
            llvm.store %0, %2 : !llvm.ptr<i64>
            llvm.return
          }
    "};

    assert_eq!(output, expected_output);
}
