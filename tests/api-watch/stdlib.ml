open Api_watch
open Test_helpers

let%expect_test "Identical types" =
  let reference = compile_interface {| type t = String.t |} in
  let current = compile_interface {| type t = string |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]
