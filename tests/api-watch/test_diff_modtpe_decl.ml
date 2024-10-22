open Api_watch
open Test_helpers

let%expect_test "Modified module type" =
  let reference =
    compile_interface
      {|
    module type M = sig
      val b : int list -> int
    end
  |}
  in
  let current =
    compile_interface
      {|
    module type M = sig
      val b : float list -> float
    end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module M: {Modified (Supported [ Value (b, Modified)])}])}) |}]

let%expect_test "Concrete Module_type to abstract module_type change" =
  let reference =
    compile_interface {|
  module type P = sig val x : int end
  |}
  in
  let current = compile_interface {|
  module type P
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type P: {Modified (Unsupported)}])}) |}]

let%expect_test "Module type addition" =
  let reference = compile_interface {|
  |} in
  let current =
    compile_interface {|
  module type M = sig val x : int end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type M: Added])}) |}]

let%expect_test "Module type removal" =
  let reference =
    compile_interface {|
   module type P = sig val y : int list end
    |}
  in
  let current = compile_interface {|
    |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type P: Removed])}) |}]
