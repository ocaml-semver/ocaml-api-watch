open Api_watch_diff
open Test_helpers

let%expect_test "Modules with multiple value and submodule changes" =
  let reference =
    compile_interface {|
  val x: int
  module M: sig 
  
  end|}
  in
  let current = compile_interface {|
  module M: sig
  type t
  end
  |} in
  let result = diff_interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect
    {|
    Some Module Main: [ Value (x, Removed);
    Module M: Unsupported changes]|}]
