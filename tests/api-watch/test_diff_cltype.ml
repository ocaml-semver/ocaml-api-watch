open Api_watch
open Test_helpers

let%expect_test "Class type addition" =
  let reference = compile_interface {||} in
  let current =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m1 : float 
            method m2 : int -> int 
        end 
      |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class_type (cltype, Added)])}) |}]

let%expect_test "Class type removal" =
  let reference =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m1 : float 
            method m2 : int -> int 
        end 
      |}
  in
  let current = compile_interface {||} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class_type (cltype, Removed)])}) |}]

let%expect_test "Class type modification" =
  let reference =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m1 : float 
            method m2 : int -> int 
      end 
      |}
  in
  let current =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m2 : int -> float
            method m3 : float -> float
        end 
      |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class_type (cltype, Modified)])}) |}]

let%expect_test "Class type modification" =
  let reference =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m1 : float 
      end 
      |}
  in
  let current =
    compile_interface
      {| 
        class type cltype = 
          object 
            method m1 : float 
            method m2 : int -> int 
        end 
      |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class_type (cltype, Modified)])}) |}]
