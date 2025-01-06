open Api_watch
open Test_helpers

let%expect_test "Class Addition" =
  let reference =
    {|
  class cls1 : object
    method m1 : int -> int
  end
  class cls2 : object
    method m2 : string -> string
  end
|}
  in
  let current =
    {|
  class cls1 : object
    method m1 : int -> int
  end
  class cls2 : object
    method m2 : string -> string
  end
  class cls3 : object
    method m3 : float -> float
  end
|}
  in
  try
    let ref_compiled = compile_interface reference in
    let curr_compiled = compile_interface current in
    let result =
      Diff.interface ~module_name:"Main" ~reference:ref_compiled
        ~current:curr_compiled
    in
    Format.printf "%a" pp_diff_option result;
    [%expect
      {| Some (Module Main: {Modified (Supported [ Class (cls3, Added)])}) |}]
  with e ->
    Format.printf "Error: %s" (Printexc.to_string e);
    [%expect.unreachable]

let%expect_test "Class Removal" =
  let reference =
    {|
  class cls1 : object
    method m1 : int -> int
  end
  class cls2 : object
    method m2 : string -> string
  end
|}
  in
  let current = {|
  class cls1 : object
    method m1 : int -> int
  end
|} in
  try
    let ref_compiled = compile_interface reference in
    let curr_compiled = compile_interface current in
    let result =
      Diff.interface ~module_name:"Main" ~reference:ref_compiled
        ~current:curr_compiled
    in
    Format.printf "%a" pp_diff_option result;
    [%expect
      {| Some (Module Main: {Modified (Supported [ Class (cls2, Removed)])}) |}]
  with e ->
    Format.printf "Error: %s" (Printexc.to_string e);
    [%expect.unreachable]

let%expect_test "Class Modification" =
  let reference =
    compile_interface
      {|
      class cls1 : object 
        method m1: int -> int
        method m2: int -> char
      end 
  |}
  in
  let current =
    compile_interface
      {|
      class cls1 : object 
        method m2: float -> float
        method m3: char -> char
      end 
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class (cls1, Modified)])}) |}]

let%expect_test "Class Modification" =
  let reference =
    compile_interface
      {|
      class cls1 : object 
        method m1: int -> int
        method m2: char -> int
      end 
  |}
  in
  let current =
    compile_interface
      {|
      class cls1 : object 
        method m1: int -> int
        method m2: char -> int
        method m3: string -> string
      end 
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Class (cls1, Modified)])}) |}]
