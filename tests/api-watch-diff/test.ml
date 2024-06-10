open Api_watch_diff

let pp_diff_list fmt diffs =
  let pp_item_change fmt = function
    | Added -> Format.fprintf fmt "Added"
    | Removed -> Format.fprintf fmt "Removed"
    | Modified _ -> Format.fprintf fmt "Modified"
  in
  let pp_diff fmt = function
    | Value (name, change) ->
        Format.fprintf fmt "Value (%s, %a)" name pp_item_change change
    | Any -> Format.fprintf fmt "Any"
  in
  Format.fprintf fmt "[%a]" (Format.pp_print_list pp_diff) diffs

let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let x_int_value_signature =
  Test_helpers.compile_interface {|
    val x : int
    |}

let x_string_value_signature =
  Test_helpers.compile_interface {|
    val x : string
    |}

let%expect_test "Simple value, identical" =
  let result =
    Api_watch_diff.diff_interface ~reference:x_int_value_signature
      ~current:x_int_value_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let%expect_test "Simple value, modified" =
  let result =
    Api_watch_diff.diff_interface ~reference:x_int_value_signature
      ~current:x_string_value_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (x, Modified)]|}]

(* Signature for ref.mli:
      > type t = int
      > type unused_type = string
      > val f : t -> string *)
let ref_signature =
  Test_helpers.compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> string
    |}

let%expect_test "Same signature" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:ref_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

(* Signature for add_value.mli:
     > type t = int
     > type unused_type = string
     > val f : t -> string
     > val g : t -> t *)
let add_value_signature =
  Test_helpers.compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> string
    val g : t -> t
    |}

let%expect_test "Adding a value" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:add_value_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (g, Added)]|}]

(* Signature for remove_value.mli:
     > type t = int
     > type unused_type = string *)
let remove_value_signature =
  Test_helpers.compile_interface
    {|
  type t = int
  type unused_type = string
  |}

let%expect_test "Removing a value" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:remove_value_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (f, Removed)]|}]

(* Signature for modify_value.mli:
     > type t = int
     > type unused_type = string
     > val f : t -> t *)
let modify_value_signature =
  Test_helpers.compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> t
    |}

let%expect_test "Modifying a value" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:modify_value_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (f, Modified)]|}]

(* Signature for add_type.mli:
      > type t = int
      > type unused_type = string
      > val f : t -> string
      > type added_type = float *)
let add_type_signature =
  Test_helpers.compile_interface
    {|
    type t = int
     type unused_type = string
     type added_t = float
     val f : t -> string
     |}

let%expect_test "Adding a type" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:add_type_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

(* Signature for remove_type.mli:
    > type t = int
    > val f : t -> string
    > type t = float *)
let remove_type_signature =
  Test_helpers.compile_interface {|
  type t = int
  val f : t -> string
  |}

let%expect_test "Removing a type" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:remove_type_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

(* Signature for modify_type.mli:
   > type t = int
   > type unused_type = bool
   > val f : t -> string *)
let modify_type_signature =
  Test_helpers.compile_interface
    {|
    type t = int
    type unused_type = bool
    val f : t -> string
    |}

let%expect_test "Modifying a type" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:modify_type_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

(* Signature for file mod_ref.mli:
     > module M : sig val x : int end *)

let ref_module_signature =
  Test_helpers.compile_interface {|
  module M : sig val x : int end
  |}

let%expect_test "Same module" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:ref_module_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

(* Signature for add_module.mli:
     > module M : sig val x : int end
     > module N : sig val y : float end *)

let add_module_signature =
  Test_helpers.compile_interface
    {|
    module M : sig val x : int end
    module N : sig val y : float end
    |}

let%expect_test "Adding a module" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:add_module_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

(* Signature for remove_module.mli:
     > *)

let remove_module_signature = Test_helpers.compile_interface {|

|}

let%expect_test "Removing a module" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:remove_module_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

(* Signature for modify_module.mli:
    > module M : sig val x : float end *)

let modify_module_signature =
  Test_helpers.compile_interface {|
  module M : sig val x : float end
  |}

let%expect_test "Modifying a module" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:modify_module_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]
