open Api_watch_diff

let pp_diff_list fmt diffs =
  let pp_item_change fmt = function
    | Added -> Format.fprintf fmt "Added"
    | Removed -> Format.fprintf fmt "Removed"
    | Modified -> Format.fprintf fmt "Modified"
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

let%expect_test "Simple value, identical" =
  let reference = Test_helpers.compile_interface {|val x : int|} in
  let current = Test_helpers.compile_interface {|val x : int|} in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let%expect_test "Simple value, modified" =
  let reference = Test_helpers.compile_interface {|val x : int|} in
  let current = Test_helpers.compile_interface {|val x : string|} in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (x, Modified)]|}]

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

let%expect_test "Modifying a simple type" =
  let reference = Test_helpers.compile_interface {|type t = int|} in
  let current = Test_helpers.compile_interface {|type t = string|} in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Modifying a type used in a value" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = int
    val f : t -> string
    |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = float
    val f : t -> string
    |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (f, Modified)]|}]

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

let remove_module_signature = Test_helpers.compile_interface {|

|}

let%expect_test "Removing a module" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:remove_module_signature
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

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

let%expect_test "One value more general than the other" =
  let general = Test_helpers.compile_interface {|val x : 'a list|} in
  let specialized = Test_helpers.compile_interface {|val x : float list|} in
  let result =
    Api_watch_diff.diff_interface ~reference:general ~current:specialized
  in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Value (x, Modified)]|}];
  let rev_result =
    Api_watch_diff.diff_interface ~reference:specialized ~current:general
  in
  Format.printf "%a" pp_diff_list rev_result;
  [%expect {|[Value (x, Modified)]|}]

let%expect_test "Same abstract type" =
  let reference =
    Test_helpers.compile_interface {|
    type t
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface {|
    type t
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let%expect_test "Same record type" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let%expect_test "Adding a record field" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float; c:bool}
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Removing a record field" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface {|
    type t = {a:int}
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Modifying a record field" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = {a:int; b:string}
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Same variant type" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = A | B of int
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[]|}]

let%expect_test "Adding a variant" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = A | B of int | C
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Removing a variant" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface {|
    type t = A
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]

let%expect_test "Modifying a variant type" =
  let reference =
    Test_helpers.compile_interface
      {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    Test_helpers.compile_interface
      {|
    type t = A | B of float
    val x : t
  |}
  in
  let result = Api_watch_diff.diff_interface ~reference ~current in
  Format.printf "%a" pp_diff_list result;
  [%expect {|[Any]|}]
