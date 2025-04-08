open Api_watch
open Test_helpers

let pp_option fmt opt =
  Fmt.option
    ~none:(fun fmt () -> Fmt.string fmt "None")
    (Printtyp.type_declaration (Ident.create_local "type_name"))
    fmt opt

let pp_expansion_item fmt (type_expr, type_decl_opt) =
  Fmt.pf fmt "(%a, %a)" pp_option type_decl_opt Printtyp.type_expr type_expr

let pp_expansion_lst fmt lst =
  Format.fprintf fmt "@[<hv 2>[";
  Format.force_newline ();
  Fmt.pf fmt "%a"
    (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";@;") pp_expansion_item)
    lst;
  Format.fprintf fmt "@]@;]"

let interface =
  compile_interface
    {|
    type ('a, 'b) t1 = ('a * 'b) list
    type ('a, 'b) t2 = ('a, 'b) t1
    val v1 : (string, int) t2

    type ('a, 'b) t3 = { f1 : 'a; f2 : 'b }
    type ('a, 'b) t4 = ('a, 'b) t3
    val v2 : (string, int) t4

    type ('a, 'b) not_in_env = 'a * 'b
    val v3 : (string, int) not_in_env
|}

let env =
  List.fold_right
    (fun sig_item env ->
      match sig_item with
      | Types.Sig_type (id, td, _, _) ->
          if String.starts_with ~prefix:"not_in_env" (Ident.name id) then env
          else Env.add_type ~check:true id td env
      | _ -> env)
    interface Env.empty

let value_map =
  List.fold_left
    (fun map sig_item ->
      match sig_item with
      | Types.Sig_value (id, vd, _) ->
          String_map.add (Ident.name id) vd.val_type map
      | _ -> map)
    String_map.empty interface

let%expect_test "test_expansion_lst_on_alias_types" =
  let v1_type = String_map.find "v1" value_map in
  let path, args = get_tconstr v1_type in
  let expansion_lst =
    Typing_env.expansion_lst ~typing_env:env ~type_expr:v1_type ~path ~args
  in
  pp_expansion_lst Format.std_formatter expansion_lst;
  [%expect
    {|
    [
      (type ('a, 'b) type_name = ('a * 'b) list, (string * int) list);
      (type ('a, 'b) type_name = ('a, 'b) t1, (string, int) t1);
      (None, (string, int) t2)
    ]
    |}]

let%expect_test "test_expansion_lst_on_nominal_types" =
  let v2_type = String_map.find "v2" value_map in
  let path, args = get_tconstr v2_type in
  let expansion_lst =
    Typing_env.expansion_lst ~typing_env:env ~type_expr:v2_type ~path ~args
  in
  pp_expansion_lst Format.std_formatter expansion_lst;
  [%expect
    {|
    [
      (type ('a, 'b) type_name = ('a, 'b) t3, (string, int) t3);
      (None, (string, int) t4)
    ]
    |}]

let%expect_test "test_expansion_lst_on_type_not_in_the_env" =
  let v3_type = String_map.find "v3" value_map in
  let path, args = get_tconstr v3_type in
  let expansion_lst =
    Typing_env.expansion_lst ~typing_env:env ~type_expr:v3_type ~path ~args
  in
  pp_expansion_lst Format.std_formatter expansion_lst;
  [%expect {|
    [
      (None, (string, int) not_in_env)
    ]
    |}]
