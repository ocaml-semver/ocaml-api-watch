open Api_watch
open Test_helpers

let interface =
  compile_interface
    {|
    type ('a, 'b) t1 = ('a * 'b) list
    val v1 : (string, int) t1

    type ('a, 'b) t2 = { f1 : 'a; f2 : 'b }
    val v2 : (string, int) t2

    type ('a, 'b) t3 = ('a * 'b) list
    val v3 : (string, int) t3
|}

let env =
  List.fold_right
    (fun sig_item env ->
      match sig_item with
      | Types.Sig_type (id, td, _, _) ->
          if String.starts_with ~prefix:"t3" (Ident.name id) then env
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

let%expect_test "test_expand_tconstr_on_alias_types" =
  let v1_type = String_map.find "v1" value_map in
  let path, args = get_tconstr v1_type in
  let expanded_type_expr =
    Typing_env.expand_tconstr ~typing_env:env ~path ~args
  in
  match expanded_type_expr with
  | None -> assert false
  | Some e ->
      Printtyp.type_expr Format.std_formatter e;
      [%expect {| (string * int) list |}]

let%expect_test "test_expand_tconstr_on_nominal_types" =
  let v2_type = String_map.find "v2" value_map in
  let path, args = get_tconstr v2_type in
  let expanded_type_expr =
    Typing_env.expand_tconstr ~typing_env:env ~path ~args
  in
  match expanded_type_expr with
  | Some _ -> assert false
  | None ->
      Printtyp.type_expr Format.std_formatter v2_type;
      [%expect {| (string, int) t2 |}]

let%expect_test "test_expand_tconstr_on_type_not_in_the_env" =
  let v3_type = String_map.find "v3" value_map in
  let path, args = get_tconstr v3_type in
  let expanded_type_expr =
    Typing_env.expand_tconstr ~typing_env:env ~path ~args
  in
  match expanded_type_expr with
  | None ->
      Printtyp.type_expr Format.std_formatter v3_type;
      [%expect {| (string, int) t3 |}]
  | Some _ -> assert false
