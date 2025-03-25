open Api_watch
open Test_helpers

let%expect_test "test_subst_type_params_on_alias_types" =
  let interface =
    compile_interface
      {|
        type ('a, 'b) t = ('a * 'b) list
        type u = (string, int) t
      |}
  in
  let env, type_decls =
    List.fold_right
      (fun sig_item (env, type_decls) ->
        match sig_item with
        | Types.Sig_type (id, td, _, _) ->
            (Env.add_type ~check:true id td env, td :: type_decls)
        | _ -> (env, type_decls))
      interface (Env.empty, [])
  in
  let type_expr =
    Option.get (List.hd (List.tl type_decls)).Types.type_manifest
  in
  let subst_type_expr =
    Typing_env.subst_type_params ~typing_env:env ~type_expr
      ~path:(get_path type_expr) ~args:(get_args type_expr)
  in
  match subst_type_expr with
  | `Expanded _ -> assert false
  | `Expr e ->
      Printtyp.type_expr Format.std_formatter e;
      [%expect {| (string * int) list |}]

let%expect_test "test_subst_type_params_on_nominal_types" =
  let interface =
    compile_interface
      {|
        type ('a, 'b) t = { f : 'a * 'b }
        type u = (string, int) t

      |}
  in
  let env, type_decls =
    List.fold_right
      (fun sig_item (env, type_decls) ->
        match sig_item with
        | Types.Sig_type (id, td, _, _) ->
            (Env.add_type ~check:true id td env, td :: type_decls)
        | _ -> (env, type_decls))
      interface (Env.empty, [])
  in
  let type_expr =
    Option.get (List.hd (List.tl type_decls)).Types.type_manifest
  in
  let subst_type_expr =
    Typing_env.subst_type_params ~typing_env:env ~type_expr
      ~path:(get_path type_expr) ~args:(get_args type_expr)
  in
  match subst_type_expr with
  | `Expr _ -> assert false
  | `Expanded e ->
      Printtyp.type_expr Format.std_formatter e;
      [%expect {| (string, int) t |}]

let%expect_test "test_subst_type_params_on_type_not_in_the_env" =
  let interface =
    compile_interface
      {|
        type ('a, 'b) t = ('a * 'b) list
        type u = (string, int) t 
      |}
  in
  let env, type_decls =
    List.fold_right
      (fun sig_item (env, type_decls) ->
        match sig_item with
        | Types.Sig_type (_, td, _, _) -> (env, td :: type_decls)
        | _ -> (env, type_decls))
      interface (Env.empty, [])
  in
  let type_expr =
    Option.get (List.hd (List.tl type_decls)).Types.type_manifest
  in
  let subst_type_expr =
    Typing_env.subst_type_params ~typing_env:env ~type_expr
      ~path:(get_path type_expr) ~args:(get_args type_expr)
  in
  match subst_type_expr with
  | `Expanded e ->
      Printtyp.type_expr Format.std_formatter e;
      [%expect {| (string, int) t |}]
  | `Expr _ -> assert false
