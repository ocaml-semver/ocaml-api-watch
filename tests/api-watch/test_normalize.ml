open Api_watch
open Test_helpers
open Intermed.TypeDecl

let%expect_test "test_normalize_type_declarations" =
  let ref_name, reference =
    compile_interface {|type ('a, 'b) t = 'a * 'b|}
    |> first_type_decl |> Option.get
  in
  let cur_name, current =
    compile_interface {|type ('c, 'd, 'e) t = 'c * 'd * 'e|}
    |> first_type_decl |> Option.get
  in
  Normalize.type_decls ~reference ~current;
  let reference_str = Text_diff.td_to_lines ref_name reference in
  List.iter Format.print_string reference_str;
  Format.force_newline ();
  let current_str = Text_diff.td_to_lines cur_name current in
  List.iter Format.print_string current_str;
  [%expect
    {|
              type ('t1, 't2) t = 't1 * 't2
              type ('t1, 't2, 't3) t = 't1 * 't2 * 't3 |}]

let%expect_test "test_normalize_is_type_params_true" =
  let reference =
    List.init 5 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (CCString.of_char (Char.chr (Char.code 'a' + i)))))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  let current =
    List.init 3 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (CCString.of_char (Char.chr (Char.code 'a' + i)))))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  Printf.printf "%b" (Normalize.is_params ~reference ~current);
  [%expect "true"]

let%expect_test "test_normalize_is_type_params_false" =
  let reference =
    List.init 5 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (CCString.of_char (Char.chr (Char.code 'a' + i + 1)))))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  let current =
    List.init 3 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (CCString.of_char (Char.chr (Char.code 'a' + i)))))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  Printf.printf "%b" (Normalize.is_params ~reference ~current);
  [%expect "false"]

let%expect_test "test_normalize_type_params_arity" =
  let reference =
    List.init 5 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (Printf.sprintf "t%d" i)))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  let current =
    List.init 3 (fun i ->
        {
          type_expr =
            Types.create_expr
              (Tvar (Some (Printf.sprintf "t%d" i)))
              ~level:0 ~scope:0 ~id:i;
        })
  in
  let normed_ref, normed_cur = Normalize.params_arity ~reference ~current in
  Printf.printf "%b"
    (Ctype.is_equal Env.empty true
       (List.map (fun param -> param.type_expr) normed_ref)
       (List.map (fun param -> param.type_expr) reference)
    && Ctype.is_equal Env.empty true
         (List.map (fun param -> param.type_expr) normed_cur)
         (List.map (fun param -> param.type_expr) current
         @ List.init 2 (fun _ ->
               Types.create_expr (Tvar None) ~level:0 ~scope:0 ~id:0)));
  [%expect "true"]
