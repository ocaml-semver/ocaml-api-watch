open Types
open Intermed.TypeDecl

let get_type_param_name param =
  match get_desc param with Tvar (Some name) -> name | _ -> assert false

let gen_unique_type_var_name =
  let counter = ref 1 in
  fun ~reset ->
    if reset then counter := 0 else ();
    let unique_name = Printf.sprintf "t%d" !counter in
    counter := !counter + 1;
    unique_name

let mutate_type_expr desc te =
  let tran = Transient_expr.repr te in
  Transient_expr.set_desc tran desc

let rec type_params reference current =
  match (reference, current) with
  | [], [] ->
      let _ = gen_unique_type_var_name ~reset:true in
      ()
  | { type_expr } :: reference', [] ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) type_expr;
      type_params reference' []
  | [], { type_expr } :: current' ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) type_expr;
      type_params [] current'
  | ( { type_expr = ref_type_expr } :: reference',
      { type_expr = cur_type_expr } :: current' ) ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) ref_type_expr;
      mutate_type_expr (Tvar (Some normed_name)) cur_type_expr;
      type_params reference' current'

let type_decls ~reference ~current =
  type_params reference.params current.params

let rec is_params ~reference ~current =
  match (reference, current) with
  | [], _ | _, [] -> true
  | { type_expr = ref_type_expr } :: reference',
    { type_expr = cur_type_expr } :: current' ->
      String.equal
        (get_type_param_name ref_type_expr)
        (get_type_param_name cur_type_expr)
      && is_params ~reference:reference' ~current:current'

let append_tvar_none n params_lst =
  let rest =
    List.init n (fun _ -> { type_expr =
                              create_expr (Tvar None) ~level:0 ~scope:0 ~id:0 })
  in
  params_lst @ rest

let params_arity ~reference ~current =
  let ref_len = List.length reference in
  let cur_len = List.length current in
  if ref_len = cur_len then (reference, current)
  else if ref_len > cur_len then
    (reference, append_tvar_none (ref_len - cur_len) current)
  else (append_tvar_none (cur_len - ref_len) reference, current)
