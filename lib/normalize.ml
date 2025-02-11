open Types

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
  | ref_param :: reference', [] ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) ref_param;
      type_params reference' []
  | [], cur_param :: current' ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) cur_param;
      type_params [] current'
  | ref_param :: reference', cur_param :: current' ->
      let normed_name = gen_unique_type_var_name ~reset:false in
      mutate_type_expr (Tvar (Some normed_name)) ref_param;
      mutate_type_expr (Tvar (Some normed_name)) cur_param;
      type_params reference' current'

let type_declarations ~reference ~current =
  type_params reference.type_params current.type_params

let rec is_type_params ~reference ~current =
  match (reference, current) with
  | [], _ | _, [] -> true
  | ref_type_param :: reference', cur_type_param :: current' ->
      String.equal
        (get_type_param_name ref_type_param)
        (get_type_param_name cur_type_param)
      && is_type_params ~reference:reference' ~current:current'

let type_params_arity ~reference ~current =
  if List.length reference = List.length current then (reference, current)
  else if List.length reference > List.length current then
    let rest =
      List.init
        (List.length reference - List.length current)
        (fun _ -> create_expr (Tvar None) ~level:0 ~scope:0 ~id:0)
    in
    (reference, current @ rest)
  else
    let rest =
      List.init
        (List.length current - List.length reference)
        (fun _ -> create_expr (Tvar None) ~level:0 ~scope:0 ~id:0)
    in
    (reference @ rest, current)
