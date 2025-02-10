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

let rec build_tvmaps ref_type_params cur_type_params (ref_tvmap, cur_tvmap) =
  match (ref_type_params, cur_type_params) with
  | [], [] ->
      let _ = gen_unique_type_var_name ~reset:true in
      (ref_tvmap, cur_tvmap)
  | ref_type_param :: ref_type_params', [] ->
      build_tvmaps ref_type_params' []
        ( String_map.add
            (get_type_param_name ref_type_param)
            (gen_unique_type_var_name ~reset:false)
            ref_tvmap,
          cur_tvmap )
  | [], cur_type_param :: cur_type_params' ->
      build_tvmaps [] cur_type_params'
        ( ref_tvmap,
          String_map.add
            (get_type_param_name cur_type_param)
            (gen_unique_type_var_name ~reset:false)
            cur_tvmap )
  | ref_type_param :: ref_type_params', cur_type_param :: cur_type_params' ->
      let unique_type_var_name = gen_unique_type_var_name ~reset:false in
      build_tvmaps ref_type_params' cur_type_params'
        ( String_map.add
            (get_type_param_name ref_type_param)
            unique_type_var_name ref_tvmap,
          String_map.add
            (get_type_param_name cur_type_param)
            unique_type_var_name cur_tvmap )

and create_type_expr type_desc type_expr =
  create_expr type_desc ~level:(get_level type_expr)
    ~scope:(get_scope type_expr) ~id:(get_id type_expr)

let rec type_expr tvmap te =
  match get_desc te with
  | Tvar None -> te
  | Tvar (Some name) ->
      create_type_expr (Tvar (Some (String_map.find name tvmap))) te
  | Tarrow (arg_label, te', te'', commutable) ->
      create_type_expr
        (Tarrow
           (arg_label, type_expr tvmap te', type_expr tvmap te'', commutable))
        te
  | Ttuple te_list ->
      create_type_expr (Ttuple (List.map (type_expr tvmap) te_list)) te
  | Tconstr (path, te_list, abbrev_memo) ->
      create_type_expr
        (Tconstr (path, List.map (type_expr tvmap) te_list, abbrev_memo))
        te
  | Tobject (te', funs) ->
      create_type_expr (Tobject (type_expr tvmap te', funs)) te
  | Tfield (name, kind, te', te'') ->
      create_type_expr
        (Tfield (name, kind, type_expr tvmap te', type_expr tvmap te''))
        te
  | Tnil -> te
  | Tsubst (te', te''_opt) ->
      create_type_expr
        (Tsubst (type_expr tvmap te', Option.map (type_expr tvmap) te''_opt))
        te
  | _ -> te

let type_params type_params tvmap =
  List.map (fun type_param -> type_expr tvmap type_param) type_params

let type_kind type_kind tvmap =
  match type_kind with
  | Type_record (lbls, x) ->
      let normalized_lbls =
        List.map
          (fun lbl -> { lbl with ld_type = type_expr tvmap lbl.ld_type })
          lbls
      in
      Type_record (normalized_lbls, x)
  | Type_variant (cstrs, x) ->
      let normalized_cstrs =
        List.map
          (fun cstr ->
            match cstr.cd_args with
            | Cstr_tuple type_exprs ->
                let type_exprs = List.map (type_expr tvmap) type_exprs in
                { cstr with cd_args = Cstr_tuple type_exprs }
            | Cstr_record lbls ->
                let normalized_lbls =
                  List.map
                    (fun lbl ->
                      { lbl with ld_type = type_expr tvmap lbl.ld_type })
                    lbls
                in
                { cstr with cd_args = Cstr_record normalized_lbls })
          cstrs
      in
      Type_variant (normalized_cstrs, x)
  | Type_abstract _ -> type_kind
  | Type_open -> type_kind

and normalize_type_manifest type_manifest tvmap =
  Option.map (type_expr tvmap) type_manifest

let type_declarations ~reference ~current =
  let normalize_type_decl type_decl tvmap =
    let normalized_type_params =
      type_params type_decl.Types.type_params tvmap
    in
    let normalized_type_kind = type_kind type_decl.type_kind tvmap in
    let normalized_type_manifest =
      normalize_type_manifest type_decl.type_manifest tvmap
    in
    {
      type_decl with
      type_params = normalized_type_params;
      type_kind = normalized_type_kind;
      type_manifest = normalized_type_manifest;
    }
  in
  let ref_tvmap, cur_tvmap =
    build_tvmaps reference.type_params current.type_params
      (String_map.empty, String_map.empty)
  in
  ( normalize_type_decl reference ref_tvmap,
    normalize_type_decl current cur_tvmap )

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
