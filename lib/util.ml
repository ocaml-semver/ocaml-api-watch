open Types

let get_type_param_name param =
  match get_desc param with Tvar (Some name) -> name | _ -> assert false

let gen_unique_type_var_name =
  let counter = ref 1 in
  fun reset ->
    if reset then counter := 0 else ();
    let unique_name = Printf.sprintf "t%d" !counter in
    counter := !counter + 1;
    unique_name

let rec build_tvmaps ref_type_params cur_type_params (ref_tvmap, cur_tvmap) =
  match (ref_type_params, cur_type_params) with
  | [], [] ->
      let _ = gen_unique_type_var_name true in
      (ref_tvmap, cur_tvmap)
  | ref_type_param :: ref_type_params', [] ->
      build_tvmaps ref_type_params' []
        ( String_map.add
            (get_type_param_name ref_type_param)
            (gen_unique_type_var_name false)
            ref_tvmap,
          cur_tvmap )
  | [], cur_type_param :: cur_type_params' ->
      build_tvmaps [] cur_type_params'
        ( ref_tvmap,
          String_map.add
            (get_type_param_name cur_type_param)
            (gen_unique_type_var_name false)
            cur_tvmap )
  | ref_type_param :: ref_type_params', cur_type_param :: cur_type_params' ->
      let unique_type_var_name = gen_unique_type_var_name false in
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

let rec normalize_type_expr tvmap type_expr =
  match get_desc type_expr with
  | Tvar None -> type_expr
  | Tvar (Some name) ->
      create_type_expr (Tvar (Some (String_map.find name tvmap))) type_expr
  | Tarrow (arg_label, te1, te2, commutable) ->
      create_type_expr
        (Tarrow
           ( arg_label,
             normalize_type_expr tvmap te1,
             normalize_type_expr tvmap te2,
             commutable ))
        type_expr
  | Ttuple te_list ->
      create_type_expr
        (Ttuple (List.map (normalize_type_expr tvmap) te_list))
        type_expr
  | Tconstr (path, te_list, abbrev_memo) ->
      create_type_expr
        (Tconstr
           (path, List.map (normalize_type_expr tvmap) te_list, abbrev_memo))
        type_expr
  | Tobject (te, funs) ->
      create_type_expr (Tobject (normalize_type_expr tvmap te, funs)) type_expr
  | Tfield (name, kind, te1, te2) ->
      create_type_expr
        (Tfield
           ( name,
             kind,
             normalize_type_expr tvmap te1,
             normalize_type_expr tvmap te2 ))
        type_expr
  | Tnil -> type_expr
  | Tsubst (te1, te2_opt) ->
      create_type_expr
        (Tsubst
           ( normalize_type_expr tvmap te1,
             Option.map (normalize_type_expr tvmap) te2_opt ))
        type_expr
  | _ -> type_expr

let normalize_type_params type_params tvmap =
  List.map (fun type_param -> normalize_type_expr tvmap type_param) type_params

let normalize_type_kind type_kind tvmap =
  match type_kind with
  | Type_record (lbls, x) ->
      let normalized_lbls =
        List.map
          (fun lbl ->
            { lbl with ld_type = normalize_type_expr tvmap lbl.ld_type })
          lbls
      in
      Type_record (normalized_lbls, x)
  | Type_variant (cstrs, x) ->
      let normalized_cstrs =
        List.map
          (fun cstr ->
            match cstr.cd_args with
            | Cstr_tuple type_exprs ->
                let normalized_type_exprs =
                  List.map (normalize_type_expr tvmap) type_exprs
                in
                { cstr with cd_args = Cstr_tuple normalized_type_exprs }
            | Cstr_record lbls ->
                let normalized_lbls =
                  List.map
                    (fun lbl ->
                      {
                        lbl with
                        ld_type = normalize_type_expr tvmap lbl.ld_type;
                      })
                    lbls
                in
                { cstr with cd_args = Cstr_record normalized_lbls })
          cstrs
      in
      Type_variant (normalized_cstrs, x)
  | Type_abstract _ -> type_kind
  | Type_open -> type_kind

and normalize_type_manifest type_manifest tvmap =
  Option.map (normalize_type_expr tvmap) type_manifest

let normalize_type_decls ref_type_decl cur_type_decl =
  let normalize_type_decl type_decl tvmap =
    let normalized_type_params =
      normalize_type_params type_decl.Types.type_params tvmap
    in
    let normalized_type_kind = normalize_type_kind type_decl.type_kind tvmap in
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
    build_tvmaps ref_type_decl.type_params cur_type_decl.type_params
      (String_map.empty, String_map.empty)
  in
  ( normalize_type_decl ref_type_decl ref_tvmap,
    normalize_type_decl cur_type_decl cur_tvmap )
