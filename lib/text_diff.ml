type conflict2 = { orig : string list; new_ : string list }
type hunk = Change of conflict2 | Same of string
type t = hunk list String_map.t
type printer = { same : string Fmt.t; diff : conflict2 Fmt.t }

let printer ~same ~diff = { same; diff }

let git_printer =
  {
    same = (fun ppf -> Fmt.pf ppf " %s\n");
    diff =
      (fun ppf { orig; new_ } ->
        List.iter (fun line -> Fmt.pf ppf "-%s\n" line) orig;
        List.iter (fun line -> Fmt.pf ppf "+%s\n" line) new_);
  }

let pp_ diff_printer =
  let pp_dh ppf dh =
    match dh with
    | Change c -> diff_printer.diff ppf c
    | Same s -> diff_printer.same ppf s
  in
  Fmt.list ~sep:Fmt.nop pp_dh

let vd_to_lines name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.value_description (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let td_to_lines name td =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.type_declaration (Ident.create_local name) formatter td;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let lbl_to_lines ld =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.label formatter ld;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let typ_expr_to_line typ_exp =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.type_expr formatter typ_exp;
  Format.pp_print_flush formatter ();
  String.map (function '\n' -> ' ' | c -> c) (Buffer.contents buf)

let cstr_to_lines ld =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.constructor formatter ld;
  Format.pp_print_flush formatter ();
  CCString.lines ("| " ^ Buffer.contents buf)

let md_to_lines name md =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.modtype formatter Types.(md.md_type);
  Format.pp_print_flush formatter ();
  let module_str = "module " ^ name ^ ": " ^ Buffer.contents buf in
  CCString.lines module_str

let mtd_to_lines name mtd =
  match Types.(mtd.mtd_type) with
  | Some m ->
      let buf = Buffer.create 256 in
      let formatter = Format.formatter_of_buffer buf in
      Printtyp.modtype formatter m;
      Format.pp_print_flush formatter ();
      let module_type_str =
        "module type " ^ name ^ " = " ^ Buffer.contents buf
      in
      CCString.lines module_type_str
  | None ->
      let abstract_module_type_str = "module type " ^ name in
      CCString.lines abstract_module_type_str

let cd_to_lines name cd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.class_declaration (Ident.create_local name) formatter cd;
  Format.pp_print_flush formatter ();
  let class_str = Buffer.contents buf in
  CCString.lines class_str

let ctd_to_lines name cd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.cltype_declaration (Ident.create_local name) formatter cd;
  Format.pp_print_flush formatter ();
  let class_str = Buffer.contents buf in
  CCString.lines class_str

let indent n h =
  let indentation = String.init n (fun _ -> ' ') in
  match h with
  | Same s -> Same (indentation ^ s)
  | Change { orig; new_ } ->
      Change
        {
          orig = List.map (fun s -> indentation ^ s) orig;
          new_ = List.map (fun s -> indentation ^ s) new_;
        }

let process_atomic_diff (diff : (_, _ Diff.atomic_modification) Diff.t) name
    to_lines =
  match diff with
  | Added item -> [ Change { orig = []; new_ = to_lines name item } ]
  | Removed item -> [ Change { orig = to_lines name item; new_ = [] } ]
  | Modified { reference; current } ->
      [
        Change { orig = to_lines name reference; new_ = to_lines name current };
      ]

let rec process_type_diff (type_diff : Diff.type_) =
  match type_diff.tdiff with
  | Diff.Modified { type_kind; type_privacy; type_manifest } ->
      process_modified_type type_diff.tname type_kind type_privacy type_manifest
  | Diff.Added td ->
      process_atomic_diff (Diff.Added td) type_diff.tname td_to_lines
  | Diff.Removed td ->
      process_atomic_diff (Diff.Removed td) type_diff.tname td_to_lines

and process_modified_type name type_kind type_privacy type_manifest =
  let type_header_diff = process_type_header_diff name type_privacy type_manifest in
  let type_kind_diff = process_type_kind_diff type_kind in
  order_type_diffs type_header_diff type_kind_diff

and process_type_header_diff name type_privacy type_manifest =
  match type_privacy, type_manifest with
  | (Either.Left private_flag, Either.Left te_opt) -> 
      [ Same type_header_to_line name private_flag te_opt ]
  | type_privacy_diff, type_manifest_diff -> (
      let (ref_private_flag, cur_private_flag),
          (ref_te_opt, cur_te_opt) = 
        match type_privacy_diff, type_manifest_diff with
        | Either.Left private_flag, Either.Right manifest_diff ->
          (private_flag, private_flag),
          manifest_to_pair manifest_diff
        | Either.Right privacy_diff, Either.Left type_manifest_same ->
          privacy_to_pair privacy_diff,
          (type_manifest_same, type_manifest_same)
        | Either.Right privacy_diff, Either.Right manifest_diff ->
          privacy_to_pair privacy_diff,
          manifest_to_pair manifest_diff
        | _ -> assert false
      in
      [
        Change { orig = type_header_to_line name ref_private_flag ref_te_opt; new_ = [] };
        Change { orig = []; new_ = type_header_to_line name cur_private_flag cur_te_opt }
      ]
    )

and privacy_diff_to_pair privacy_diff =
  match privacy_diff with
  | Added_p -> (Asttypes.Public, Asttypes.Private)
  | Removed_p -> (Asttypes.Private, Asttypes.Public)

and manifest_to_pair manifest_diff =
  match manifest_diff with
  | Added te -> (None, Some te)
  | Removed te -> (Some te, None)
  | Modified ref_te, cur_te -> (Some ref_te, Some cur_te)

and type_header_to_line name private_flag type_expr_opt =
  let private_flag_str = string_of_private_flag private_flag in
  let manifest_str = string_of_manifest type_expr_opt in
  Printf.sprintf "type %s=%s%s" name private_flag_str _mainfest_str

and string_of_private_flag private_flag =
  match private_flag with
  | Asttypes.Public -> ""
  | Asttypes.Private -> "private"

and string_of_manifest manifest =
  match manifest with
  | None -> ""
  | Some te -> typ_expr_to_line te

and process_type_kind_diff type_kind =
  match type_kind with
  | Either.Left same_type_kind -> [ Same type_kind_to_lines same_type_kind ]
  | Either.Right (Record_tk changed_lst) -> 
    process_modified_record_type changed_lst
  | Either.Right (Variant_tk changed_lst) ->
    process_modified_variant_type changed_lst
  | Either.Right (Atomic_tk { reference; current }) ->
    [ 
      Change { orig = type_kind_to_lines reference; new_ = [] };
      Change { orig = []; new_ = type_kind_to_lines current }
    ]

and process_modified_type name type_kind type_privacy type_manifest =
  match (type_kind, type_privacy, type_manifest) with
  | (Either.Right (Record_tk chnage_lst), )
  | ( Some (Record_tk change_lst),
      (Either.Left _ as privacy),
      (Either.Left _ as manifest) ) ->
      Same (type_header_to_line `Ref name privacy manifest)
      :: process_modified_record_type_diff change_lst
  | ( Some (Variant_tk change_lst),
      (Either.Left _ as privacy),
      (Either.Left _ as manifest) ) ->
      Same
        (Printf.sprintf "type %s =%s%s " name
           (type_privacy_diff_to_line `Ref privacy)
           (type_manifest_diff_to_line `Ref manifest))
      :: process_modified_variant_type_diff change_lst
  | Some (Record_tk change_lst), privacy_diff, manifest_diff ->
      Change
        {
          orig = [ type_header_to_line `Ref name privacy_diff manifest_diff ];
          new_ = [ type_header_to_line `Curr name privacy_diff manifest_diff ];
        }
      :: process_modified_record_type_diff change_lst
  | Some (Variant_tk change_lst), privacy_diff, manifest_diff ->
      Change
        {
          orig = [ type_header_to_line `Ref name privacy_diff manifest_diff ];
          new_ = [ type_header_to_line `Curr name privacy_diff manifest_diff ];
        }
      :: process_modified_variant_type_diff change_lst
  | Some (Atomic_tk { reference; current }), privacy_diff, manifest_diff ->
      [
        Change
          {
            orig =
              type_kind_to_lines `Ref name reference privacy_diff manifest_diff;
            new_ =
              type_kind_to_lines `Curr name current privacy_diff manifest_diff;
          };
      ]
  | None, privacy_diff, manifest_diff ->
      let privacy_diff_str =
        match privacy_diff with
        | Either.Left Asttypes.Public -> ""
        | Either.Left Asttypes.Private -> " private"
        | Either.Right Diff.Added_p -> " +private"
        | Either.Right Diff.Removed_p -> " -private"
      in
      let manifest_diff_str =
        match manifest_diff with
        | Either.Left None -> ""
        | Either.Left (Some te) -> " " ^ typ_expr_to_line te
        | Either.Right (Diff.Added te) -> " +" ^ typ_expr_to_line te
        | Either.Right (Diff.Removed te) -> " -" ^ typ_expr_to_line te
        | Either.Right (Diff.Modified { reference; current }) ->
            " -" ^ typ_expr_to_line reference ^ " +" ^ typ_expr_to_line current
      in
      [
        Same
          (Printf.sprintf "type %s = %s%s" name privacy_diff_str
             manifest_diff_str);
      ]

and type_header_to_line src name privacy_diff manifest_diff =
  Printf.sprintf "type %s =%s%s" name
    (type_privacy_diff_to_line src privacy_diff)
    (type_manifest_diff_to_line src manifest_diff)

and type_privacy_diff_to_line (src : [ `Ref | `Curr ]) diff =
  match diff with
  | Either.Left Asttypes.Public -> ""
  | Either.Left Asttypes.Private -> " private"
  | Either.Right Diff.Added_p -> (
      match src with `Ref -> "" | `Curr -> " private")
  | Either.Right Diff.Removed_p -> (
      match src with `Ref -> " private" | `Curr -> "")

and type_manifest_diff_to_line (src : [ `Ref | `Curr ])
    (diff :
      ( Types.type_expr option,
        (Types.type_expr, Types.type_expr Diff.atomic_modification) Diff.t )
      Either.t) =
  match (diff, src) with
  | Either.Left None, _ -> ""
  | Either.Left (Some te), _ -> " " ^ typ_expr_to_line te
  | Either.Right (Diff.Added _), `Ref | Either.Right (Diff.Removed _), `Curr ->
      ""
  | Either.Right (Diff.Added te), `Curr | Either.Right (Diff.Removed te), `Ref
    ->
      " " ^ typ_expr_to_line te
  | Either.Right (Diff.Modified mods), `Ref ->
      " " ^ typ_expr_to_line mods.reference
  | Either.Right (Diff.Modified mods), `Curr ->
      " " ^ typ_expr_to_line mods.current

and indent_s n s =
  let indentation = String.init n (fun _ -> ' ') in
  indentation ^ s

and type_kind_to_lines src name type_kind type_privacy_diff type_manifest_diff :
    string list =
  let type_privacy_diff = type_privacy_diff_to_line src type_privacy_diff in
  let type_manifest_diff = type_manifest_diff_to_line src type_manifest_diff in
  let equal_str =
    if type_privacy_diff <> "" || type_manifest_diff <> "" then "=" else ""
  in
  match type_kind with
  | Types.Type_record (lbl_lst, _) ->
      Printf.sprintf "type %s =%s%s" name type_privacy_diff type_manifest_diff
      :: indent_s 2 "{"
      :: List.map
           (fun s -> indent_s 4 s)
           (List.concat_map (fun lbl -> lbl_to_lines lbl) lbl_lst)
      @ [ indent_s 2 "}" ]
  | Types.Type_variant (cstr_lst, _) ->
      Printf.sprintf "type %s =%s%s" name type_privacy_diff type_manifest_diff
      :: List.map
           (fun s -> indent_s 2 s)
           (List.concat_map (fun cstr -> cstr_to_lines cstr) cstr_lst)
  | Types.Type_abstract _ ->
      [ "type t" ^ equal_str ^ type_privacy_diff ^ type_manifest_diff ]
  | Types.Type_open ->
      [ "type t = " ^ type_privacy_diff ^ type_manifest_diff ^ ".." ]

and process_modified_record_type_diff diff =
  let changes = process_modified_labels diff in
  [ indent 2 (Same "{") ]
  @ [ indent 4 (Same "...") ]
  @ List.map (fun c -> indent 4 c) changes
  @ [ indent 2 (Same "}") ]

and process_modified_labels (lbls_diffs : Diff.record_field list) =
  List.map
    (fun (lbl_diff : Diff.record_field) -> process_lbl_diff lbl_diff.rdiff)
    lbls_diffs
  |> List.flatten

and process_lbl_diff
    (diff :
      ( Types.label_declaration,
        Types.label_declaration Diff.atomic_modification )
      Diff.t) =
  match diff with
  | Added item -> [ Change { orig = []; new_ = lbl_to_lines item } ]
  | Removed item -> [ Change { orig = lbl_to_lines item; new_ = [] } ]
  | Modified { reference; current } ->
      [ Change { orig = lbl_to_lines reference; new_ = lbl_to_lines current } ]

and process_modified_variant_type_diff diff =
  let changes = process_modified_cstrs diff in
  [ indent 4 (Same "...") ] @ List.map (fun c -> indent 4 c) changes

and process_modified_cstrs (lbls_diffs : Diff.constructor_ list) =
  List.map
    (fun (cstr_diff : Diff.constructor_) -> process_cstr_diff cstr_diff)
    lbls_diffs
  |> List.flatten

and process_cstr_diff cstr_diff =
  match cstr_diff.csdiff with
  | Diff.Added cd -> process_cstr_atomic_diff (Diff.Added cd)
  | Diff.Removed cd -> process_cstr_atomic_diff (Diff.Removed cd)
  | Diff.Modified (Diff.Atomic_c cd) ->
      process_cstr_atomic_diff (Diff.Modified cd)
  | Diff.Modified (Diff.Tuple_c tc_lst) ->
      process_modified_tuple_type_diff cstr_diff.csname tc_lst
  | Diff.Modified (Diff.Record_c rc_lst) ->
      Same ("| " ^ cstr_diff.csname ^ " of {")
      :: process_modified_record_type_diff rc_lst
      @ [ Same "}" ]

and process_cstr_atomic_diff
    (diff :
      ( Types.constructor_declaration,
        Types.constructor_declaration Diff.atomic_modification )
      Diff.t) =
  match diff with
  | Added item -> [ Change { orig = []; new_ = cstr_to_lines item } ]
  | Removed item -> [ Change { orig = cstr_to_lines item; new_ = [] } ]
  | Modified { reference; current } ->
      [
        Change { orig = cstr_to_lines reference; new_ = cstr_to_lines current };
      ]

and process_modified_tuple_type_diff name diff =
  let starify typ_opts =
    List.filter_map
      (fun t_opt -> Option.map (fun t -> typ_expr_to_line t) t_opt)
      typ_opts
    |> String.concat " * "
  in
  let cstr1_tpl_args, cstr2_tpl_args =
    diff
    |> List.map (fun (c : Diff.tuple_component) ->
           match c with
           | Either.Left t -> (Some t, Some t)
           | Either.Right (Diff.Added t) -> (None, Some t)
           | Either.Right (Diff.Removed t) -> (Some t, None)
           | Either.Right (Diff.Modified { reference; current }) ->
               (Some reference, Some current))
    |> List.split
  in
  [
    Change
      {
        orig = [ Printf.sprintf "| %s of %s" name (starify cstr1_tpl_args) ];
        new_ = [ Printf.sprintf "| %s of %s" name (starify cstr2_tpl_args) ];
      };
  ]

let process_value_diff (val_diff : Diff.value) =
  process_atomic_diff val_diff.vdiff val_diff.vname vd_to_lines

let process_class_diff (class_diff : Diff.class_) =
  process_atomic_diff class_diff.cdiff class_diff.cname cd_to_lines

let process_class_type_diff (class_type_diff : Diff.cltype) =
  process_atomic_diff class_type_diff.ctdiff class_type_diff.ctname ctd_to_lines

let rec process_sig_diff :
    type a. _ -> (string -> a -> string list) -> (a, _) Diff.t * _ -> _ -> _ =
 fun path to_lines ((diff : (a, _) Diff.t), name) acc ->
  match diff with
  | Added curr_mtd ->
      let diff = [ Change { orig = []; new_ = to_lines name curr_mtd } ] in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Removed ref_mtd ->
      let diff = [ Change { orig = to_lines name ref_mtd; new_ = [] } ] in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Modified Diff.Unsupported ->
      String_map.add path
        [ Change { orig = []; new_ = [ "<unsupported change>" ] } ]
        acc
  | Modified (Supported changes) -> signature_changes path changes acc

and process_module_type_diff module_path (module_type_diff : Diff.modtype) acc =
  process_sig_diff module_path mtd_to_lines
    (module_type_diff.mtdiff, module_type_diff.mtname)
    acc

and process_module_diff module_path (module_diff : Diff.module_) acc =
  process_sig_diff module_path md_to_lines
    (module_diff.mdiff, module_diff.mname)
    acc

and signature_changes module_path items acc =
  List.fold_left
    (fun acc' change ->
      match (change : Diff.sig_item) with
      | Value val_diff ->
          let diff = process_value_diff val_diff in
          String_map.update module_path
            (function
              | None -> Some diff | Some existing -> Some (existing @ diff))
            acc'
      | Type type_diff ->
          let diff = process_type_diff type_diff in
          String_map.update module_path
            (function
              | None -> Some diff | Some existing -> Some (existing @ diff))
            acc'
      | Class class_diff ->
          let diff = process_class_diff class_diff in
          String_map.update module_path
            (function
              | None -> Some diff | Some existing -> Some (existing @ diff))
            acc'
      | Classtype class_type_diff ->
          let diff = process_class_type_diff class_type_diff in
          String_map.update module_path
            (function
              | None -> Some diff | Some existing -> Some (existing @ diff))
            acc'
      | Module sub_module_diff ->
          let sub_module_path =
            match sub_module_diff.mdiff with
            | Modified _ -> module_path ^ "." ^ sub_module_diff.mname
            | Added _ | Removed _ -> module_path
          in
          process_module_diff sub_module_path sub_module_diff acc'
      | Modtype sub_module_type_diff ->
          let sub_module_path =
            match sub_module_type_diff.mtdiff with
            | Modified _ -> module_path ^ "." ^ sub_module_type_diff.mtname
            | Added _ | Removed _ -> module_path
          in
          process_module_type_diff sub_module_path sub_module_type_diff acc')
    acc items

and from_diff (diff : Diff.module_) : t =
  process_module_diff diff.mname diff String_map.empty

let gen_pp pp_diff fmt t =
  let print_module_diff module_path diff =
    Format.fprintf fmt "diff module %s:\n" module_path;
    pp_diff fmt diff;
    Format.fprintf fmt "\n"
  in
  String_map.iter print_module_diff t

let pp_diff fmt diff = pp_ git_printer fmt diff
let pp fmt t = gen_pp pp_diff fmt t

module With_colors = struct
  let pp_l fmt ~color ~prefix ~line =
    Format.fprintf fmt "%a%a\n"
      Fmt.(styled color string)
      prefix
      Fmt.(styled color string)
      line

  let pp_add fmt line = pp_l fmt ~color:`Green ~prefix:"+" ~line
  let pp_remove fmt line = pp_l fmt ~color:`Red ~prefix:"-" ~line
  let pp_keep fmt line = Format.fprintf fmt " %s\n" line

  let printer =
    printer ~same:pp_keep ~diff:(fun fmt { orig; new_ } ->
        List.iter (pp_remove fmt) orig;
        List.iter (pp_add fmt) new_)

  let pp_diff fmt diff = pp_ printer fmt diff
  let pp fmt t = gen_pp pp_diff fmt t
end
