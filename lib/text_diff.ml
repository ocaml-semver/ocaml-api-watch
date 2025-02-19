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

let lbl_to_line ld =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.label formatter ld;
  Format.pp_print_flush formatter ();
  String.map (function '\n' -> ' ' | c -> c) (Buffer.contents buf)

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
  let indentation = String.make n ' ' in
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
  | Modified { type_kind; type_privacy; type_manifest; type_params } ->
      process_modified_type_diff type_diff.tname type_kind type_privacy
        type_manifest type_params
  | Added td -> process_atomic_diff (Added td) type_diff.tname td_to_lines
  | Removed td -> process_atomic_diff (Removed td) type_diff.tname td_to_lines

and process_modified_type_diff name type_kind type_privacy type_manifest
    type_params =
  let type_header_diff =
    process_type_header_diff name type_privacy type_manifest type_params
      type_kind
  in
  let type_kind_diff = process_type_kind_diff type_kind in
  order_type_diffs type_header_diff type_kind_diff

and order_type_diffs type_header_diff type_kind_diff =
  match (type_header_diff, type_kind_diff) with
  | `Same _, (`Same _ | `None) -> assert false
  | `Same type_header, `Atomic_change type_kind_change
  | `Same type_header, `Compound_change type_kind_change ->
      type_header @ type_kind_change
  | `Atomic_change type_header_change, `None -> type_header_change
  | `Atomic_change type_header_change, `Same type_kind ->
      type_header_change @ type_kind
  | `Atomic_change type_header_change, `Atomic_change type_kind_change ->
      List.hd type_header_change :: List.hd type_kind_change
      :: List.tl type_header_change
      @ List.tl type_kind_change
  | `Atomic_change type_header_change, `Compound_change type_kind_change ->
      type_header_change @ type_kind_change

and process_type_header_diff name type_privacy type_manifest type_params
    type_kind =
  match (type_privacy, type_manifest, type_params, type_kind) with
  | ( Same private_flag,
      Same te_opt,
      Same same_params,
      Different
        ( Record_tk _ | Variant_tk _
        | Atomic_tk
            {
              reference = Type_record _ | Type_variant _ | Type_open;
              current = Type_record _ | Type_variant _ | Type_open;
            } ) ) ->
      `Same
        [
          Same (type_header_to_line name private_flag te_opt same_params false);
        ]
  | type_privacy_diff, type_manifest_diff, type_params_diff, type_kind_diff ->
      let ref_private_flag, cur_private_flag =
        match type_privacy_diff with
        | Same private_flag -> (private_flag, private_flag)
        | Different privacy_diff -> pair_of_privacy_diff privacy_diff
      in
      let ref_manifest, cur_manifest =
        match type_manifest_diff with
        | Same te_opt -> (te_opt, te_opt)
        | Different manifest_diff -> pair_of_manifest_diff manifest_diff
      in
      let ref_abstract, cur_abstract =
        match type_kind_diff with
        | Same (Type_abstract _) -> (true, true)
        | Different (Atomic_tk { reference = Type_abstract _; current = _ }) ->
            (true, false)
        | Different (Atomic_tk { reference = _; current = Type_abstract _ }) ->
            (false, true)
        | _ -> (false, false)
      in
      let ref_params, cur_params =
        match type_params_diff with
        | Same same_params -> (same_params, same_params)
        | Different changed_params ->
            List.fold_right
              (fun p (ref_params, cur_params) ->
                match p with
                | Diff.Same same_param ->
                    (same_param :: ref_params, same_param :: cur_params)
                | Different (Diff.Added_tp param) ->
                    (ref_params, param :: cur_params)
                | Different (Diff.Removed_tp param) ->
                    (param :: ref_params, cur_params))
              changed_params ([], [])
      in
      let orig =
        [
          type_header_to_line name ref_private_flag ref_manifest ref_params
            ref_abstract;
        ]
      in
      let new_ =
        [
          type_header_to_line name cur_private_flag cur_manifest cur_params
            cur_abstract;
        ]
      in
      `Atomic_change [ Change { orig; new_ = [] }; Change { orig = []; new_ } ]

and pair_of_privacy_diff privacy_diff =
  match privacy_diff with
  | Added_p -> (Public, Private)
  | Removed_p -> (Private, Public)

and pair_of_manifest_diff manifest_diff =
  match manifest_diff with
  | Added te -> (None, Some te)
  | Removed te -> (Some te, None)
  | Modified { reference; current } -> (Some reference, Some current)

and type_header_to_line name private_flag type_expr_opt type_params abstract =
  let private_flag_str = string_of_private_flag private_flag in
  let manifest_str = string_of_manifest type_expr_opt in
  let type_params_str = string_of_type_params type_params in
  let equal_sign =
    if abstract && private_flag_str = "" && manifest_str = "" then "" else " ="
  in
  Printf.sprintf "type %s%s%s%s%s" type_params_str name equal_sign
    private_flag_str manifest_str

and string_of_private_flag private_flag =
  match private_flag with Public -> "" | Private -> " private"

and string_of_manifest manifest =
  match manifest with None -> "" | Some te -> " " ^ typ_expr_to_line te

and string_of_type_params type_params =
  let wrap = match type_params with _ :: _ :: _ -> true | _ -> false in
  (if wrap then "(" else "")
  ^ String.concat ", " (List.map get_type_param_name type_params)
  ^ (if wrap then ") " else "")
  ^ match type_params with [ _ ] -> " " | _ -> ""

and get_type_param_name param =
  match Types.get_desc param with
  | Types.Tvar (Some name) -> "'" ^ name
  | _ -> assert false

and process_type_kind_diff type_kind =
  match type_kind with
  | Same same_type_kind -> (
      match same_type_kind with
      | Type_abstract _ -> `None
      | _ ->
          `Same
            [
              Same
                (String.concat "\n"
                   (Option.value
                      (type_kind_to_lines same_type_kind)
                      ~default:[]));
            ])
  | Different (Record_tk changed_lst) ->
      `Compound_change
        (process_modified_record_type_diff ~indent_amount:1 changed_lst)
  | Different (Variant_tk changed_lst) ->
      `Compound_change (process_modified_variant_type_diff changed_lst)
  | Different (Atomic_tk { reference; current }) ->
      `Atomic_change
        [
          Change
            {
              orig = Option.value (type_kind_to_lines reference) ~default:[];
              new_ = [];
            };
          Change
            {
              orig = [];
              new_ = Option.value (type_kind_to_lines current) ~default:[];
            };
        ]

and type_kind_to_lines type_kind : string list option =
  match type_kind with
  | Type_record (lbl_lst, _) ->
      Some
        [
          indent_s 1 "{ "
          ^ (List.map lbl_to_line lbl_lst |> String.concat " ")
          ^ " }";
        ]
  | Type_variant (cstr_lst, _) ->
      Some
        (List.map
           (fun s -> indent_s 1 s)
           (List.concat_map (fun cstr -> cstr_to_lines cstr) cstr_lst))
  | Type_abstract _ -> None
  | Type_open -> Some [ indent_s 1 ".." ]

and indent_s n s =
  let indentation = String.make n ' ' in
  indentation ^ s

and process_modified_record_type_diff ~indent_amount diff =
  let changes = process_modified_labels diff in
  indent indent_amount (Same "{")
  :: indent (indent_amount + 2) (Same "...")
  :: List.map (fun c -> indent (indent_amount + 2) c) changes
  @ [ indent indent_amount (Same "}") ]

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
  [ indent 1 (Same "...") ] @ List.map (fun c -> indent 1 c) changes

and process_modified_cstrs (lbls_diffs : Diff.constructor_ list) =
  List.map
    (fun (cstr_diff : Diff.constructor_) -> process_cstr_diff cstr_diff)
    lbls_diffs
  |> List.flatten

and process_cstr_diff cstr_diff =
  match cstr_diff.csdiff with
  | Added cd -> process_cstr_atomic_diff (Added cd)
  | Removed cd -> process_cstr_atomic_diff (Removed cd)
  | Modified (Atomic_c cd) -> process_cstr_atomic_diff (Modified cd)
  | Modified (Tuple_c tc_lst) ->
      process_modified_tuple_type_diff cstr_diff.csname tc_lst
  | Modified (Record_c rc_lst) ->
      Same ("| " ^ cstr_diff.csname ^ " of")
      :: process_modified_record_type_diff ~indent_amount:2 rc_lst

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
           | Same t -> (Some t, Some t)
           | Different (Added t) -> (None, Some t)
           | Different (Removed t) -> (Some t, None)
           | Different (Modified { reference; current }) ->
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
