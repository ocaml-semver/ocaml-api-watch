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

let cstr_to_line ld =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.constructor formatter ld;
  Format.pp_print_flush formatter ();
  String.map (function '\n' -> ' ' | c -> c) (Buffer.contents buf)

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
  | Diff.Modified
      (Compound_tm
        {
          type_kind_mismatch = Some (Record_mismatch change_lst);
          type_privacy_mismatch;
        }) ->
      let s = string_of_type_privacy_mismatch type_privacy_mismatch in
      Same ("type " ^ type_diff.tname ^ " = " ^ s ^ "{")
      :: process_modified_record_type_diff ~indent_n:2 change_lst
      @ [ Same "}" ]
  | Diff.Modified
      (Compound_tm
        {
          type_kind_mismatch = Some (Variant_mismatch change_lst);
          type_privacy_mismatch;
        }) ->
      let s = string_of_type_privacy_mismatch type_privacy_mismatch in
      Same ("type " ^ type_diff.tname ^ " = " ^ s)
      :: process_modified_variant_type_diff change_lst
  | Diff.Modified
      (Compound_tm
        {
          type_kind_mismatch = Some (Atomic_mismatch { reference; current });
          type_privacy_mismatch;
        }) ->
      process_atomic_type_kind_diff type_diff.tname type_privacy_mismatch
        reference current
  | Diff.Modified
      (Compound_tm
        { type_kind_mismatch = None; type_privacy_mismatch = Either.Right pm })
    ->
      let privacy_str = string_of_type_privacy_mismatch (Either.Right pm) in
      [ Same (Printf.sprintf "type %s = %s...." type_diff.tname privacy_str) ]
  | Diff.Modified (Atomic_tm mods) ->
      process_atomic_diff (Diff.Modified mods) type_diff.tname td_to_lines
  | Diff.Added td ->
      process_atomic_diff (Diff.Added td) type_diff.tname td_to_lines
  | Diff.Removed td ->
      process_atomic_diff (Diff.Removed td) type_diff.tname td_to_lines
  | _ -> assert false

and string_of_type_privacy_mismatch type_privacy_mismatch =
  match type_privacy_mismatch with
  | Either.Left Asttypes.Public -> ""
  | Either.Left Asttypes.Private -> "private "
  | Either.Right (Diff.Added_p Asttypes.Private) -> "+private "
  | Either.Right (Diff.Removed_p Asttypes.Private) -> "-private "
  | _ -> assert false

and process_atomic_type_kind_diff name privacy reference current =
  [
    Change
      {
        orig = type_kind_to_lines name privacy reference;
        new_ = type_kind_to_lines name privacy current;
      };
  ]

and type_kind_to_lines name privacy type_kind =
  let privacy_str = string_of_type_privacy_mismatch privacy in
  match type_kind with
  | Types.Type_record (lbl_lst, _) ->
      [
        Printf.sprintf "type %s = %s{ " name privacy_str
        ^ (List.map (fun lbl -> lbl_to_line lbl) lbl_lst |> String.concat " ; ")
        ^ " }";
      ]
  | Types.Type_variant (cstr_lst, _) ->
      [
        Printf.sprintf "type %s = %s " name privacy_str
        ^ (List.map (fun cstr -> cstr_to_line cstr) cstr_lst
          |> String.concat " | ");
      ]
  | Types.Type_abstract _ -> [ Printf.sprintf "type %s" name ]
  | Types.Type_open -> [ Printf.sprintf "type %s = %s.." name privacy_str ]

and process_modified_record_type_diff ~indent_n diff =
  let changes = process_modified_labels diff in
  [ indent indent_n (Same "...") ]
  @ List.map (fun c -> indent indent_n c) changes

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
  | Diff.Added cd -> process_cstr_atomic_diff (Diff.Added cd)
  | Diff.Removed cd -> process_cstr_atomic_diff (Diff.Removed cd)
  | Diff.Modified (Diff.Atomic_c cd) ->
      process_cstr_atomic_diff (Diff.Modified cd)
  | Diff.Modified (Diff.Tuple_c tc_lst) ->
      process_modified_tuple_type_diff cstr_diff.csname tc_lst
  | Diff.Modified (Diff.Record_c rc_lst) ->
      Same ("| " ^ cstr_diff.csname ^ " of {")
      :: process_modified_record_type_diff ~indent_n:3 rc_lst
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
