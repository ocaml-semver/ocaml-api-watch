type conflict2 = { orig : string list; new_ : string list }
type hunk = Change of conflict2 | Same of string
type t = hunk list String_map.t
type printer = { same : string Fmt.t; diff : hunk Fmt.t }

let printer ~same ~diff = { same; diff }

let git_diff_printer ppf c =
  match c with
  | Change { orig; new_ } ->
      List.iter (fun line -> Fmt.pf ppf "-%s\n" line) orig;
      List.iter (fun line -> Fmt.pf ppf "+%s\n" line) new_
  | Same common -> Fmt.pf ppf "%s\n" common

let git_printer =
  { same = (fun ppf -> Fmt.pf ppf " %s\n"); diff = git_diff_printer }

let pp_ diff_printer =
  let pp_dh ppf dh = match dh with c -> diff_printer.diff ppf c in
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

let process_atomic_diff (diff : (_, _ Diff.atomic_modification) Diff.t) name
    to_lines =
  match diff with
  | Added item -> [ Change { orig = []; new_ = to_lines name item } ]
  | Removed item -> [ Change { orig = to_lines name item; new_ = [] } ]
  | Modified { reference; current } ->
      [
        Change { orig = to_lines name reference; new_ = to_lines name current };
      ]

let process_value_diff (val_diff : Diff.value) =
  process_atomic_diff val_diff.vdiff val_diff.vname vd_to_lines

let process_lbl_diff
    (diff :
      ( Types.label_declaration,
        Types.label_declaration Diff.atomic_modification )
      Diff.t) =
  match diff with
  | Added item -> [ Change { orig = []; new_ = lbl_to_lines item } ]
  | Removed item -> [ Change { orig = lbl_to_lines item; new_ = [] } ]
  | Modified { reference; current } ->
      [ Change { orig = lbl_to_lines reference; new_ = lbl_to_lines current } ]

let rec process_type_diff (type_diff : Diff.type_) =
  match type_diff.tdiff with
  | Diff.Modified (Compound change_lst) ->
      process_modified_record_type_diff type_diff.tname change_lst
  | Diff.Modified (Atomic mods) ->
      process_atomic_diff (Diff.Modified mods) type_diff.tname td_to_lines
  | Diff.Added td ->
      process_atomic_diff (Diff.Added td) type_diff.tname td_to_lines
  | Diff.Removed td ->
      process_atomic_diff (Diff.Removed td) type_diff.tname td_to_lines

and process_modified_record_type_diff name diff =
  let indent_lbl c =
    match c with
    | Same c -> Same ("  " ^ c)
    | Change { orig; new_ } ->
        Change
          {
            orig = List.map (fun s -> "  " ^ s) orig;
            new_ = List.map (fun s -> "  " ^ s) new_;
          }
  in
  let changes = process_changed_labels diff in
  [ Same ("type " ^ name ^ " = {") ]
  @ [ indent_lbl (Same "...") ]
  @ List.map (fun c -> indent_lbl c) changes
  @ [ Same "}" ]

and process_changed_labels (lbls_diffs : Diff.record_field list) =
  List.flatten
    (List.map
       (fun (lbl_diff : Diff.record_field) -> process_lbl_diff lbl_diff.ldiff)
       lbls_diffs)

let process_class_diff (class_diff : Diff.class_) =
  match class_diff.cdiff with
  | Added cd -> [ Change { orig = []; new_ = cd_to_lines class_diff.cname cd } ]
  | Removed cd ->
      [ Change { orig = cd_to_lines class_diff.cname cd; new_ = [] } ]
  | Modified _ -> []

let process_class_type_diff (class_type_diff : Diff.cltype) =
  match class_type_diff.ctdiff with
  | Added ctd ->
      [ Change { orig = []; new_ = ctd_to_lines class_type_diff.ctname ctd } ]
  | Removed ctd ->
      [ Change { orig = ctd_to_lines class_type_diff.ctname ctd; new_ = [] } ]
  | Modified _ -> []

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
    printer ~same:pp_keep ~diff:(fun fmt c ->
        match c with
        | Change { orig; new_ } ->
            List.iter (pp_remove fmt) orig;
            List.iter (pp_add fmt) new_
        | Same common -> (pp_keep fmt) common)

  let pp_diff fmt diff = pp_ printer fmt diff
  let pp fmt t = gen_pp pp_diff fmt t
end
