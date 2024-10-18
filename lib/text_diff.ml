open Types

type conflict2 = { orig : string list; new_ : string list }
type t = conflict2 list String_map.t
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
  let pp_dh ppf dh = match dh with c -> diff_printer.diff ppf c in
  Fmt.list ~sep:Fmt.nop pp_dh

let vd_to_lines name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.value_description (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let td_to_lines name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.type_declaration (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let md_to_lines name md =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.modtype formatter md.md_type;
  Format.pp_print_flush formatter ();
  let module_str = "module " ^ name ^ ": " ^ Buffer.contents buf in
  CCString.lines module_str

let mtd_to_lines name mtd =
  match mtd.mtd_type with
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

let process_diff (diff : (_, _ Diff.atomic_modification) Diff.t) name to_lines =
  match diff with
  | Added item -> [ { orig = []; new_ = to_lines name item } ]
  | Removed item -> [ { orig = to_lines name item; new_ = [] } ]
  | Modified { reference; current } ->
      [ { orig = to_lines name reference; new_ = to_lines name current } ]

let cd_to_lines name cd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.class_declaration (Ident.create_local name) formatter cd;
  Format.pp_print_flush formatter ();
  let class_str = "class " ^ name ^ ": " ^ Buffer.contents buf in
  CCString.lines class_str

let process_diff (diff : (_, _ Diff.atomic_modification) Diff.t) name to_lines =
  match diff with
  | Added item -> [ { orig = []; new_ = to_lines name item } ]
  | Removed item -> [ { orig = to_lines name item; new_ = [] } ]
  | Modified { reference; current } ->
      [ { orig = to_lines name reference; new_ = to_lines name current } ]

let process_value_diff (val_diff : Diff.value) =
  process_diff val_diff.vdiff val_diff.vname vd_to_lines

let process_type_diff (type_diff : Diff.type_) =
  process_diff type_diff.tdiff type_diff.tname td_to_lines

let process_class_diff (class_diff : Diff.class_) =
  match class_diff.cdiff with
  | Added cd -> [ { orig = []; new_ = cd_to_lines class_diff.cname cd } ]
  | Removed cd -> [ { orig = cd_to_lines class_diff.cname cd; new_ = [] } ]
  | Modified _ -> []

let rec process_sig_diff :
    type a. _ -> (string -> a -> string list) -> (a, _) Diff.t * _ -> _ -> _ =
 fun path to_lines ((diff : (a, _) Diff.t), name) acc ->
  match diff with
  | Added curr_mtd ->
      let diff = [ { orig = []; new_ = to_lines name curr_mtd } ] in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Removed ref_mtd ->
      let diff = [ { orig = to_lines name ref_mtd; new_ = [] } ] in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Modified Diff.Unsupported ->
      String_map.add path
        [ { orig = []; new_ = [ "<unsupported change>" ] } ]
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
