type inline_conflict = { iorig : string option; inew : string option }
type inline_hunk = Icommon of string | Iconflict of inline_conflict
type line_conflict = { orig : string list; new_ : string list }

type hunk =
  | Common of string list
  | Line_conflict of line_conflict
  | Inline_hunks of inline_hunk list

type t = hunk list String_map.t

type printer = {
  common : string list Fmt.t;
  line_conflict : line_conflict Fmt.t;
  inline_hunks : inline_hunk list Fmt.t;
}

let pp_inline_hunks ~src ~pp_inline_conflict ppf ihunks =
  (match src with `Orig -> Fmt.pf ppf "-" | `New -> Fmt.pf ppf "+");
  List.iter
    (fun ihunk ->
      match (src, ihunk) with
      | _, Icommon s -> Fmt.string ppf s
      | `Orig, Iconflict { iorig; _ } ->
          Fmt.option ~none:Fmt.nop pp_inline_conflict ppf iorig
      | `New, Iconflict { inew; _ } ->
          Fmt.option ~none:Fmt.nop pp_inline_conflict ppf inew)
    ihunks;
  Fmt.pf ppf "\n"

let line_git_printer =
  {
    common =
      (fun ppf lines -> List.iter (fun line -> Fmt.pf ppf " %s\n" line) lines);
    line_conflict =
      (fun ppf { orig; new_ } ->
        List.iter (fun line -> Fmt.pf ppf "-%s\n" line) orig;
        List.iter (fun line -> Fmt.pf ppf "+%s\n" line) new_);
    inline_hunks =
      (fun ppf ihunks ->
        pp_inline_hunks ~src:`Orig ~pp_inline_conflict:Fmt.string ppf ihunks;
        pp_inline_hunks ~src:`New ~pp_inline_conflict:Fmt.string ppf ihunks);
  }

let printer ~common ~line_conflict ~inline_hunks =
  { common; line_conflict; inline_hunks }

let pp_ diff_printer =
  let pp_dh ppf dh =
    match dh with
    | Common c -> diff_printer.common ppf c
    | Line_conflict lc -> diff_printer.line_conflict ppf lc
    | Inline_hunks ihs -> diff_printer.inline_hunks ppf ihs
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

let lbl_to_line ld =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.label formatter ld;
  Format.pp_print_flush formatter ();
  String.map (function '\n' -> ' ' | c -> c) (Buffer.contents buf)

let type_expr_to_string typ_exp =
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

let process_atomic_diff
    (diff : (_, _ Stddiff.atomic_modification) Stddiff.entry) name to_lines =
  match diff with
  | Added item -> [ Line_conflict { orig = []; new_ = to_lines name item } ]
  | Removed item -> [ Line_conflict { orig = to_lines name item; new_ = [] } ]
  | Modified { reference; current } ->
      [
        Line_conflict
          { orig = to_lines name reference; new_ = to_lines name current };
      ]

let process_entry ~entry_to_string ~process_modification ~name diff =
  match (diff : (_, _) Stddiff.entry) with
  | Added item -> process_atomic_diff (Added item) name entry_to_string
  | Removed item -> process_atomic_diff (Removed item) name entry_to_string
  | Modified entry_change -> process_modification name entry_change

let rec process_type_diff (type_diff : Diff.type_) =
  process_entry ~entry_to_string:td_to_lines
    ~process_modification:process_modified_type_diff ~name:type_diff.tname
    type_diff.tdiff

and process_modified_type_diff name
    { Diff.type_kind; type_privacy; type_manifest; type_params } =
  let type_header_hunk =
    process_type_header_diff name type_privacy type_manifest type_params
      type_kind
  in
  let type_kind_hunks = process_type_kind_diff type_kind in
  type_header_hunk @ List.map (indent_hunk 2) type_kind_hunks

and indent_hunk amount hunk =
  match hunk with
  | Common lst -> Common (List.map (indent amount) lst)
  | Line_conflict { orig; new_ } ->
      Line_conflict
        {
          orig = List.map (indent amount) orig;
          new_ = List.map (indent amount) new_;
        }
  | Inline_hunks ihunks ->
      Inline_hunks
        (match ihunks with
        | [] -> ihunks
        | ihunk :: ihunks' ->
            (match ihunk with
            | Icommon s -> Icommon (indent amount s)
            | Iconflict { iorig; inew } ->
                Iconflict
                  {
                    iorig = Option.map (indent amount) iorig;
                    inew = Option.map (indent amount) inew;
                  })
            :: ihunks')

and indent amount str =
  let indentation = String.make amount ' ' in
  indentation ^ str

and process_type_header_diff name type_privacy_diff type_manifest_diff
    type_params_diff type_kind_diff =
  let type_hunk = Icommon "type" in
  let type_params_hunks = process_type_params_diff type_params_diff in
  let type_name_hunk = Icommon (" " ^ name) in
  let equal_hunks = process_equal_sign_diff type_manifest_diff type_kind_diff in
  let type_privacy_hunks = process_privacy_diff type_privacy_diff in
  let type_manifest_hunks =
    process_manifest_diff ~paren:false type_manifest_diff
  in
  let type_header_hunks =
    if List.length equal_hunks = 2 then
      List.concat
        [
          [ type_hunk ];
          type_params_hunks;
          [ type_name_hunk ];
          [ List.hd equal_hunks ];
          type_manifest_hunks;
          List.tl equal_hunks;
          type_privacy_hunks;
        ]
    else
      List.concat
        [
          [ type_hunk ];
          type_params_hunks;
          [ type_name_hunk ];
          equal_hunks;
          type_privacy_hunks;
          type_manifest_hunks;
        ]
  in
  match
    (type_params_diff, type_privacy_diff, type_manifest_diff, equal_hunks)
  with
  | Stddiff.Same _, Stddiff.Same _, Stddiff.Same _, ([] | [ Icommon _ ]) ->
      let common_header =
        type_header_hunks
        |> List.map (function Icommon c -> c | _ -> assert false)
        |> String.concat ""
      in
      [ Common [ common_header ] ]
  | _ -> [ Inline_hunks type_header_hunks ]

and process_privacy_diff privacy_diff =
  let open Asttypes in
  match privacy_diff with
  | Same Private -> [ Icommon " private" ]
  | Same Public -> []
  | Changed Added_p -> [ Iconflict { iorig = None; inew = Some " private" } ]
  | Changed Removed_p -> [ Iconflict { iorig = Some " private"; inew = None } ]

and process_type_params_diff params_diff =
  let module S = Stddiff in
  let params_hunks =
    match params_diff with
    | Same params ->
        List.mapi
          (fun i p ->
            let comma = if i > 0 then ", " else "" in
            Icommon (Printf.sprintf "%s%s" comma (type_expr_to_string p)))
          params
    | Changed changed_params ->
        List.mapi
          (fun i p ->
            let comma = if i > 0 then ", " else "" in
            match p with
            | S.Same same_param ->
                Icommon
                  (Printf.sprintf "%s%s" comma (type_expr_to_string same_param))
            | Changed (S.Added p) ->
                Iconflict
                  { iorig = None; inew = Some (comma ^ type_expr_to_string p) }
            | Changed (Removed p) ->
                Iconflict
                  { iorig = Some (comma ^ type_expr_to_string p); inew = None }
            | Changed (Modified _) -> assert false)
          changed_params
  in
  let open_paren = Icommon " (" in
  let close_paren = Icommon ")" in
  match params_hunks with
  | [] -> []
  | _ :: [] -> Icommon " " :: params_hunks
  | _ -> (open_paren :: params_hunks) @ [ close_paren ]

and concrete = function
  | Stddiff.Same
      (Types.Type_variant (_, _) | Types.Type_record (_, _) | Types.Type_open)
  | Changed
      ( Diff.Record_tk _ | Diff.Variant_tk _
      | Diff.Atomic_tk
          {
            reference =
              ( Types.Type_variant (_, _)
              | Types.Type_record (_, _)
              | Types.Type_open );
            current =
              ( Types.Type_variant (_, _)
              | Types.Type_record (_, _)
              | Types.Type_open );
          } ) ->
      true
  | _ -> false

and abstract = function
  | Stddiff.Same (Types.Type_abstract _) -> true
  | _ -> false

and changed_to_abstract = function
  | Stddiff.Changed
      (Diff.Atomic_tk { reference = _; current = Types.Type_abstract _ }) ->
      true
  | _ -> false

and changed_to_concrete = function
  | Stddiff.Changed
      (Diff.Atomic_tk { reference = Types.Type_abstract _; current = _ }) ->
      true
  | _ -> false

and process_equal_sign_diff type_manifest_diff type_kind_diff =
  match (type_manifest_diff, type_kind_diff) with
  | (Same (Some _) | Changed (Modified _)), type_kind_diff
    when concrete type_kind_diff ->
      [ Icommon " ="; Icommon " =" ]
  | (Same (Some _) | Changed (Modified _)), type_kind_diff
    when changed_to_abstract type_kind_diff ->
      [ Icommon " ="; Iconflict { iorig = Some " ="; inew = None } ]
  | Changed (Removed _), type_kind_diff when changed_to_abstract type_kind_diff
    ->
      [
        Iconflict { iorig = Some " ="; inew = None };
        Iconflict { iorig = Some " ="; inew = None };
      ]
  | (Same (Some _) | Changed (Modified _)), type_kind_diff
    when changed_to_concrete type_kind_diff ->
      [ Icommon " ="; Iconflict { iorig = None; inew = Some " =" } ]
  | Changed (Added _), type_kind_diff when changed_to_concrete type_kind_diff ->
      [
        Iconflict { iorig = None; inew = Some " =" };
        Iconflict { iorig = None; inew = Some " =" };
      ]
  | Same None, type_kind_diff when abstract type_kind_diff -> []
  | (Same None | Changed (Removed _)), type_kind_diff
    when abstract type_kind_diff || changed_to_abstract type_kind_diff ->
      [ Iconflict { iorig = Some " ="; inew = None } ]
  | (Same None | Changed (Added _)), type_kind_diff
    when abstract type_kind_diff || changed_to_concrete type_kind_diff ->
      [ Iconflict { iorig = None; inew = Some " =" } ]
  | _ -> [ Icommon " =" ]

and process_manifest_diff ~paren manifest_diff =
  match manifest_diff with
  | Same None -> []
  | Same (Some te) -> [ Icommon (" " ^ type_expr_to_string te) ]
  | Changed (Added te) ->
      [ Iconflict { iorig = None; inew = Some (" " ^ type_expr_to_string te) } ]
  | Changed (Removed te) ->
      [ Iconflict { iorig = Some (" " ^ type_expr_to_string te); inew = None } ]
  | Changed (Modified te_diff) ->
      Icommon " " :: process_type_expr_diff ~paren te_diff

and process_type_kind_diff kind_diff =
  match kind_diff with
  | Same same_type_kind -> [ Common (type_kind_to_lines same_type_kind) ]
  | Changed (Record_tk record_diff) ->
      [ Inline_hunks (process_record_type_diff record_diff) ]
  | Changed (Variant_tk variant_diff) -> process_variant_type_diff variant_diff
  | Changed (Atomic_tk { reference; current }) ->
      let orig = type_kind_to_lines reference in
      let new_ = type_kind_to_lines current in
      [ Line_conflict { orig; new_ } ]

and process_record_type_diff record_diff =
  let open Stddiff.Map in
  let { same_map = same_lbls; changed_map = changed_lbls } = record_diff in
  let common_hunks =
    List.map
      (fun lbl -> Icommon (" " ^ lbl_to_line lbl))
      (String_map.bindings same_lbls |> List.map snd)
  in
  let different_hunks =
    List.concat_map
      ((fun f (x, y) -> f x y) process_label_diff)
      (String_map.bindings changed_lbls)
  in
  (Icommon "{" :: common_hunks) @ different_hunks @ [ Icommon " }" ]

and process_label_diff name label_diff =
  match label_diff with
  | Added lbl ->
      [ Iconflict { iorig = None; inew = Some (" " ^ lbl_to_line lbl) } ]
  | Removed lbl ->
      [ Iconflict { iorig = Some (" " ^ lbl_to_line lbl); inew = None } ]
  | Modified diff ->
      let mutable_hunks = process_mutablity_diff diff.label_mutable in
      let name_hunk = Icommon (" " ^ name ^ " :") in
      let type_hunks = process_label_type_diff diff.label_type in
      let semicolon_hunk = Icommon ";" in
      List.concat
        [ mutable_hunks; [ name_hunk ]; type_hunks; [ semicolon_hunk ] ]

and process_label_type_diff label_type_diff =
  match label_type_diff with
  | Stddiff.Same te -> [ Icommon (" " ^ type_expr_to_string te) ]
  | Stddiff.Changed te_diff -> Icommon " " :: process_type_expr_diff te_diff

and process_mutablity_diff mutablity_diff =
  let open Stddiff in
  let open Asttypes in
  match mutablity_diff with
  | Same Mutable -> [ Icommon " mutable" ]
  | Same Immutable -> []
  | Changed Added_m -> [ Iconflict { iorig = None; inew = Some " mutable" } ]
  | Changed Removed_m -> [ Iconflict { iorig = Some " mutable"; inew = None } ]

and type_kind_to_lines type_kind =
  match type_kind with
  | Type_record (lbl_lst, _) -> [ lbls_to_line lbl_lst ]
  | Type_variant (cstr_lst, _) -> List.concat_map cstr_to_lines cstr_lst
  | Type_abstract _ -> []
  | Type_open -> [ ".." ]

and process_variant_type_diff variant_diff =
  let open Stddiff.Map in
  let { same_map = same_cstrs; changed_map = changed_cstrs } = variant_diff in
  let common_hunks =
    List.map
      (fun cstr -> Common (cstr_to_lines cstr))
      (String_map.bindings same_cstrs |> List.map snd)
  in
  let different_hunks =
    List.map
      ((fun f (x, y) -> f x y) process_cstr_diff)
      (String_map.bindings changed_cstrs)
  in
  common_hunks @ different_hunks

and process_cstr_diff name cstr_diff =
  match cstr_diff with
  | Added cstr -> Line_conflict { orig = []; new_ = cstr_to_lines cstr }
  | Removed cstr -> Line_conflict { orig = cstr_to_lines cstr; new_ = [] }
  | Modified diff -> (
      match diff with
      | Atomic_cstr { reference; current } ->
          Inline_hunks
            [
              Icommon (Printf.sprintf "| %s of " name);
              Iconflict
                {
                  iorig = Some (cstr_args_to_line reference);
                  inew = Some (cstr_args_to_line current);
                };
            ]
      | Record_cstr record_diff ->
          let record_hunks = process_record_type_diff record_diff in
          Inline_hunks (Icommon (Printf.sprintf "| %s of " name) :: record_hunks)
      | Tuple_cstr tuple_diff ->
          let tuple_hunks = process_tuple_type_diff tuple_diff in
          Inline_hunks (Icommon (Printf.sprintf "| %s of " name) :: tuple_hunks)
      )

and process_tuple_type_diff diff =
  let module S = Stddiff in
  List.mapi
    (fun i te_diff ->
      let star = if i > 0 then " * " else "" in
      match te_diff with
      | S.Same same_te ->
          [ Icommon (Printf.sprintf "%s%s" star (type_expr_to_string same_te)) ]
      | Changed (Stddiff.Added te) ->
          [
            Iconflict
              { iorig = None; inew = Some (star ^ type_expr_to_string te) };
          ]
      | Changed (Removed te) ->
          [
            Iconflict
              { iorig = Some (star ^ type_expr_to_string te); inew = None };
          ]
      | Changed (Modified te) ->
          let te_hunks = process_type_expr_diff ~paren:true te in
          if i > 0 then Icommon " * " :: te_hunks else te_hunks)
    diff
  |> List.concat

and process_type_expr_diff ?(paren = true) (diff : Diff.type_expr) :
    inline_hunk list =
  match diff with
  | Diff.Atomic { reference; current } ->
      [
        Iconflict
          {
            iorig = Some (type_expr_to_string reference);
            inew = Some (type_expr_to_string current);
          };
      ]
  | Tuple tuple_diff ->
      let tuple_hunks = process_tuple_type_diff tuple_diff in
      if paren then (Icommon "(" :: tuple_hunks) @ [ Icommon ")" ]
      else tuple_hunks

and cstr_args_to_line cstr_args =
  match cstr_args with
  | Types.Cstr_tuple type_exprs -> tuple_to_line type_exprs
  | Types.Cstr_record lbls -> lbls_to_line lbls

and lbls_to_line lbls =
  let len = List.length lbls in
  List.mapi
    (fun i lbl ->
      let s = lbl_to_line lbl in
      if i = 0 && i = len - 1 then Printf.sprintf "{ %s }" s
      else if i = 0 then Printf.sprintf "{ %s" s
      else if i = len - 1 then Printf.sprintf "%s }" s
      else s)
    lbls
  |> String.concat " "

and tuple_to_line tuple =
  List.map type_expr_to_string tuple |> String.concat " * "

let process_vd_diff name diff =
  let header = Icommon (Format.sprintf "val %s : " name) in
  let type_ = process_type_expr_diff diff in
  [ Inline_hunks (header :: type_) ]

let process_value_diff (val_diff : Diff.value) =
  process_entry ~entry_to_string:vd_to_lines
    ~process_modification:process_vd_diff ~name:val_diff.vname val_diff.vdiff

let process_class_diff (class_diff : Diff.class_) =
  process_atomic_diff class_diff.cdiff class_diff.cname cd_to_lines

let process_class_type_diff (class_type_diff : Diff.cltype) =
  process_atomic_diff class_type_diff.ctdiff class_type_diff.ctname ctd_to_lines

let rec process_sig_diff :
    type a.
    _ -> (string -> a -> string list) -> (a, _) Stddiff.entry * _ -> _ -> _ =
 fun path to_lines ((diff : (a, _) Stddiff.entry), name) acc ->
  match diff with
  | Added curr_mtd ->
      let diff =
        [ Line_conflict { orig = []; new_ = to_lines name curr_mtd } ]
      in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Removed ref_mtd ->
      let diff =
        [ Line_conflict { orig = to_lines name ref_mtd; new_ = [] } ]
      in
      String_map.update path
        (function None -> Some diff | Some existing -> Some (existing @ diff))
        acc
  | Modified Diff.Unsupported ->
      String_map.add path
        [ Line_conflict { orig = []; new_ = [ "<unsupported change>" ] } ]
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

let pp_diff fmt diff = pp_ line_git_printer fmt diff
let pp fmt t = gen_pp pp_diff fmt t

module With_colors = struct
  let pp_l fmt ~color ~prefix ~line =
    Fmt.pf fmt "%a%a\n"
      Fmt.(styled color string)
      prefix
      Fmt.(styled color string)
      line

  let pp_add fmt line = pp_l fmt ~color:`Green ~prefix:"+" ~line
  let pp_remove fmt line = pp_l fmt ~color:`Red ~prefix:"-" ~line
  let pp_keep fmt line = Format.fprintf fmt " %s\n" line
  let pp_common fmt lines = List.iter (pp_keep fmt) lines

  let pp_line_conflict fmt { orig; new_ } =
    List.iter (pp_remove fmt) orig;
    List.iter (pp_add fmt) new_

  let pp_inline_conflict ~src ~mode =
    match (src, mode) with
    | _, `Color -> Fmt.(styled `Reverse string)
    | `Orig, `Plain -> fun ppf s -> Fmt.pf ppf "[-%s-]" s
    | `New, `Plain -> fun ppf s -> Fmt.pf ppf "{+%s+}" s

  let pp_inline_hunks ~mode fmt ihunks =
    Fmt.styled (`Fg `Red)
      (pp_inline_hunks ~src:`Orig
         ~pp_inline_conflict:(pp_inline_conflict ~src:`Orig ~mode))
      fmt ihunks;
    Fmt.styled (`Fg `Green)
      (pp_inline_hunks ~src:`New
         ~pp_inline_conflict:(pp_inline_conflict ~src:`New ~mode))
      fmt ihunks

  let printer ~mode =
    printer ~common:pp_common ~line_conflict:pp_line_conflict
      ~inline_hunks:(pp_inline_hunks ~mode)

  let pp_diff ~mode fmt diff = pp_ (printer ~mode) fmt diff
  let pp ~(mode : [ `Plain | `Color ]) fmt t = gen_pp (pp_diff ~mode) fmt t
end

module Word = struct
  let pp_inline_conflict_string ~src ~mode ppf s =
    match (src, mode) with
    | _, `Color -> Fmt.pf ppf "%s" s
    | `Orig, `Plain -> Fmt.pf ppf "[-%s-]" s
    | `New, `Plain -> Fmt.pf ppf "{+%s+}" s

  let pp_inline_conflict ~src ~mode ppf conflict =
    let color = match src with `Orig -> `Red | `New -> `Green in
    Fmt.styled color
      (Fmt.option ~none:Fmt.nop (pp_inline_conflict_string ~src ~mode))
      ppf conflict

  let pp_inline_hunk ~mode ppf inline_hunk =
    match inline_hunk with
    | Icommon s -> Fmt.string ppf s
    | Iconflict { iorig; inew } ->
        pp_inline_conflict ~src:`Orig ~mode ppf iorig;
        pp_inline_conflict ~src:`New ~mode ppf inew

  let pp_inline_hunks ~mode ppf ihunks =
    Fmt.pf ppf " %a\n" (Fmt.list ~sep:Fmt.nop (pp_inline_hunk ~mode)) ihunks

  let printer ~mode =
    { (With_colors.printer ~mode) with inline_hunks = pp_inline_hunks ~mode }

  let pp_diff ~mode fmt diff = pp_ (printer ~mode) fmt diff
  let pp ~(mode : [ `Plain | `Color ]) fmt t = gen_pp (pp_diff ~mode) fmt t
end
