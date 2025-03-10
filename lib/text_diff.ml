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

let type_expr_to_string typ_exp =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.type_expr formatter typ_exp;
  Format.pp_print_flush formatter ();
  String.map (function '\n' -> ' ' | c -> c) (Buffer.contents buf)

let params_to_string params =
  let module TD = Intermed.TypeDecl in
  match params with
  | [] -> ""
  | param :: [] -> type_expr_to_string param.TD.type_expr
  | _ ->
      Printf.sprintf "(%s)"
        (String.concat ", "
           (List.map
              (fun param -> type_expr_to_string param.TD.type_expr)
              params))

let private_to_string private_ = if private_ then " private" else ""
let mutable_to_string mutable_ = if mutable_ then " mutable" else ""

let field_to_string field =
  let open Intermed.TypeDecl.Field in
  let mutable_string = mutable_to_string field.mutable_ in
  let type_expr_string = type_expr_to_string field.type_ in
  Printf.sprintf " %s %s : %s;" mutable_string field.name type_expr_string

let fields_to_line fields =
  let len = List.length fields in
  List.mapi
    (fun i field ->
      let s = field_to_string field in
      if i = 0 && i = len - 1 then Printf.sprintf "{ %s }" s
      else if i = 0 then Printf.sprintf "{ %s" s
      else if i = len - 1 then Printf.sprintf "%s }" s
      else s)
    fields
  |> String.concat " "

and tuple_to_line tuple =
  List.map type_expr_to_string tuple |> String.concat " * "

let cstr_args_to_string = function
  | Intermed.TypeDecl.Constructor.Tuple type_exprs -> tuple_to_line type_exprs
  | Record fields -> fields_to_line fields

let cstr_to_string cstr =
  let module C = Intermed.TypeDecl.Constructor in
  Printf.sprintf "| %s of %s" cstr.C.name (cstr_args_to_string cstr.args)

let cstrs_to_lines cstrs = List.map cstr_to_string cstrs

let type_definition_to_lines definition =
  let open Intermed.TypeDecl.Kind in
  match definition with
  | Open -> []
  | Record fields -> [ fields_to_line fields ]
  | Variant cstrs -> cstrs_to_lines cstrs

let type_header_to_line private_ manifest definition =
  let module K = Intermed.TypeDecl.Kind in
  match (manifest, definition) with
  | None, K.Open -> Printf.sprintf " = %s .." (private_to_string private_)
  | Some type_expr, Open ->
      Printf.sprintf " = %s = %s .."
        (type_expr_to_string type_expr)
        (private_to_string private_)
  | None, _ -> Printf.sprintf " = %s" (private_to_string private_)
  | Some type_expr, _ ->
      Printf.sprintf " = %s = %s"
        (type_expr_to_string type_expr)
        (private_to_string private_)

let type_kind_to_lines kind =
  let open Intermed.TypeDecl.Kind in
  match kind with
  | Abstract -> ("", [])
  | Alias { private_; type_expr } ->
      let private_string = private_to_string private_ in
      let type_expr_string = type_expr_to_string type_expr in
      (Printf.sprintf " = %s %s" private_string type_expr_string, [])
  | Concrete { manifest; private_; definition } ->
      ( type_header_to_line private_ manifest definition,
        type_definition_to_lines definition )

let td_to_lines name td =
  let open Intermed.TypeDecl in
  let params_string = params_to_string td.params in
  let header_string, definition_string = type_kind_to_lines td.kind in
  Printf.sprintf "type %s %s%s" params_string name header_string
  :: definition_string

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

let indent amount str =
  let indentation = String.make amount ' ' in
  indentation ^ str

let indent_hunk amount hunk =
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

and process_type_params_diff diff =
  let module TD = Intermed.TypeDecl in
  let module P = Diff.TypeDecl.Param in
  let module S = Stddiff in
  let params_hunks =
    match diff with
    | S.Same params ->
        List.mapi
          (fun i p ->
            let comma = if i > 0 then ", " else "" in
            Icommon
              (Printf.sprintf "%s%s" comma (type_expr_to_string p.TD.type_expr)))
          params
    | S.Changed changed_params ->
        List.mapi
          (fun i p ->
            let comma = if i > 0 then ", " else "" in
            match p with
            | Stddiff.Same p ->
                Icommon
                  (Printf.sprintf "%s%s" comma
                     (type_expr_to_string p.TD.type_expr))
            | Changed (P.Added p) ->
                Iconflict
                  {
                    iorig = None;
                    inew = Some (comma ^ type_expr_to_string p.TD.type_expr);
                  }
            | Changed (P.Removed p) ->
                Iconflict
                  {
                    iorig = Some (comma ^ type_expr_to_string p.TD.type_expr);
                    inew = None;
                  })
          changed_params
  in
  let open_paren = Icommon " (" in
  let close_paren = Icommon ")" in
  match params_hunks with
  | [] -> []
  | _ :: [] -> Icommon " " :: params_hunks
  | _ -> (open_paren :: params_hunks) @ [ close_paren ]

and process_privacy_diff diff =
  let module S = Stddiff in
  let module K = Diff.TypeDecl.Kind in
  match diff with
  | S.Same true -> Icommon " = private"
  | Same false -> Icommon " ="
  | Changed K.Added -> Iconflict { iorig = None; inew = Some " = private" }
  | Changed K.Removed -> Iconflict { iorig = Some " = private"; inew = None }

and process_type_expr_diff diff =
  let module S = Stddiff in
  match diff with
  | S.Same type_expr -> Icommon (type_expr_to_string type_expr)
  | S.Changed { S.reference; current } ->
      Iconflict
        {
          iorig = Some (type_expr_to_string reference);
          inew = Some (type_expr_to_string current);
        }

let process_manifest_diff diff =
  let open Stddiff in
  match diff with
  | Same None -> []
  | Same (Some te) -> [ Icommon (" = " ^ type_expr_to_string te) ]
  | Changed (Added te) ->
      [
        Iconflict { iorig = None; inew = Some (" = " ^ type_expr_to_string te) };
      ]
  | Changed (Removed te) ->
      [
        Iconflict { iorig = Some (" = " ^ type_expr_to_string te); inew = None };
      ]
  | Changed (Modified { reference; current }) ->
      [
        Icommon " ";
        Iconflict
          {
            iorig = Some (type_expr_to_string reference);
            inew = Some (type_expr_to_string current);
          };
      ]

let process_mutable_diff diff =
  let module S = Stddiff in
  let module F = Diff.TypeDecl.Field in
  match diff with
  | S.Same mutable_ -> Icommon (mutable_to_string mutable_)
  | Changed F.Added ->
      Iconflict { iorig = None; inew = Some (mutable_to_string true) }
  | Changed Removed ->
      Iconflict { iorig = Some (mutable_to_string true); inew = None }

let process_field_diff name diff =
  let module S = Stddiff in
  let module F = Diff.TypeDecl.Field in
  match diff with
  | S.Added field ->
      [ Iconflict { iorig = None; inew = Some (field_to_string field) } ]
  | Removed field ->
      [ Iconflict { iorig = Some (field_to_string field); inew = None } ]
  | Modified field_change ->
      let mutable_hunk = process_mutable_diff field_change.F.mutable_ in
      let name_hunk = Icommon (Printf.sprintf " %s :" name) in
      let type_hunk = process_type_expr_diff field_change.type_ in
      let semicolon_hunk = Icommon ";" in
      [ mutable_hunk; name_hunk; type_hunk; semicolon_hunk ]

let process_fields_diff diff =
  let module S = Stddiff in
  let { S.same_map = same_fields; changed_map = changed_fields } = diff in
  let common_hunks =
    List.map
      (fun field -> Icommon (field_to_string field))
      (String_map.bindings same_fields |> List.map snd)
  in
  let different_hunks =
    List.concat_map
      ((fun f (x, y) -> f x y) process_field_diff)
      (String_map.bindings changed_fields)
  in
  (Icommon "{" :: common_hunks) @ different_hunks @ [ Icommon " }" ]

let process_tuple_type_diff tuple_diff =
  let open Stddiff in
  List.mapi
    (fun i te_diff ->
      let star = if i > 0 then " * " else "" in
      match te_diff with
      | Same same_te ->
          [ Icommon (Printf.sprintf "%s%s" star (type_expr_to_string same_te)) ]
      | Changed (Added te) ->
          [
            Iconflict
              { iorig = None; inew = Some (star ^ type_expr_to_string te) };
          ]
      | Changed (Removed te) ->
          [
            Iconflict
              { iorig = Some (star ^ type_expr_to_string te); inew = None };
          ]
      | Changed (Modified { reference; current }) ->
          let te_hunk =
            Iconflict
              {
                iorig = Some (type_expr_to_string reference);
                inew = Some (type_expr_to_string current);
              }
          in
          if i > 0 then [ Icommon " * "; te_hunk ] else [ te_hunk ])
    tuple_diff
  |> List.concat

let process_cstr_diff name cstr_diff =
  let module S = Stddiff in
  let module C = Diff.TypeDecl.Constructor in
  match cstr_diff with
  | S.Added cstr -> Line_conflict { orig = []; new_ = [ cstr_to_string cstr ] }
  | Removed cstr -> Line_conflict { orig = [ cstr_to_string cstr ]; new_ = [] }
  | Modified cstr_change -> (
      match cstr_change.C.args with
      | C.Unshared { reference; current } ->
          Inline_hunks
            [
              Icommon (Printf.sprintf "| %s of " name);
              Iconflict
                {
                  iorig = Some (cstr_args_to_string reference);
                  inew = Some (cstr_args_to_string current);
                };
            ]
      | Record fields_change ->
          let record_hunks = process_fields_diff fields_change in
          Inline_hunks (Icommon (Printf.sprintf "| %s of " name) :: record_hunks)
      | Tuple tuple_change ->
          let tuple_hunks = process_tuple_type_diff tuple_change in
          Inline_hunks (Icommon (Printf.sprintf "| %s of " name) :: tuple_hunks)
      )

let process_cstrs_diff variant_diff =
  let open Stddiff in
  let { same_map = same_cstrs; changed_map = changed_cstrs } = variant_diff in
  let common_hunks =
    List.map
      (fun cstr -> Common [ cstr_to_string cstr ])
      (String_map.bindings same_cstrs |> List.map snd)
  in
  let different_hunks =
    List.map
      ((fun f (x, y) -> f x y) process_cstr_diff)
      (String_map.bindings changed_cstrs)
  in
  common_hunks @ different_hunks

let process_definition_diff diff =
  let module S = Stddiff in
  let module K = Intermed.TypeDecl.Kind in
  let module KD = Diff.TypeDecl.Kind in
  match diff with
  | S.Same K.Open -> ( [ Icommon " .." ], [])
  | Same (Record fields) -> ([], [ Common ([ fields_to_line fields ]) ])
  | Same (Variant cstrs) -> ([], [ Common (cstrs_to_lines cstrs) ])
  | Changed (KD.Record fields_change) ->
      ([], [ Inline_hunks (process_fields_diff fields_change) ])
  | Changed (Variant cstrs_change) -> ([], process_cstrs_diff cstrs_change)
  | Changed (Unshared_definition { reference; current }) -> (
      match (reference, current) with
      | Open, _ ->
          ( [ Iconflict { iorig = Some " .."; inew = None } ],
            [
              Line_conflict
                { orig = []; new_ = type_definition_to_lines current };
            ] )
      | _, Open ->
          ( [ Iconflict { iorig = None; inew = Some " .." } ],
            [
              Line_conflict
                { orig = type_definition_to_lines reference; new_ = [] };
            ] )
      | _ ->
          ( [],
            [
              Line_conflict
                {
                  orig = type_definition_to_lines reference;
                  new_ = type_definition_to_lines current;
                };
            ] ))

let process_type_kind_diff diff : inline_hunk list * hunk list =
  let module S = Stddiff in
  let module K = Diff.TypeDecl.Kind in
  let header_ihunks, definition_hunk =
    match diff with
    | S.Same kind ->
        let header_string, definition_strings = type_kind_to_lines kind in
        ([ Icommon header_string ], [ Common definition_strings ])
    | Changed (K.Alias { type_expr; private_ }) ->
        ([ process_privacy_diff private_; process_type_expr_diff type_expr ], [])
    | Changed (Concrete { private_; manifest; definition }) ->
        let manifest_ihunk = process_manifest_diff manifest in
        let private_ihunk = process_privacy_diff private_ in
        let header_ihunks, definition_hunk = process_definition_diff definition in
        ([ manifest_ihunk; private_ihunk ] @ header_ihunks, definition_hunk)
    | Changed (Unshared { reference; current }) -> assert false
  in
  (header_ihunks, definition_hunk)

let process_modified_type_diff name diff =
  let module TD = Diff.TypeDecl in
  let type_ihunk = Icommon "type" in
  let params_ihunks = process_type_params_diff diff.TD.params in
  let name_ihunk = Icommon name in
  let header_ihunks, definition_hunks = process_type_kind_diff diff.TD.kind in
  Inline_hunks ((type_ihunk :: params_ihunks) @ [ name_ihunk ] @ header_ihunks)
  :: List.map (indent_hunk 2) definition_hunks

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

let process_entry_diff ~name ~entry_to_lines ~process_modification diff =
  match (diff : (_, _) Stddiff.entry) with
  | Added entry -> process_atomic_diff (Added entry) name entry_to_lines
  | Removed entry -> process_atomic_diff (Removed entry) name entry_to_lines
  | Modified diff -> process_modification name diff

let process_type_diff (type_diff : Diff.type_) =
  process_entry_diff ~name:type_diff.tname ~entry_to_lines:td_to_lines
    ~process_modification:process_modified_type_diff type_diff.tdiff

let process_value_diff (val_diff : Diff.value) =
  process_atomic_diff val_diff.vdiff val_diff.vname vd_to_lines

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

  let pp_inline_hunks fmt ihunks =
    Fmt.styled (`Fg `Red)
      (pp_inline_hunks ~src:`Orig
         ~pp_inline_conflict:Fmt.(styled `Reverse string))
      fmt ihunks;
    Fmt.styled (`Fg `Green)
      (pp_inline_hunks ~src:`New
         ~pp_inline_conflict:Fmt.(styled `Reverse string))
      fmt ihunks

  let printer =
    printer ~common:pp_common ~line_conflict:pp_line_conflict
      ~inline_hunks:pp_inline_hunks

  let pp_diff fmt diff = pp_ printer fmt diff
  let pp fmt t = gen_pp pp_diff fmt t
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
    { With_colors.printer with inline_hunks = pp_inline_hunks ~mode }

  let pp_diff ~mode fmt diff = pp_ (printer ~mode) fmt diff
  let pp ~(mode : [ `Plain | `Color ]) fmt t = gen_pp (pp_diff ~mode) fmt t
end
