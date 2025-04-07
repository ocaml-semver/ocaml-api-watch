open Types

type t = {
  values_map : value_description String_map.t;
  modules_map : module_declaration String_map.t;
  modtypes_map : modtype_declaration String_map.t;
  types_map : (type_declaration * Ident.t) String_map.t;
  class_map : class_declaration String_map.t;
  class_type_map : class_type_declaration String_map.t;
  ext_cstr_map : (extension_constructor * bool) String_map.t;
}

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type
  | Class : class_declaration item_type
  | Classtype : class_type_declaration item_type
  | Extcstr : string -> (extension_constructor * bool) item_type

let empty : t =
  {
    values_map = String_map.empty;
    modules_map = String_map.empty;
    modtypes_map = String_map.empty;
    types_map = String_map.empty;
    class_map = String_map.empty;
    class_type_map = String_map.empty;
    ext_cstr_map = String_map.empty;
  }

let ext_cstr_full_name ~type_name ~name = Printf.sprintf "%s-%s" type_name name

let add (type a) ~name (item_type : a item_type) (item : a) maps : t =
  match item_type with
  | Value -> { maps with values_map = String_map.add name item maps.values_map }
  | Module ->
      { maps with modules_map = String_map.add name item maps.modules_map }
  | Modtype ->
      { maps with modtypes_map = String_map.add name item maps.modtypes_map }
  | Type -> { maps with types_map = String_map.add name item maps.types_map }
  | Class -> { maps with class_map = String_map.add name item maps.class_map }
  | Classtype ->
      {
        maps with
        class_type_map = String_map.add name item maps.class_type_map;
      }
  | Extcstr extcstr_name ->
      {
        maps with
        ext_cstr_map =
          String_map.add
            (ext_cstr_full_name ~type_name:name ~name:extcstr_name)
            item maps.ext_cstr_map;
      }

let has (type a) ~name (item_type : a item_type) maps : bool =
  match item_type with
  | Value -> String_map.mem name maps.values_map
  | Module -> String_map.mem name maps.modules_map
  | Modtype -> String_map.mem name maps.modtypes_map
  | Type -> String_map.mem name maps.types_map
  | Class -> String_map.mem name maps.class_map
  | Classtype -> String_map.mem name maps.class_type_map
  | Extcstr extcstr_name ->
      String_map.mem
        (ext_cstr_full_name ~type_name:name ~name:extcstr_name)
        maps.ext_cstr_map

type ('a, 'diff) diff_item =
  'a item_type -> string -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a. ('a, 'diff) diff_item }

let diff ~diff_item:{ diff_item } ref_maps curr_maps : 'diff list =
  let value_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Value name ref_opt curr_opt)
      ref_maps.values_map curr_maps.values_map
    |> String_map.bindings |> List.map snd
  in
  let module_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Module name ref_opt curr_opt)
      ref_maps.modules_map curr_maps.modules_map
    |> String_map.bindings |> List.map snd
  in
  let modtype_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Modtype name ref_opt curr_opt)
      ref_maps.modtypes_map curr_maps.modtypes_map
    |> String_map.bindings |> List.map snd
  in
  let type_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Type name ref_opt curr_opt)
      ref_maps.types_map curr_maps.types_map
    |> String_map.bindings |> List.map snd
  in
  let class_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Class name ref_opt curr_opt)
      ref_maps.class_map curr_maps.class_map
    |> String_map.bindings |> List.map snd
  in
  let class_type_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Classtype name ref_opt curr_opt)
      ref_maps.class_type_map curr_maps.class_type_map
    |> String_map.bindings |> List.map snd
  in
  let ext_cstr_diffs =
    String_map.merge
      (fun full_name ref_opt curr_opt ->
        let names = String.split_on_char '-' full_name in
        let type_name = List.hd names in
        let cstr_name = List.hd (List.tl names) in
        diff_item (Extcstr cstr_name) type_name ref_opt curr_opt)
      ref_maps.ext_cstr_map curr_maps.ext_cstr_map
    |> String_map.bindings |> List.map snd
  in
  value_diffs @ module_diffs @ modtype_diffs @ type_diffs @ class_diffs
  @ class_type_diffs @ ext_cstr_diffs
