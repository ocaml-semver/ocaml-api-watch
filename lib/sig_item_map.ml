open Types

module String_pair_map = Map.Make (struct
  type t = string * string [@@deriving ord]
end)

type t = {
  values_map : value_description String_map.t;
  modules_map : module_declaration String_map.t;
  modtypes_map : modtype_declaration String_map.t;
  types_map : (type_declaration * Ident.t) String_map.t;
  class_map : class_declaration String_map.t;
  class_type_map : class_type_declaration String_map.t;
  ext_cstr_map : extension_constructor String_pair_map.t;
}

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type
  | Class : class_declaration item_type
  | Classtype : class_type_declaration item_type
  | Extcstr : extension_constructor item_type

type _ item_key = Single : string item_key | Pair : (string * string) item_key

let empty : t =
  {
    values_map = String_map.empty;
    modules_map = String_map.empty;
    modtypes_map = String_map.empty;
    types_map = String_map.empty;
    class_map = String_map.empty;
    class_type_map = String_map.empty;
    ext_cstr_map = String_pair_map.empty;
  }

let add (type a b) (key_type : b item_key) (key : b) (item_type : a item_type)
    (item : a) maps : t =
  match (item_type, key_type) with
  | Value, Single ->
      { maps with values_map = String_map.add key item maps.values_map }
  | Module, Single ->
      { maps with modules_map = String_map.add key item maps.modules_map }
  | Modtype, Single ->
      { maps with modtypes_map = String_map.add key item maps.modtypes_map }
  | Type, Single ->
      { maps with types_map = String_map.add key item maps.types_map }
  | Class, Single ->
      { maps with class_map = String_map.add key item maps.class_map }
  | Classtype, Single ->
      { maps with class_type_map = String_map.add key item maps.class_type_map }
  | Extcstr, Pair ->
      {
        maps with
        ext_cstr_map = String_pair_map.add key item maps.ext_cstr_map;
      }
  | _ -> assert false

let has (type a b) (key_type : b item_key) (key : b)
    (item_type : a item_type) maps : bool =
  match (item_type, key_type) with
  | Value, Single -> String_map.mem key maps.values_map
  | Module, Single -> String_map.mem key maps.modules_map
  | Modtype, Single -> String_map.mem key maps.modtypes_map
  | Type, Single -> String_map.mem key maps.types_map
  | Class, Single -> String_map.mem key maps.class_map
  | Classtype, Single -> String_map.mem key maps.class_type_map
  | Extcstr, Pair -> String_pair_map.mem key maps.ext_cstr_map
  | _ -> assert false

type ('a, 'key, 'diff) diff_item =
  'a item_type -> 'key -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a 'key. ('a, 'key, 'diff) diff_item }

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
    String_pair_map.merge
      (fun (name, type_name) ref_opt curr_opt ->
        diff_item Extcstr (name, type_name) ref_opt curr_opt)
      ref_maps.ext_cstr_map curr_maps.ext_cstr_map
    |> String_pair_map.bindings |> List.map snd
  in
  value_diffs @ module_diffs @ modtype_diffs @ type_diffs @ class_diffs
  @ class_type_diffs @ ext_cstr_diffs
