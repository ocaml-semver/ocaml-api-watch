open Types

type t = {
  values_map : value_description String_map.t;
  modules_map : module_declaration String_map.t;
  modtypes_map : modtype_declaration String_map.t;
  types_map : (type_declaration * Ident.t) String_map.t;
  class_map : class_declaration String_map.t;
  class_type_map : class_type_declaration String_map.t;
}

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type
  | Class : class_declaration item_type
  | Classtype : class_type_declaration item_type

let empty : t =
  {
    values_map = String_map.empty;
    modules_map = String_map.empty;
    modtypes_map = String_map.empty;
    types_map = String_map.empty;
    class_map = String_map.empty;
    class_type_map = String_map.empty;
  }

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

let has (type a) ~name (item_type : a item_type) maps : bool =
  match item_type with
  | Value -> String_map.mem name maps.values_map
  | Module -> String_map.mem name maps.modules_map
  | Modtype -> String_map.mem name maps.modtypes_map
  | Type -> String_map.mem name maps.types_map
  | Class -> String_map.mem name maps.class_map
  | Classtype -> String_map.mem name maps.class_type_map

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
  value_diffs @ module_diffs @ modtype_diffs @ type_diffs @ class_diffs
  @ class_type_diffs
