open Types

type t = {
  values_map : value_description String_map.t;
  modules_map : module_declaration String_map.t;
  modtypes_map : modtype_declaration String_map.t;
  types_map : (type_declaration * Ident.t) String_map.t;
}

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type

let empty : t =
  {
    values_map = String_map.empty;
    modules_map = String_map.empty;
    modtypes_map = String_map.empty;
    types_map = String_map.empty;
  }

let add (type a) ~name (item_type : a item_type) (item : a)
    { values_map; modules_map; modtypes_map; types_map } : t =
  match item_type with
  | Value ->
      {
        values_map = String_map.add name item values_map;
        modules_map;
        modtypes_map;
        types_map;
      }
  | Module ->
      {
        values_map;
        modules_map = String_map.add name item modules_map;
        modtypes_map;
        types_map;
      }
  | Modtype ->
      {
        values_map;
        modules_map;
        modtypes_map = String_map.add name item modtypes_map;
        types_map;
      }
  | Type ->
      {
        values_map;
        modules_map;
        modtypes_map;
        types_map = String_map.add name item types_map;
      }

type ('a, 'diff) diff_item =
  'a item_type -> string -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a. ('a, 'diff) diff_item }

let diff ~diff_item:{ diff_item }
    {
      values_map = ref_values_map;
      modules_map = ref_modules_map;
      modtypes_map = ref_modtypes_map;
      types_map = ref_types_map;
    }
    {
      values_map = curr_values_map;
      modules_map = curr_modules_map;
      modtypes_map = curr_modtypes_map;
      types_map = curr_types_map;
    } : 'diff list =
  let value_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Value name ref_opt curr_opt)
      ref_values_map curr_values_map
    |> String_map.bindings |> List.map snd
  in
  let module_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Module name ref_opt curr_opt)
      ref_modules_map curr_modules_map
    |> String_map.bindings |> List.map snd
  in
  let modtype_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Modtype name ref_opt curr_opt)
      ref_modtypes_map curr_modtypes_map
    |> String_map.bindings |> List.map snd
  in
  let type_diffs =
    String_map.merge
      (fun name ref_opt curr_opt -> diff_item Type name ref_opt curr_opt)
      ref_types_map curr_types_map
    |> String_map.bindings |> List.map snd
  in
  value_diffs @ module_diffs @ modtype_diffs @ type_diffs
