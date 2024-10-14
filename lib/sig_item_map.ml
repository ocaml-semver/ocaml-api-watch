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
    { values_map; modules_map; modtypes_map; types_map } =
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

let diff = failwith "TODO"
