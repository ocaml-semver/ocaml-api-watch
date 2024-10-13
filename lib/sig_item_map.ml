open Types

type t =
  value_description String_map.t
  * module_declaration String_map.t
  * modtype_declaration String_map.t
  * (type_declaration * Ident.t) String_map.t

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type

let empty : t =
  (String_map.empty, String_map.empty, String_map.empty, String_map.empty)

let add : type a. name:string -> a item_type -> a -> t -> t =
 fun ~name item_type item maps ->
  match (item_type, maps) with
  | Value, (values_map, modules_map, modtypes_map, types_map) ->
      (String_map.add name item values_map, modules_map, modtypes_map, types_map)
  | Module, (values_map, modules_map, modtypes_map, types_map) ->
      (values_map, String_map.add name item modules_map, modtypes_map, types_map)
  | Modtype, (values_map, modules_map, modtypes_map, types_map) ->
      (values_map, modules_map, String_map.add name item modtypes_map, types_map)
  | Type, (values_map, modules_map, modtypes_map, types_map) ->
      (values_map, modules_map, modtypes_map, String_map.add name item types_map)

let diff = failwith "TODO"
