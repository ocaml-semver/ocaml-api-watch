open Types

type t

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type

val empty : t
val add : name:string -> 'a item_type -> 'a -> t -> t

val diff :
  diff_item:('a item_type -> string -> 'a option -> 'a option -> 'diff option) ->
  t ->
  t ->
  'diff list
