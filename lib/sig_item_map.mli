open Types

type t

type _ item_type =
  | Value : value_description item_type
  | Module : module_declaration item_type
  | Modtype : modtype_declaration item_type
  | Type : (type_declaration * Ident.t) item_type

val empty : t
val add : name:string -> 'a item_type -> 'a -> t -> t

type ('a, 'diff) diff_item =
  'a item_type -> string -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a. ('a, 'diff) diff_item }

val diff : diff_item:'diff poly_diff_item -> t -> t -> 'diff list
