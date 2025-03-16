open Types

type t

type _ item_type =
  | Value : (int * value_description) item_type
  | Module : (int * module_declaration) item_type
  | Modtype : (int * modtype_declaration) item_type
  | Type : (int * type_declaration * Ident.t) item_type
  | Class : (int * class_declaration) item_type
  | Classtype : (int * class_type_declaration) item_type

val empty : t
val add : name:string -> 'a item_type -> 'a -> t -> t
val has : name:string -> 'a item_type -> t -> bool

type ('a, 'diff) diff_item =
  'a item_type -> string -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a. ('a, 'diff) diff_item }

val diff : diff_item:'diff poly_diff_item -> t -> t -> 'diff list
