open Types

type t

type (_, _) item_type =
  | Value : (value_description, string) item_type
  | Module : (module_declaration, string) item_type
  | Modtype : (modtype_declaration, string) item_type
  | Type : (type_declaration * Ident.t, string) item_type
  | Class : (class_declaration, string) item_type
  | Classtype : (class_type_declaration, string) item_type
  | Extcstr : (extension_constructor, string * string) item_type

val empty : t
val add : key:'b -> ('a, 'b) item_type -> 'a -> t -> t
val has : key:'b -> ('a, 'b) item_type -> t -> bool

type ('a, 'b, 'diff) diff_item =
  ('a, 'b) item_type -> string -> 'a option -> 'a option -> 'diff option

type 'diff poly_diff_item = { diff_item : 'a 'b. ('a, 'b, 'diff) diff_item }

val diff : diff_item:'diff poly_diff_item -> t -> t -> 'diff list
