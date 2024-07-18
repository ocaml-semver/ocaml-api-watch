type 'item change =
  | Added of 'item
  | Removed of 'item
  | Modified of { ref_ : 'item; current : 'item }

type module_diff = { module_name : string; changes : module_change }

and item_change =
  | Value of { name : string; change : Types.value_description change }
  | Module of module_diff

and module_change = Unsupported | Supported of item_change list

type diff = module_diff option

module FieldMap : Map.S with type key = string

val diff_interface :
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  diff

val to_text_diff : diff -> Diffutils.Diff.t FieldMap.t
