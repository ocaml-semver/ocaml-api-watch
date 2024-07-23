type 'item change =
  | Added of 'item
  | Removed of 'item
  | Modified of { ref_ : 'item; current : 'item }

type module_diff = { module_name : string; changes : module_change }

and item_change =
  | Value of { name : string; change : Types.value_description change }
  | Module of module_diff

and module_change = Unsupported | Supported of item_change list

module String_map : Map.S with type key = string

val diff_interface :
  string ->
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  module_diff option

val to_text_diff : module_diff -> Diffutils.Diff.t String_map.t
