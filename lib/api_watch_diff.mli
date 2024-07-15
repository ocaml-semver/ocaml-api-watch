type 'item change =
  | Added of 'item
  | Removed of 'item
  | Modified of { ref : 'item; current : 'item }

type diff = Value of string * Types.value_description change | Any

val diff_interface :
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  diff list

val to_diff : diff list -> Diffutils.Diff.t
