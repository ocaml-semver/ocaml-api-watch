type change = Added | Removed | Modified
type diff = Value of string * change | Any

val diff_interface :
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  diff list
