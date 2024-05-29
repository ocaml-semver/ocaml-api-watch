type 'a change_type = Added | Removed | Modified of 'a

type diff =
  | Value of
      string * (Types.value_description * Types.value_description) change_type
  | Any

val diff_interface :
  reference:Types.signature_item list ->
  current:Types.signature_item list ->
  diff list
