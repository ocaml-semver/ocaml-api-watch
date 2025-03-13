(** Type aliases for representing common OCaml types diffs *)

(** Represent a change of an entry in a collection *)
type ('item, 'diff) entry =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
    [reference] is the value before and [current] is the value after the change
    occured. Use this type when there is no better representation available. *)

type 'item atomic_entry = ('item, 'item atomic_modification) entry
type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change

module List_ : sig
  type ('a, 'diff) t = ('a, ('a, 'diff) entry) maybe_changed list

  val diff :
    diff_one:('a -> 'a -> ('a, 'diff) maybe_changed) ->
    reference:'a list ->
    current:'a list ->
    ('a list, ('a, 'diff) t) maybe_changed
end

module Option_ : sig
  type ('a, 'diff) t = ('a option, ('a, 'diff) entry) maybe_changed

  val diff :
    diff_one:('a -> 'a -> ('a, 'diff) maybe_changed) ->
    reference:'a option ->
    current:'a option ->
    ('a, 'diff) t
end

module Map_ : sig
  type ('a, 'diff) t = {
    same_map : 'a String_map.t;
    changed_map : ('a, 'diff) entry String_map.t;
  }

  val diff :
    diff_one:('a -> 'a -> ('a, 'diff) maybe_changed) ->
    reference:'a String_map.t ->
    current:'a String_map.t ->
    ('a, 'diff) t
end
