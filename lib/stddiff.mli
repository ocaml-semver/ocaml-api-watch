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

type ('item, 'diff) map = {
  same_map : 'item String_map.t;
  changed_map : ('item, 'diff) entry String_map.t;
}

type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change
type 'item atomic_entry = ('item, 'item atomic_modification) entry

type 'same maybe_changed_atomic =
  ('same, 'same atomic_modification) maybe_changed

type 'same maybe_changed_atomic_entry =
  ('same, 'same atomic_entry) maybe_changed

type ('same, 'diff) option_ = ('same option, ('same, 'diff) entry) maybe_changed
type 'same atomic_option = ('same, 'same atomic_modification) option_
type ('same, 'diff) list_ = ('same list, 'diff list) maybe_changed
