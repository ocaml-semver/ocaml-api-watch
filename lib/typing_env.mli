(** Utilities to setup and manipulate typing environments *)

open Types

type t = { env : Env.t; subst : Subst.t }

val for_diff :
  reference:signature -> current:signature -> signature * signature * t
(** Returns two modified signatures with unique IDs that are suitable
    for placing in the same typing environment that we use for diffing.

    To do so, we first add the [reference] signature to a fresh typing
    environment and then modify the [current] signature items to have
    different IDs from the [reference] signature items.
    We then build a subsitition for alias types, modules and module types,
    so that these items appearing in the [current] signature treated
    equally across the two signatures by the compiler.
    The [subst] should be run aganist a signature item in the [current]
    signature before diffing it with signature item in the [reference] signature.
*)

val pp : Format.formatter -> Env.t -> unit
(** Use for debugging *)
