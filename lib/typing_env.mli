(** Utilities to setup and manipulate typing environments *)

open Types

type t = Env.t

val initialized_env : unit -> t
(** Returns a environment initialized with the standard library. *)

val for_diff :
  reference:signature ->
  current:signature ->
  env:Env.t ->
  signature * signature * t
(** Returns two modified signatures with unique IDs that are suitable
    for placing in the same typing environment that we use for diffing.

    To do so, we first add the [reference] signature to a fresh typing
    environment and then modify the [current] signature items to have
    different IDs from the [reference] signature items.
    We then build a subsitition for non alias types, modules and module types,
    so that these items appearing in the [current] signature are treated
    equally across the two signatures by the compiler.
    We then run the subst aganist the signature items in the [current]
    signature before diffing it with signature item in the [reference] signature.
*)

val pp : Format.formatter -> Env.t -> unit
(** Use for debugging *)
