(** Utilities for custom diff printing  *)

type t
(** Represents a single textual diff of a module interface*)

val pp : Format.formatter -> Diff.module_ -> unit
(** Pretty-print the text diff in a human readable, git diff like format. *)

module With_colors : sig
  val pp : Format.formatter -> Diff.module_ -> unit
  (** Same as regular [pp] but prints added lines in green and removed lines
    in red. *)
end
