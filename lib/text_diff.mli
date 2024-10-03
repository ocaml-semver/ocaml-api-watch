(** Utilities for custom diff printing  *)

type conflict2 = { orig : string list; new_ : string list }
(** In a value of type {!conflict2}, the sequence [orig] and [new_] should
        have no common value. *)

type hunk = Same of string | Diff of conflict2
type t = hunk list

type ts = t String_map.t
(** Type for representing library interface diffs as text diff.

    Changes are arranged per fully qualified module path.
    Keys are module path, as strings, that map to the textual diff
    for the content of said module.
    
    The removal or addition of a module is listed under its parent.
    E.g. if [Main.M] was removed, this will show in the textual diff
    under the key ["Main"].
    On the other hand, if [Main.M] is present in both versions but received
    a new function [Main.M.do_something], this will show in the textual
    diff under the key ["Main.M"].
    Identical modules won't appear in the map.

    Note that the individual [t] stored in the map only
    contain line changes, i.e. only [Diff {orig; new_}] hunks, no [Same s]
    ones. *)

type printer

val printer : same:string Fmt.t -> diff:conflict2 Fmt.t -> printer
val git_printer : printer
val pp : printer -> t Fmt.t
val from_diff : Diff.module_ -> ts

val pp_git : Format.formatter -> ts -> unit
(** Pretty-print the text diff in a human readable, git diff like format. *)

module With_colors : sig
  val pp : Format.formatter -> ts -> unit
  (** Same as regular [pp] but prints added lines in green and removed lines
    in red. *)
end
