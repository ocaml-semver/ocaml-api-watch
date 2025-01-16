val load_cmi : string -> (Types.signature * string, string) result

type t = Types.signature String_map.t
(** Type representing a library's root modules.
    For wrapped libraries, contains only the main module.
    For unwrapped libraries, contains all root-level modules. *)

val load_unwrapped : string -> (t, string) result
(** Load an unwrapped library, returning all root-level modules.
    Each module in the returned map represents a .cmi file at the root
    of the project path. *)

val load : main_module:string -> string -> (t, string) result
(** Load a wrapped library, returning only its main module.
    The returned map will contain a single entry for the main module,
    with all its dependencies expanded. *)
