open Api_watch
open Test_helpers

let%expect_test "multi-line items are represented as multi-line diffs" =
  let reference = compile_interface {|
      |} in
  let current =
    compile_interface
      {|
      val f :
        some_long_labeled_argument: int ->
        some_other_long_labeled_arg: int * int ->
        string * string ->
        unit ->
        string

      module M : sig
        val some_val : int -> int -> int -> string
        val some_other_val : string -> string -> string -> int
        val yet_some_other_val : string -> bool -> string
      end
      |}
  in
  let diff_opt = Diff.interface ~module_name:"Main" ~reference ~current in
  let diff = Option.get diff_opt in
  let text_diff = Text_diff.from_diff diff in
  Format.printf "%a" Text_diff.pp text_diff;
  [%expect
    {|
    diff module Main:
    +val f :
    +  some_long_labeled_argument:int ->
    +  some_other_long_labeled_arg:int * int -> string * string -> unit -> string
    +module M: sig
    +  val some_val : int -> int -> int -> string
    +  val some_other_val : string -> string -> string -> int
    +  val yet_some_other_val : string -> bool -> string
    +end
    |}]
