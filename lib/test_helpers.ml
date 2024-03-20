open Parsetree
open Typedtree
open Types

(* Helper library to parse .mli content and return Types.signature *)
let compile_interface (content : string) : Types.signature =
  (* Parses and tokenizes the contents of the .mli file*)
  let parse_interface content =
    let lexbuf = Lexing.from_string content in
    try Parse.interface lexbuf
    with exn ->
      failwith ("Error while parsing .mli content: " ^ Printexc.to_string exn)
  in
  (* Create an env and generate a typedtree representation from the parsed interface *)
  let typedtree_of_interface intf =
    let initial_env = Compmisc.initial_env () in
    let env =
      Typemod.initial_env ~initially_opened_module:None
        ~open_implicit_modules:[] ~loc:Location.none
    in
    let _mod_info = Typemod.type_interface env intf in
    env
  in
  (* Main function: parse content, perform type checking, and return signature *)
  let intf = parse_interface content in
  let env = typedtree_of_interface intf in
  signature (* To be resolved *)
