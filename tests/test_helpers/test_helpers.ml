(* Parses and tokenizes the contents of the .mli file*)
let parse_interface content =
  let lexbuf = Lexing.from_string content in
  Parse.interface lexbuf

(* Generate the Types.signature from the parsed and typedtreed interface *)
let generate_signature intf =
  let env =
    Typemod.initial_env ~loc:Location.none ~initially_opened_module:None
      ~open_implicit_modules:[]
  in
  let typed_tree = Typemod.type_interface env intf in
  typed_tree.sig_type

(* Compile the .mli content and return the signature *)
let compile_interface (content : string) : Types.signature =
  let intf = parse_interface content in
  let signature = generate_signature intf in
  signature