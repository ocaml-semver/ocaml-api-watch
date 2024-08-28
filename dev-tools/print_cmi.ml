let run (`Input_file fn) =
  let cmi_infos = Cmi_format.read_cmi fn in
  Format.printf "cmi_name: %s\n" cmi_infos.cmi_name;
  Format.printf "cmi_sign:\n";
  Printtyp.printed_signature fn Format.std_formatter cmi_infos.cmi_sign;
  Format.printf "\n"

let named f = Cmdliner.Term.(app (const f))

let input_file =
  let docv = "CMI_FILE" in
  let doc = "Path to the $(b,.cmi) file to print" in
  named
    (fun x -> `Input_file x)
    Cmdliner.Arg.(required & pos 0 (some non_dir_file) None & info ~doc ~docv [])

let info =
  let open Cmdliner in
  Cmd.info "print_cmi" ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"Pretty prints the content of a $(b,.cmi) file"

let term = Cmdliner.Term.(const run $ input_file)

let () =
  let exit_code = Cmdliner.Cmd.eval (Cmdliner.Cmd.v info term) in
  exit exit_code
