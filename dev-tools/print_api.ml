let print_cmi path =
  let cmi_infos = Cmi_format.read_cmi path in
  Format.printf "cmi_name: %s\n" cmi_infos.cmi_name;
  Format.printf "cmi_sign:\n";
  Printtyp.signature Format.std_formatter cmi_infos.cmi_sign;
  Format.printf "\n"

let all_cmi_files path =
  Sys.readdir path |> Array.to_list
  |> List.filter (fun p -> Filename.check_suffix p ".cmi")
  |> List.map (Filename.concat path)

let run (`Main_module main_module) (`Input fn) =
  let open CCResult.Infix in
  match (Sys.is_directory fn, main_module) with
  | false, _ ->
      print_cmi fn;
      Ok ()
  | true, None ->
      let cmi_files = all_cmi_files fn in
      List.iter print_cmi cmi_files;
      Ok ()
  | true, Some main_module ->
      let+ sig_map = Api_watch.Library.load ~main_module fn in
      let sig_ = Api_watch.String_map.find main_module sig_map in 
      Printtyp.signature Format.std_formatter sig_;
      Format.printf "\n"

let named f = Cmdliner.Term.(app (const f))

let main_module =
  let docv = "MAIN_MODULE_NAME" in
  let doc =
    "The name of the library's main module. Ignored when input is a $(b,.cmi) \
     file"
  in
  named
    (fun x -> `Main_module x)
    Cmdliner.Arg.(
      value & opt (some string) None & info ~doc ~docv [ "main-module" ])

let input_file =
  let docv = "PATH" in
  let doc =
    "Path to the $(b,.cmi) file or lib directory. If $(docv) is directory\n\
    \     and no $(b,--main-module) is provided, prints the API of all \
     $(b,.cmi) files.\n\
    \     If $(b,--main-module) is provided, prints the public API of the \
     library."
  in
  named
    (fun x -> `Input x)
    Cmdliner.Arg.(required & pos 0 (some file) None & info ~doc ~docv [])

let info =
  let open Cmdliner in
  Cmd.info "print_api" ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"Pretty prints the API of a $(b,.cmi) file or a whole library"

let term = Cmdliner.Term.(const run $ main_module $ input_file)

let () =
  let exit_code = Cmdliner.Cmd.eval_result (Cmdliner.Cmd.v info term) in
  exit exit_code
