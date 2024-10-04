let tool_name = "api-diff"

let run (`Main_module main_module) (`Ref_cmi reference) (`Current_cmi current) =
  let open CCResult.Infix in
  let* reference_sig, current_sig, module_name =
    match
      (Sys.is_directory reference, Sys.is_directory current, main_module)
    with
    | true, true, Some main_module ->
        let main_module = String.capitalize_ascii main_module in
        let+ reference_sig = Api_watch.Library.load ~main_module reference
        and+ current_sig = Api_watch.Library.load ~main_module current in
        let module_name = String.capitalize_ascii main_module in
        (reference_sig, current_sig, module_name)
    | false, false, main_module ->
        let () =
          match main_module with
          | None -> ()
          | Some _ ->
              Printf.eprintf
                "%s: --main-module ignored when diffing single .cmi files\n"
                tool_name
        in
        let+ reference_cmi, _ = Api_watch.Library.load_cmi reference
        and+ current_cmi, module_name = Api_watch.Library.load_cmi current in
        (reference_cmi, current_cmi, module_name)
    | true, false, _ | false, true, _ ->
        Error
          "Arguments must either both be directories or both single .cmi files."
    | true, true, None ->
        Error "--main-module must be provided when diffing entire libraries."
  in
  let diff =
    Api_watch.Diff.interface ~module_name ~reference:reference_sig
      ~current:current_sig
  in
  match diff with
  | None -> Ok 0
  | Some diff ->
      Api_watch.Text_diff.With_colors.pp Format.std_formatter diff;
      Ok 1

let named f = Cmdliner.Term.(app (const f))

let main_module =
  let docv = "MAIN_MODULE_NAME" in
  let doc =
    "The name of the library's main module. Ignored when diffing single \
     $(b,.cmi) files."
  in
  named
    (fun x -> `Main_module x)
    Cmdliner.Arg.(
      value & opt (some string) None & info ~doc ~docv [ "main-module" ])

let ref_cmi =
  let docv = "REF_CMI_FILES" in
  let doc =
    "A single $(b,.cmi) file or a directory containing all cmi files for the \
     reference version"
  in
  named
    (fun x -> `Ref_cmi x)
    Cmdliner.Arg.(required & pos 0 (some file) None & info ~doc ~docv [])

let current_cmi =
  let docv = "CURRENT_CMI_FILES" in
  let doc =
    "A single $(b,.cmi) file or a directory containing all cmi files for the \
     current version"
  in
  named
    (fun x -> `Current_cmi x)
    Cmdliner.Arg.(required & pos 1 (some file) None & info ~doc ~docv [])

let info =
  let open Cmdliner in
  Cmd.info tool_name ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"List API changes between two versions of a library"

let term = Cmdliner.Term.(const run $ main_module $ ref_cmi $ current_cmi)

let () =
  Fmt_tty.setup_std_outputs ();
  let exit_code = Cmdliner.Cmd.eval_result' (Cmdliner.Cmd.v info term) in
  exit exit_code
