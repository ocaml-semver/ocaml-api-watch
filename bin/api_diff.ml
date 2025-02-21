let tool_name = "api-diff"

type mode = Unwrapped | Wrapped of string | Cmi

let both_directories reference current =
  match (Sys.is_directory reference, Sys.is_directory current) with
  | true, true -> Ok true
  | false, false -> Ok false
  | _ ->
      Error
        "Arguments must either both be directories or both single .cmi files."

let print_warning main_module unwrapped =
  match (main_module, unwrapped) with
  | None, false -> ()
  | Some _, false ->
      Printf.eprintf
        "%s: --main-module is ignored when diffing single .cmi files\n"
        tool_name
  | None, true ->
      Printf.eprintf
        "%s: --unwrapped is ignored when diffing single .cmi files\n" tool_name
  | Some _, true ->
      Printf.eprintf
        "%s: --main-module and --unwrapped are ignored when diffing single \
         .cmi files\n"
        tool_name

let mode ~reference ~current ~main_module ~unwrapped =
  let open CCResult.Infix in
  let* both_dirs = both_directories reference current in
  match (both_dirs, main_module, unwrapped) with
  | true, Some main_module, false -> Ok (Wrapped main_module)
  | true, None, true -> Ok Unwrapped
  | false, main_module, unwrapped ->
      print_warning main_module unwrapped;
      Ok Cmi
  | true, _, _ ->
      Error
        "Either --main-module or --unwrapped must be provided when diffing \
         entire libraries."

let run (`Word_diff word_diff) (`Main_module main_module)
    (`Unwrapped_library unwrapped) (`Ref_cmi reference) (`Current_cmi current) =
  let open CCResult.Infix in
  let* reference_map, current_map =
    let* curr_mode = mode ~reference ~current ~main_module ~unwrapped in
    match curr_mode with
    | Wrapped main_module ->
        let main_module = String.capitalize_ascii main_module in
        let+ reference_map = Api_watch.Library.load ~main_module reference
        and+ current_map = Api_watch.Library.load ~main_module current in
        (reference_map, current_map)
    | Unwrapped ->
        let+ reference_map = Api_watch.Library.load_unwrapped reference
        and+ current_map = Api_watch.Library.load_unwrapped current in
        (reference_map, current_map)
    | Cmi ->
        let+ reference_cmi, _ = Api_watch.Library.load_cmi reference
        and+ current_cmi, module_name = Api_watch.Library.load_cmi current in
        let reference_map =
          Api_watch.String_map.singleton module_name reference_cmi
        in
        let current_map =
          Api_watch.String_map.singleton module_name current_cmi
        in
        (reference_map, current_map)
  in
  let diff_map =
    Api_watch.Diff.library ~reference:reference_map ~current:current_map
    |> Api_watch.String_map.bindings
    |> List.filter_map (fun (_, v) -> v)
  in
  let has_changes = not (List.is_empty diff_map) in
  List.iter
    (fun diff ->
      let text_diff = Api_watch.Text_diff.from_diff diff in
      if word_diff then
        Api_watch.Text_diff.Word.pp Format.std_formatter text_diff
      else Api_watch.Text_diff.With_colors.pp Format.std_formatter text_diff)
    diff_map;
  if has_changes then Ok 1 else Ok 0

let named f = Cmdliner.Term.(app (const f))

let word_diff =
  let doc =
    "Show changes in a signature item inline, where a removed part of an item \
     is wrapped in\n\
    \      $(b,[-removed-]) and an added one is wrapped in $(b,{+added+})"
  in
  named
    (fun x -> `Word_diff x)
    Cmdliner.Arg.(value & flag & info ~doc [ "word-diff" ])

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

let unwrapped_library =
  let doc =
    "Loads a library without a main module. Ignored when diffing single \
     $(b,.cmi) files."
  in
  named
    (fun x -> `Unwrapped_library x)
    Cmdliner.Arg.(value & flag & info ~doc [ "unwrapped" ])

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

let term =
  Cmdliner.Term.(
    const run $ word_diff $ main_module $ unwrapped_library $ ref_cmi
    $ current_cmi)

let () =
  Fmt_tty.setup_std_outputs ();
  let exit_code = Cmdliner.Cmd.eval_result' (Cmdliner.Cmd.v info term) in
  exit exit_code
