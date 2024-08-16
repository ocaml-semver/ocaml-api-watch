let rec collect_cmi_files dir =
  try
    let files = Sys.readdir dir in
    Array.fold_left
      (fun acc file ->
        let path = Filename.concat dir file in
        try
          if Sys.is_directory path then acc @ collect_cmi_files path
          else if Filename.check_suffix file ".cmi" then path :: acc
          else acc
        with Sys_error e ->
          Printf.eprintf "Error processing %s: %s\n" path e;
          acc)
      [] files
  with Sys_error e ->
    Printf.eprintf "Error reading directory %s: %s\n" dir e;
    []

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok cmi_infos.cmi_sign
  with e -> Error (Printexc.to_string e)

let module_name_of_file file_path =
  file_path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let find_build_artifacts project_path =
  let build_dir = Filename.concat project_path "_build" in
  if not (Sys.file_exists build_dir) then
    Printf.eprintf "Build directory not found: %s\n" build_dir;
  let default_dir = Filename.concat build_dir "default" in
  let install_dir = Filename.concat build_dir "install/default/lib" in
  if Sys.file_exists install_dir then collect_cmi_files install_dir
  else if Sys.file_exists default_dir then collect_cmi_files default_dir
  else collect_cmi_files build_dir

let load project_path =
  let cmi_files = find_build_artifacts project_path in
  let signatures =
    List.filter_map
      (fun cmi_file ->
        let module_name = module_name_of_file cmi_file in
        match load_cmi cmi_file with
        | Ok signature -> Some (module_name, signature)
        | Error e ->
            Printf.eprintf "Failed to load %s: %s\n" cmi_file e;
            None)
      cmi_files
  in
  let merged_signature =
    List.fold_left
      (fun acc (module_name, signature) ->
        String_map.add module_name signature acc)
      String_map.empty signatures
  in
  String_map.fold
    (fun module_name module_sig acc ->
      Types.Sig_module
        ( Ident.create_local module_name,
          Mp_present,
          {
            md_type = Mty_signature module_sig;
            md_attributes = [];
            md_loc = Location.none;
            md_uid = Types.Uid.internal_not_actually_unique;
          },
          Trec_not,
          Exported )
      :: acc)
    merged_signature []
