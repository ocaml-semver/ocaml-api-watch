let collect_cmi_files dir =
  try
    let files = Sys.readdir dir in
    Ok
      (Array.fold_left
         (fun acc file ->
           let path = Filename.concat dir file in
           if (not (Sys.is_directory path)) && Filename.check_suffix file ".cmi"
           then path :: acc
           else acc)
         [] files)
  with Sys_error e ->
    Error (Printf.sprintf "Error reading directory %s: %s" dir e)

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok (cmi_infos.cmi_sign, cmi_infos.cmi_name)
  with e -> Error (Printexc.to_string e)

let load project_path =
  let open CCResult.Infix in
  let* cmi_files = collect_cmi_files project_path in
  let* signatures =
    CCResult.map_l
      (fun cmi_file ->
        let+ signature, module_name = load_cmi cmi_file in
        (module_name, signature))
      cmi_files
  in
  let merged_signature =
    List.fold_left
      (fun acc (module_name, signature) ->
        String_map.add module_name signature acc)
      String_map.empty signatures
  in
  Ok
    (String_map.fold
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
       merged_signature [])
