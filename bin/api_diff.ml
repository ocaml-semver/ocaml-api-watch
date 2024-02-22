let run (`Ref_cmi reference) (`Current_cmi current) =
  let current = Cmi_format.read_cmi current in
  let reference = Cmi_format.read_cmi reference in
  let typing_env = Env.empty in
  let coercion =
    Includemod.signatures typing_env ~mark:Mark_both reference.cmi_sign
      current.cmi_sign
  in
  let () =
    match coercion with
    | Tcoerce_none -> Printf.printf "API unchanged!\n"
    | _ -> Printf.printf "API changed!\n"
  in
  0

let named f = Cmdliner.Term.(app (const f))

let ref_cmi =
  let docv = "CMI" in
  let doc = "The .cmi for the reference version" in
  named
    (fun x -> `Ref_cmi x)
    Cmdliner.Arg.(required & pos 0 (some file) None & info ~doc ~docv [])

let current_cmi =
  let docv = "CMI" in
  let doc = "The .cmi for the current version" in
  named
    (fun x -> `Current_cmi x)
    Cmdliner.Arg.(required & pos 1 (some file) None & info ~doc ~docv [])

let info =
  Cmdliner.Term.info "api-watcher" ~version:"%%VERSION%%"
    ~exits:Cmdliner.Term.default_exits
    ~doc:"List API changes between two versions of a library"

let term = Cmdliner.Term.(const run $ ref_cmi $ current_cmi)

let main = (term, info)

let () = Cmdliner.Term.(exit_status @@ eval main)
