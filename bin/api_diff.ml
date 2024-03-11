let run (`Ref_cmi reference) (`Current_cmi current) =
  let current = Cmi_format.read_cmi current in
  let reference = Cmi_format.read_cmi reference in
  let differs =
    Api_watch_diff.diff_interface ~reference:reference.cmi_sign
      ~current:current.cmi_sign
  in
  if differs then Printf.printf "API changed!\n"
  else Printf.printf "API unchanged!\n"

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
  let open Cmdliner in
  Cmd.info "api-watcher" ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"List API changes between two versions of a library"

let term = Cmdliner.Term.(const run $ ref_cmi $ current_cmi)
let main = Cmdliner.Cmd.v info term
let () = Stdlib.exit @@ Cmdliner.Cmd.eval main
