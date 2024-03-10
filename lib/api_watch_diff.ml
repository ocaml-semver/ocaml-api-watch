let diff_interface ~reference ~current = 
  let current = Cmi_format.read_cmi current in
  let reference = Cmi_format.read_cmi reference in
  let typing_env = Env.empty in
  let coercion1 () =
    Includemod.signatures typing_env ~mark:Mark_both reference.cmi_sign
      current.cmi_sign
  in
  let coercion2 () =
    Includemod.signatures typing_env ~mark:Mark_both current.cmi_sign
      reference.cmi_sign
  in 
  match (coercion1 (), coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> false
  | _, _ -> true
  | exception Includemod.Error _ -> true
  