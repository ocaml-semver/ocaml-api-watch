let diff_interface ~reference ~current =
  let typing_env = Env.empty in
  let coercion1 () =
    Includemod.signatures typing_env ~mark:Mark_both reference current
  in
  let coercion2 () =
    Includemod.signatures typing_env ~mark:Mark_both current reference
  in
  match (coercion1 (), coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> false
  | _, _ -> true
  | exception Includemod.Error _ -> true
