let diff_interface ~reference:_ ~current:_ = 
    let typing_env = Env.empty in
    let coercion =
        Includemod.signatures typing_env ~mark:Mark_both reference.cmi_sign 
        current.cmi_sign
