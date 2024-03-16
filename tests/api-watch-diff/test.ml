let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

(* Test Api_watch_diff.diff_interface with the initial mod_ref_signature *)
open Types

let%expect_test "Initial module signature test" =
  let module_identifier = Ident.create_persistent "M" in
  let value_identifier = Ident.create_persistent "int" in
  let int_type_expr =
    Tconstr (Path.Pident (value_identifier), [], ref Mnil)
  in
  let value_description :value_description =
    { val_type = int_type_expr;
      val_kind = Val_reg
      val_attributes = [];
      val_loc = Location.none;
      val_uid = Uid.create ()
    }
  in
  let module_type =
    Mty_signature [Sig_value (value_identifier, value_description, Public)]
  in
  let mod_ref_signature : signature =
    [Sig_module (module_identifier,
                       Mp_present,
                       module_type,
                       Nonrecursive,
                       Public)]
  in
  let diff_result = Api_watch_diff.diff_interface [mod_ref_signature] in

  Format.printf "%b" diff_result;
  [%expect {| API unchanged! |}]
