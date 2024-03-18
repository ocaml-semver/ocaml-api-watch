let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

(* Test Api_watch_diff.diff_interface with the initial mod_ref_signature *)
open Types

let%expect_test "Initial module signature test" =
  let mod_ref_signature : signature =
    [
      Sig_module
        ( Ident.create_persistent "M",
          Mp_present,
          {
            md_type =
              Mty_signature
                [
                  Sig_value
                    ( Ident.create_persistent "x",
                      {
                        val_type =
                          Transient_expr.type_expr
                            (Transient_expr.create
                               (Tconstr
                                  ( Path.Pident (Ident.create_persistent "int"),
                                    [],
                                    ref Mnil ))
                               ~level:0 ~scope:0 ~id:0);
                        val_kind = Val_reg;
                        val_attributes = [];
                        val_loc = Location.none;
                        val_uid = Uid.internal_not_actually_unique;
                      },
                      Exported );
                ];
            md_attributes = [];
            md_loc = Location.none;
            md_uid = Uid.internal_not_actually_unique;
          },
          Trec_not,
          Exported );
    ]
  in
  let result =
    Api_watch_diff.diff_interface ~reference:mod_ref_signature
      ~current:mod_ref_signature
  in
  Format.printf "%b" result;
  [%expect {|false|}]
