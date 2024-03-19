let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

open Types

(* The reference mod_ref_signature *)
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
                  ( Ident.create_local "x",
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

(* Test Api_watch_diff.diff_interface with the reference mod_ref_signature *)
let%expect_test "Initial module signature test" =
  let result =
    Api_watch_diff.diff_interface ~reference:mod_ref_signature
      ~current:mod_ref_signature
  in
  Format.printf "%b" result;
  [%expect {|false|}]

(* Test Api_watch_diff.diff_interface with the interface mod_add_signature *)
let%expect_test "Generate a new .mli file with an additional module" =
  let mod_add_signature : signature =
    [
      Sig_module
        ( Ident.create_persistent "M",
          Mp_present,
          {
            md_type =
              Mty_signature
                [
                  Sig_value
                    ( Ident.create_local "x",
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
      Sig_module
        ( Ident.create_persistent "N",
          Mp_present,
          {
            md_type =
              Mty_signature
                [
                  Sig_value
                    ( Ident.create_local "y",
                      {
                        val_type =
                          Transient_expr.type_expr
                            (Transient_expr.create
                               (Tconstr
                                  ( Path.Pident (Ident.create_persistent "float"),
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
      ~current:mod_add_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Test Api_watch_diff.diff_interface with the interface mod_remove_signature *)
let%expect_test "Remove a module from the interface" =
  let mod_remove_signature : signature = [] in
  let result =
    Api_watch_diff.diff_interface ~reference:mod_ref_signature
      ~current:mod_remove_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Test Api_watch_diff.diff_interface with the interface mod_modify_signature *)
let%expect_test "Modify a module in the interface" =
  let mod_modify_signature : signature =
    [
      Sig_module
        ( Ident.create_persistent "M",
          Mp_present,
          {
            md_type =
              Mty_signature
                [
                  Sig_value
                    ( Ident.create_local "x",
                      {
                        val_type =
                          Transient_expr.type_expr
                            (Transient_expr.create
                               (Tconstr
                                  ( Path.Pident (Ident.create_persistent "float"),
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
      ~current:mod_modify_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]
