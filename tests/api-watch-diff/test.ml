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
      Sig_module
        ( Ident.create_persistent "N",
          Mp_present,
          {
            md_type =
              Mty_signature
                [
                  Sig_value
                    ( Ident.create_persistent "y",
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
                    ( Ident.create_persistent "x",
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
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Misc.Fatal_error)
  Raised at Misc.fatal_errorf.(fun) in file "utils/misc.ml", line 22, characters 14-31
  Called from Subst.rename_bound_idents.rename_bound_idents in file "typing/subst.ml", line 538, characters 18-33
  Called from Subst.force_signature_once' in file "typing/subst.ml", line 661, characters 18-50
  Called from Lazy_backtrack.force in file "utils/lazy_backtrack.ml", line 34, characters 12-15
  Re-raised at Lazy_backtrack.force in file "utils/lazy_backtrack.ml", line 40, characters 8-15
  Called from Subst.force_signature_once in file "typing/subst.ml", line 650, characters 18-65
  Called from Subst.force_signature in file "typing/subst.ml", line 647, characters 32-57
  Called from Subst.force_modtype in file "typing/subst.ml", line 604, characters 39-59
  Called from Includemod.modtypes in file "typing/includemod.ml", line 426, characters 15-50
  Called from Includemod.signature_components in file "typing/includemod.ml", line 758, characters 16-106
  Called from Includemod.signatures.pair_components in file "typing/includemod.ml", line 653, characters 10-134
  Called from Includemod.signatures in file "typing/includemod.ml", line 1206, characters 8-111
  Called from Api_watch_diff.diff_interface.coercion2 in file "lib/api_watch_diff.ml", line 7, characters 4-70
  Called from Test.(fun) in file "tests/api-watch-diff/test.ml", line 176, characters 4-98
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  >> Fatal error: Ident.rename x |}]
