let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

open Types

(* Signature for type t:
     > type t = int *)

let t_sig =
  Sig_type
    ( Ident.create_persistent "t",
      {
        type_params = [];
        type_arity = 0;
        type_kind = Type_abstract;
        type_private = Public;
        type_manifest =
          Some
            (create_expr
               (Tconstr (Predef.path_int, [], ref Mnil))
               ~level:0 ~scope:0 ~id:0);
        type_variance = [];
        type_separability = [];
        type_is_newtype = false;
        type_expansion_scope = 0;
        type_loc = Location.none;
        type_attributes = [];
        type_immediate = Unknown;
        type_unboxed_default = false;
        type_uid = Uid.internal_not_actually_unique;
      },
      Trec_not,
      Exported )

(* Signature for type t:
     > type unused_type = string *)

let unused_type_sig =
  Sig_type
    ( Ident.create_persistent "unused_type",
      {
        type_params = [];
        type_arity = 0;
        type_kind = Type_abstract;
        type_private = Public;
        type_manifest =
          Some
            (create_expr
               (Tconstr (Predef.path_string, [], ref Mnil))
               ~level:0 ~scope:0 ~id:1);
        type_variance = [];
        type_separability = [];
        type_is_newtype = false;
        type_expansion_scope = 0;
        type_loc = Location.none;
        type_attributes = [];
        type_immediate = Unknown;
        type_unboxed_default = false;
        type_uid = Uid.internal_not_actually_unique;
      },
      Trec_not,
      Exported )

(* Signature for value f:
    > val f : t -> string *)

let val_f_sig =
  Sig_value
    ( Ident.create_persistent "f",
      {
        val_type =
          create_expr
            (Tarrow
               ( Nolabel,
                 create_expr
                   (Tconstr
                      (Path.Pident (Ident.create_persistent "t"), [], ref Mnil))
                   ~level:0 ~scope:0 ~id:2,
                 create_expr
                   (Tconstr (Predef.path_string, [], ref Mnil))
                   ~level:0 ~scope:0 ~id:3,
                 commu_ok ))
            ~level:0 ~scope:0 ~id:4;
        val_kind = Val_reg;
        val_loc = Location.none;
        val_attributes = [];
        val_uid = Uid.internal_not_actually_unique;
      },
      Exported )

(* Signature for value g:
    > val g : t -> t *)

let val_g_sig =
  Sig_value
    ( Ident.create_persistent "g",
      {
        val_type =
          create_expr
            (Tarrow
               ( Nolabel,
                 create_expr
                   (Tconstr
                      (Path.Pident (Ident.create_persistent "t"), [], ref Mnil))
                   ~level:0 ~scope:0 ~id:5,
                 create_expr
                   (Tconstr
                      (Path.Pident (Ident.create_persistent "t"), [], ref Mnil))
                   ~level:0 ~scope:0 ~id:6,
                 commu_ok ))
            ~level:0 ~scope:0 ~id:7;
        val_kind = Val_reg;
        val_loc = Location.none;
        val_attributes = [];
        val_uid = Uid.internal_not_actually_unique;
      },
      Exported )

(* Signature for file ref.mli:
      > type t = int
      > type unused_type = string
      > val f : t -> string *)

let ref_signature = [ t_sig; unused_type_sig; val_f_sig ]

(* Signature for add_value.mli:
     > type t = int
     > type unused_type = string
     > val f : t -> string
     > val g : t -> t *)

let add_value_signature = [ t_sig; unused_type_sig; val_f_sig; val_g_sig ]

let%expect_test "adding_a_value_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:add_value_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Signature for remove_value.mli:
     > type t = int
     > type unused_type = string *)

let remove_value_signature = [ t_sig; unused_type_sig ]

let%expect_test "removing_a_value_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:remove_value_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Signature for value f:
    > val f : t -> t *)

let modified_val_f_sig =
  Sig_value
    ( Ident.create_persistent "f",
      {
        val_type =
          create_expr
            (Tarrow
               ( Nolabel,
                 create_expr
                   (Tconstr
                      (Path.Pident (Ident.create_persistent "t"), [], ref Mnil))
                   ~level:0 ~scope:0 ~id:8,
                 create_expr
                   (Tconstr
                      (Path.Pident (Ident.create_persistent "t"), [], ref Mnil))
                   ~level:0 ~scope:0 ~id:9,
                 commu_ok ))
            ~level:0 ~scope:0 ~id:10;
        val_kind = Val_reg;
        val_loc = Location.none;
        val_attributes = [];
        val_uid = Uid.internal_not_actually_unique;
      },
      Exported )

(* Signature for modify_value.mli:
     > type t = int
     > type unused_type = string
     > val f : t -> t *)

let modify_value_signature = [ t_sig; unused_type_sig; modified_val_f_sig ]

let%expect_test "modifying_a_value_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:modify_value_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Signature for mod_ref.mli:
     > module M : sig val x : int end *)
let ref_module_signature : signature =
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
                        create_expr
                          (Tconstr (Predef.path_int, [], ref Mnil))
                          ~level:0 ~scope:0 ~id:0;
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

(* Signature for new module prepended to mod_ref.mli:
     > module N : sig val y : float end *)

let new_module : signature =
  [
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
                        create_expr
                          (Tconstr (Predef.path_float, [], ref Mnil))
                          ~level:0 ~scope:0 ~id:0;
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

(* Signature for add_module.mli:
     > module M : sig val x : int end
     > module N : sig val y : float end *)

let add_module_signature = new_module @ ref_module_signature

let%expect_test "adding_a_module_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:add_module_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Signature for remove_module.mli:
     > *)

let remove_module_signature : signature = []

let%expect_test "removing_a_module_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:remove_module_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

(* Signature for modify_module.mli:
    > module M : sig val x : float end *)

let modify_ref_module_signature : signature =
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
                        create_expr
                          (Tconstr (Predef.path_float, [], ref Mnil))
                          ~level:0 ~scope:0 ~id:0;
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

let%expect_test "modifying_a_module_test" =
  let result =
    Api_watch_diff.diff_interface ~reference:ref_module_signature
      ~current:modify_ref_module_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]
