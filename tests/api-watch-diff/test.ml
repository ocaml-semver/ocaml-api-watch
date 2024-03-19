open Types
open Predef

let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

let ref_signature =
  (* Signature for ref.mli:
     > type t = int
     > type unused_type = string
     > val f : t-> string *)
  [
    Sig_type
      ( Ident.create_persistent "t",
        {
          type_params = [];
          type_arity = 0;
          type_kind = Type_abstract;
          type_private = Public;
          type_manifest = Some type_int;
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
        Exported );
    Sig_type
      ( Ident.create_persistent "unused_type",
        {
          type_params = [];
          type_arity = 0;
          type_kind = Type_abstract;
          type_private = Public;
          type_manifest = Some type_string;
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
        Exported );
    Sig_value
      ( Ident.create_persistent "f",
        {
          val_type =
            Transient_expr.type_expr
              (Transient_expr.create
                 (Tarrow
                    ( Nolabel,
                      Transient_expr.type_expr
                        (Transient_expr.create
                           (Tconstr
                              ( Path.Pident (Ident.create_persistent "t"),
                                [],
                                ref Mnil ))
                           ~level:0 ~scope:0 ~id:0),
                      type_string,
                      commu_ok ))
                 ~level:0 ~scope:0 ~id:0);
          val_kind = Val_reg;
          val_loc = Location.none;
          val_attributes = [];
          val_uid = Uid.internal_not_actually_unique;
        },
        Exported );
  ]

let%expect_test "Testing API watch on the same signature" =
  let curr_signature =
    (* Signature for curr.mli:
       > type t = int
       > type unused_type = string
       > val f : t-> string *)
    [
      Sig_type
        ( Ident.create_persistent "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_int;
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
          Exported );
      Sig_type
        ( Ident.create_persistent "unused_type",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_string;
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
          Exported );
      Sig_value
        ( Ident.create_persistent "f",
          {
            val_type =
              Transient_expr.type_expr
                (Transient_expr.create
                   (Tarrow
                      ( Nolabel,
                        Transient_expr.type_expr
                          (Transient_expr.create
                             (Tconstr
                                ( Path.Pident (Ident.create_persistent "t"),
                                  [],
                                  ref Mnil ))
                             ~level:0 ~scope:0 ~id:0),
                        type_string,
                        commu_ok ))
                   ~level:0 ~scope:0 ~id:0);
            val_kind = Val_reg;
            val_loc = Location.none;
            val_attributes = [];
            val_uid = Uid.internal_not_actually_unique;
          },
          Exported );
    ]
  in

  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:curr_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

let%expect_test "Testing API watch on adding a type" =
  let curr_signature =
    (* Signature for curr.mli:
       > type t = int
       > type unused_type = string
       > type added_type = float
       > val f : t-> string *)
    [
      Sig_type
        ( Ident.create_persistent "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_int;
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
          Exported );
      Sig_type
        ( Ident.create_persistent "unused_type",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_string;
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
          Exported );
      Sig_type
        ( Ident.create_persistent "added_type",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_float;
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
          Exported );
      Sig_value
        ( Ident.create_persistent "f",
          {
            val_type =
              Transient_expr.type_expr
                (Transient_expr.create
                   (Tarrow
                      ( Nolabel,
                        Transient_expr.type_expr
                          (Transient_expr.create
                             (Tconstr
                                ( Path.Pident (Ident.create_predef "t"),
                                  [],
                                  ref Mnil ))
                             ~level:0 ~scope:0 ~id:0),
                        type_string,
                        commu_ok ))
                   ~level:0 ~scope:0 ~id:0);
            val_kind = Val_reg;
            val_loc = Location.none;
            val_attributes = [];
            val_uid = Uid.internal_not_actually_unique;
          },
          Exported );
    ]
  in
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:curr_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

let%expect_test "Testing API watch on removing a type" =
  let curr_signature =
    (* Signature for curr.mli:
       > type t = int
       > val f : t-> string *)
    [
      Sig_type
        ( Ident.create_persistent "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_int;
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
          Exported );
      Sig_value
        ( Ident.create_persistent "f",
          {
            val_type =
              Transient_expr.type_expr
                (Transient_expr.create
                   (Tarrow
                      ( Nolabel,
                        Transient_expr.type_expr
                          (Transient_expr.create
                             (Tconstr
                                ( Path.Pident (Ident.create_predef "t"),
                                  [],
                                  ref Mnil ))
                             ~level:0 ~scope:0 ~id:0),
                        type_string,
                        commu_ok ))
                   ~level:0 ~scope:0 ~id:0);
            val_kind = Val_reg;
            val_loc = Location.none;
            val_attributes = [];
            val_uid = Uid.internal_not_actually_unique;
          },
          Exported );
    ]
  in
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:curr_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]

let%expect_test "Testing API watch on modifying a type" =
  let curr_signature =
    (* Signature for curr.mli:
       > type t = float
       > type unused_type = string
       > val f : t-> string *)
    [
      Sig_type
        ( Ident.create_persistent "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_float;
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
          Exported );
      Sig_type
        ( Ident.create_persistent "unused_type",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest = Some type_string;
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
          Exported );
      Sig_value
        ( Ident.create_persistent "f",
          {
            val_type =
              Transient_expr.type_expr
                (Transient_expr.create
                   (Tarrow
                      ( Nolabel,
                        Transient_expr.type_expr
                          (Transient_expr.create
                             (Tconstr
                                ( Path.Pident (Ident.create_predef "t"),
                                  [],
                                  ref Mnil ))
                             ~level:0 ~scope:0 ~id:0),
                        type_string,
                        commu_ok ))
                   ~level:0 ~scope:0 ~id:0);
            val_kind = Val_reg;
            val_loc = Location.none;
            val_attributes = [];
            val_uid = Uid.internal_not_actually_unique;
          },
          Exported );
    ]
  in
  let result =
    Api_watch_diff.diff_interface ~reference:ref_signature
      ~current:curr_signature
  in
  Format.printf "%b" result;
  [%expect {|true|}]
