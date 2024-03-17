let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

open Types

let%expect_test "api-diff ref.cmi ref.cmi" =
  let signature1 =
    [
      Sig_type
        ( Ident.create_persistent "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest =
              Some
                (Transient_expr.type_expr
                   (Transient_expr.create (Tvar (Some "int")) ~level:(-1)
                      ~scope:(-1) ~id:(-1)));
            type_variance = [];
            type_separability = [];
            type_is_newtype = false;
            type_expansion_scope = -1;
            type_loc = Location.none;
            type_attributes = [];
            type_immediate = Unknown;
            type_unboxed_default = false;
            type_uid = Uid.internal_not_actually_unique;
          },
          Trec_not,
          Exported );
    ]
  in
  let signature2 = signature1 in
  let result =
    Api_watch_diff.diff_interface ~reference:signature1 ~current:signature2
  in
  Format.printf "%b" result;
  [%expect {|false|}]
