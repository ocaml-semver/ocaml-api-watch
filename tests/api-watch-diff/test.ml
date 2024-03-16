let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

open Types

let%expect_test "api-diff ref.cmi ref.cmi" =
  let signature1 =
    [
      Sig_type
        ( Global "t",
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract;
            type_private = Public;
            type_manifest =
              Some
                (Btype.newgenty
                   (Tconstr (Path.Pident (Ident.create "int"), [], ref Mnil)));
            type_variance = [];
            type_is_newtype = false;
            type_expansion = None;
            type_loc = Location.none;
            type_attributes = [];
            type_immediate = Unknown;
            type_unboxed = Default_unboxed;
          },
          Trec_not,
          Exported );
    ]
  in
  let signature2 = signature1 in
  Api_watch_diff.diff_interface signature1 signature2;
  [%expect {| API unchanged! |}]
