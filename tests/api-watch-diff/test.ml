let%expect_test "test_diff_interface" =
  let result = Api_watch_diff.diff_interface ~reference:[] ~current:[] in
  Format.printf "%b" result;
  [%expect {|false|}]

open Types

let%expect_test "adding_value_test" =

  let signature1=
  [
    Sig_type(
      Ident.create_persistent "t",
      {
        type_params = []; 
        type_arity = 0;  
        type_kind = Type_abstract ;  
        type_private = Public;  
        type_manifest = Some {
                  desc = Tconstr (Path.Pident "int", [], ref []);
                  level = 0;
                  scope =0;
                  id = 1;
                };
        type_variance = [];    
        type_separability= [];
        type_is_newtype= false;
        type_expansion_scope= 0;
        type_loc= Location.none;
        type_attributes= [];
        type_immediate= Unknown;
        type_unboxed_default= false;
        type_uid= Uid.create();
      },
      Trec_not,
      Exported
    );
    Sig_type(
      Ident.create_persistent "unused_type",
      {
        type_params = []; 
        type_arity = 0;  
        type_kind = Type_abstract;  
        type_private = Public;  
        type_manifest =      
        type_variance = [];    
        type_separability= [];
        type_is_newtype= false;
        type_expansion_scope= 0;
        type_loc= Location.none;
        type_attributes= [];
        type_immediate= Unknown;
        type_unboxed_default= false;
        type_uid= Uid.create();
      },
      Trec_not,
      Exported
    ); 
    Sig_value(
      Ident.create_persistent "f",
      {
        val_type= Tarrow (Nolabel, {
                  desc = Tvar None
                }, {
        desc = Tconstr (Path.Pident "int", [], ref []);
        level = 0;
        scope = 0;
        id = 3
      }, Cok);               
        val_kind= Reg_val;
        val_loc= Location.none;
        val_attributes=[];
        val_uid= Uid.create();
      },
      Exported
    );
    Sig_value(
      Ident.create_persistent "g",
      {
        val_type=Tarrow (Nolabel, {
                 desc = Tconstr (Path.Pident "int", [], ref []);
        level = 0;
        scope = 0;
        id = 3
                }, {
        desc = Tconstr (Path.Pident "int", [], ref []);
        level = 0;
        scope = 0;
        id = 3
      }, Cok);                   
        val_kind= Reg_val;
        val_loc= Location.none;
        val_attributes=[];
        val_uid= Uid.create();
      },
      Exported
    ); 
  ] in
  let signature2=signature1 in
let differs=Api_watch_diff.diff_interface ~reference:signature1 ~current:signature2 in 
  Format.printf "%b" differs;
  [%expect {|false|}]
