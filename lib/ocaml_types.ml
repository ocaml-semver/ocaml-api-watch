open Asttypes

type type_expr = Types.type_expr
and row_desc = Types.row_desc
and row_field = Types.row_field
and field_kind = Types.field_kind
and commutable = Types.commutable
and ident = Ident.t
and uid = Shape.Uid.t

and type_desc = Types.type_desc =
  | Tvar of string option
  | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list

and fixed_explanation = Types.fixed_explanation =
  | Univar of type_expr  (** The row type was bound to an univar *)
  | Fixed_private  (** The row type is private *)
  | Reified of Path.t  (** The row was reified *)
  | Rigid  (** The row type was made rigid during constraint verification *)

and abbrev_memo = Types.abbrev_memo =
  | Mnil  (** No known abbreviation *)
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind_view = Types.field_kind_view = Fprivate | Fpublic | Fabsent

and row_desc_repr = Types.row_desc_repr =
  | Row of {
      fields : (label * row_field) list;
      more : type_expr;
      closed : bool;
      fixed : fixed_explanation option;
      name : (Path.t * type_expr list) option;
    }

and row_field_view = Types.row_field_view =
  | Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
  | Rabsent

and value_description = Types.value_description = {
  val_type : type_expr;
  val_kind : value_kind;
  val_loc : Location.t;
  val_attributes : Parsetree.attributes;
  val_uid : uid;
}

and value_kind = Types.value_kind =
  | Val_reg (* Regular value *)
  | Val_prim of Primitive.description (* Primitive *)
  | Val_ivar of mutable_flag * string (* Instance variable (mutable ?) *)
  | Val_self of class_signature * self_meths * ident Types.Vars.t * string
    (* Self *)
  | Val_anc of class_signature * ident Types.Meths.t * string
(* Ancestor *)

and self_meths = Types.self_meths =
  | Self_concrete of ident Types.Meths.t
  | Self_virtual of ident Types.Meths.t ref

and class_signature = Types.class_signature = {
  csig_self : type_expr;
  mutable csig_self_row : type_expr;
  mutable csig_vars : (mutable_flag * virtual_flag * type_expr) Types.Vars.t;
  mutable csig_meths : (method_privacy * virtual_flag * type_expr) Types.Meths.t;
}

and method_privacy = Types.method_privacy = Mpublic | Mprivate of field_kind
(* The [field_kind] is always [Fabsent] in a complete class type. *)

and type_declaration = Types.type_declaration = {
  type_params : type_expr list;
  type_arity : int;
  type_kind : type_decl_kind;
  type_private : private_flag;
  type_manifest : type_expr option;
  type_variance : Types.Variance.t list;
  type_separability : Types.Separability.t list;
  type_is_newtype : bool;
  type_expansion_scope : int;
  type_loc : Location.t;
  type_attributes : Parsetree.attributes;
  type_immediate : Type_immediacy.t;
  type_unboxed_default : bool;
  (* true if the unboxed-ness of this type was chosen by a compiler flag *)
  type_uid : uid;
}

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind = ('lbl, 'cstr) Types.type_kind =
  | Type_abstract of type_origin
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and type_origin = Types.type_origin =
  | Definition
  | Rec_check_regularity (* See Typedecl.transl_type_decl *)
  | Existential of string

and record_representation = Types.record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_float (* All fields are floats *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int (* Inlined record *)
  | Record_extension of Path.t
(* Inlined record under extension *)
(* The argument is the path of the extension *)

and variant_representation = Types.variant_representation =
  | Variant_regular (* Constant or boxed constructors *)
  | Variant_unboxed (* One unboxed single-field constructor *)

and label_declaration = Types.label_declaration = {
  ld_id : ident;
  ld_mutable : mutable_flag;
  ld_type : type_expr;
  ld_loc : Location.t;
  ld_attributes : Parsetree.attributes;
  ld_uid : uid;
}

and constructor_declaration = Types.constructor_declaration = {
  cd_id : ident;
  cd_args : constructor_arguments;
  cd_res : type_expr option;
  cd_loc : Location.t;
  cd_attributes : Parsetree.attributes;
  cd_uid : uid;
}

and constructor_arguments = Types.constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and extension_constructor = Types.extension_constructor = {
  ext_type_path : Path.t;
  ext_type_params : type_expr list;
  ext_args : constructor_arguments;
  ext_ret_type : type_expr option;
  ext_private : private_flag;
  ext_loc : Location.t;
  ext_attributes : Parsetree.attributes;
  ext_uid : uid;
}

and type_transparence = Types.type_transparence =
  | Type_public (* unrestricted expansion *)
  | Type_new (* "new" type *)
  | Type_private (* private type *)

(* Type expressions for the class language *)
and class_type = Types.class_type =
  | Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_declaration = Types.class_declaration = {
  cty_params : type_expr list;
  mutable cty_type : class_type;
  cty_path : Path.t;
  cty_new : type_expr option;
  cty_variance : Types.Variance.t list;
  cty_loc : Location.t;
  cty_attributes : Parsetree.attributes;
  cty_uid : uid;
}

and class_type_declaration = Types.class_type_declaration = {
  clty_params : type_expr list;
  clty_type : class_type;
  clty_path : Path.t;
  clty_hash_type : type_declaration; (* object type with an open row *)
  clty_variance : Types.Variance.t list;
  clty_loc : Location.t;
  clty_attributes : Parsetree.attributes;
  clty_uid : uid;
}

(* Type expressions for the module language *)
and visibility = Types.visibility = Exported | Hidden

and module_type = Types.module_type =
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter = Types.functor_parameter =
  | Unit
  | Named of ident option * module_type

and module_presence = Types.module_presence = Mp_present | Mp_absent
and signature = signature_item list

and signature_item = Types.signature_item =
  | Sig_value of ident * value_description * visibility
  | Sig_type of ident * type_declaration * rec_status * visibility
  | Sig_typext of ident * extension_constructor * ext_status * visibility
  | Sig_module of
      ident * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of ident * modtype_declaration * visibility
  | Sig_class of ident * class_declaration * rec_status * visibility
  | Sig_class_type of ident * class_type_declaration * rec_status * visibility

and module_declaration = Types.module_declaration = {
  md_type : module_type;
  md_attributes : Parsetree.attributes;
  md_loc : Location.t;
  md_uid : uid;
}

and modtype_declaration = Types.modtype_declaration = {
  mtd_type : module_type option; (* None: abstract *)
  mtd_attributes : Parsetree.attributes;
  mtd_loc : Location.t;
  mtd_uid : uid;
}

and rec_status = Types.rec_status =
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ext_status = Types.ext_status =
  | Text_first (* first constructor in an extension *)
  | Text_next (* not first constructor in an extension *)
  | Text_exception
[@@deriving traverse]

class virtual _iter =
  object
    inherit iter
    inherit Ppxlib_traverse_builtins.iter
    method ref iter_a a_ref = iter_a !a_ref
    method virtual_flag _ = ()
    method types__vars__t _ _ = ()
    method types__variance__t _ = ()
    method types__type_expr _ = ()
    method types__separability__t _ = ()
    method types__row_field _ = ()
    method types__row_desc _ = ()
    method types__meths__t _ _ = ()
    method types__field_kind _ = ()
    method types__commutable _ = ()
    method type_immediacy__t _ = ()
    method shape__uid__t _ = ()
    method private_flag _ = ()
    method primitive__description _ = ()
    method path__t _ = ()
    method parsetree__attributes _ = ()
    method mutable_flag _ = ()
    method longident__t _ = ()
    method location__t _ = ()
    method label _ = ()
    method ident__t _ = ()
    method arg_label _ = ()
  end

class virtual iter = _iter

class print =
  object (self)
    inherit iter as super

    method! type_expr te =
      Format.printf "{%d:" (Types.get_id te);
      self#type_desc (Types.get_desc te);
      Format.printf "}"

    method! type_desc td =
      match td with
      | Tsubst (ty, None) ->
          Format.printf "Subst(";
          self#type_expr ty;
          Format.printf ", None)"
      | Tsubst (ty, Some ty') ->
          Format.printf "Subst(";
          self#type_expr ty;
          Format.printf ", ";
          self#type_expr ty';
          Format.printf ")"
      | Tconstr (path, _, _) -> Path.print Format.std_formatter path
      | Tvar (Some s) -> Format.printf "var:'%s" s
      | Tvar None -> Format.printf "var:NONE"
      | Tunivar (Some s) -> Format.printf "uvar:'%s" s
      | Tunivar None -> Format.printf "uvar:NONE"
      | _ -> super#type_desc td

    method! signature_item sigi =
      match sigi with
      | Sig_type (id, type_decl, _rec_status, _visbility) ->
          Format.printf "type_decl:\n";
          Format.printf "id:%s uid:%a\n" (Ident.unique_name id) Shape.Uid.print
            type_decl.type_uid;
          Printtyp.signature Format.std_formatter [ sigi ];
          Format.print_newline ()
      | Sig_value (id, value, _) ->
          Format.printf "value desc: %a\n" (Printtyp.value_description id) value;
          Format.printf "id:%s uid:%a\n" (Ident.unique_name id) Shape.Uid.print
            value.val_uid;
          Format.printf "val %s :" (Ident.name id);
          self#type_expr value.val_type;
          Format.print_newline ()
      | _ ->
          Printtyp.signature Format.std_formatter [ sigi ];
          Format.print_newline ()
  end
