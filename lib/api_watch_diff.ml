open Types
open Includemod

type 'a change_type = Added | Removed | Modified of 'a

type diff =
  | Value of string * (value_description * value_description) change_type
  | Any

let extract_values (items : signature_item list) :
    (string, value_description) Hashtbl.t =
  let tbl = Hashtbl.create 10 in
  List.iter
    (function
      | Sig_value (id, val_des, _) -> Hashtbl.add tbl (Ident.name id) val_des
      | _ -> ())
    items;
  tbl

let compare_values ~reference ~current =
  let ref_values = extract_values reference in
  let curr_values = extract_values current in
  let diffs = ref [] in
  Hashtbl.iter
    (fun name curr_vd ->
      if Hashtbl.mem ref_values name then (
        let ref_vd = Hashtbl.find ref_values name in
        let expr1 = Transient_expr.repr ref_vd.val_type in
        let expr2 = Transient_expr.repr curr_vd.val_type in
        if expr1.desc <> expr2.desc then
          diffs := Value (name, Modified (ref_vd, curr_vd)) :: !diffs)
      else diffs := Value (name, Added) :: !diffs)
    curr_values;

  Hashtbl.iter
    (fun name _ref_vd ->
      if not (Hashtbl.mem curr_values name) then
        diffs := Value (name, Removed) :: !diffs)
    ref_values;
  !diffs

let diff_interface ~reference ~current =
  let value_diffs = compare_values ~reference ~current in
  if value_diffs = [] then
    let typing_env = Env.empty in
    let coercion1 () =
      signatures typing_env ~mark:Mark_both reference current
    in
    let coercion2 () =
      signatures typing_env ~mark:Mark_both current reference
    in
    match (coercion1 (), coercion2 ()) with
    | Tcoerce_none, Tcoerce_none -> []
    | _, _ -> [ Any ]
    | exception Includemod.Error _ -> [ Any ]
  else value_diffs
