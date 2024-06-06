open Types
open Includemod

type 'a change_type = Added | Removed | Modified of 'a

type diff =
  | Value of string * (value_description * value_description) change_type
  | Any

type kind = Val
type name_kind = string * kind

module FieldMap = Map.Make (struct
  type t = name_kind

  let compare = Stdlib.compare
end)

let rec extract_values tbl = function
  | [] -> tbl
  | Sig_value (id, val_des, _) :: rem ->
      extract_values (FieldMap.add (Ident.name id, Val) val_des tbl) rem
  | _ :: rem -> extract_values tbl rem

let compare_values ~reference ~current =
  let ref_values = extract_values FieldMap.empty reference in
  let curr_values = extract_values FieldMap.empty current in
  let diffs = ref [] in
  FieldMap.iter
    (fun name curr_vd ->
      match name with
      | val_name, _ ->
          if FieldMap.mem name ref_values then
            let ref_vd = FieldMap.find name ref_values in
            let env = Env.empty in
            let typing_env =
              Env.add_signature reference (Env.in_signature true env)
            in
            let val_coercion () =
              Includecore.value_descriptions ~loc:ref_vd.val_loc typing_env
                val_name curr_vd ref_vd
            in
            match val_coercion () with
            | Tcoerce_none -> ()
            | _ ->
                diffs := Value (val_name, Modified (ref_vd, curr_vd)) :: !diffs
            | exception Includecore.Dont_match _ ->
                diffs := Value (val_name, Modified (ref_vd, curr_vd)) :: !diffs
          else diffs := Value (val_name, Added) :: !diffs)
    curr_values;

  FieldMap.iter
    (fun name _ref_vd ->
      match name with
      | val_name, _ ->
          if not (FieldMap.mem name curr_values) then
            diffs := Value (val_name, Removed) :: !diffs)
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
