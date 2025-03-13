(** Type aliases for representing common OCaml types diffs *)

(** Represent a change of an entry in a collection *)
type ('item, 'diff) entry =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
    [reference] is the value before and [current] is the value after the change
    occured. Use this type when there is no better representation available. *)

type ('item, 'diff) map = {
  same_map : 'item String_map.t;
  changed_map : ('item, 'diff) entry String_map.t;
}

type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change
type 'item atomic_entry = ('item, 'item atomic_modification) entry

type 'same maybe_changed_atomic =
  ('same, 'same atomic_modification) maybe_changed

type 'same maybe_changed_atomic_entry =
  ('same, 'same atomic_entry) maybe_changed

type ('same, 'diff) option_ = ('same option, ('same, 'diff) entry) maybe_changed
type 'same atomic_option = ('same, 'same atomic_modification) option_
type ('same, 'diff) list_ = ('same list, 'diff list) maybe_changed

module List_ = struct
  type ('a, 'diff) t = ('a, ('a, 'diff) entry) maybe_changed list

  let diff ~diff_one ~reference ~current =
    let rec aux reference current (acc, all) =
      match (reference, current) with
      | [], [] -> (List.rev acc, all)
      | hd :: tl, [] -> aux tl [] (Changed (Removed hd) :: acc, false)
      | [], hd :: tl -> aux [] tl (Changed (Added hd) :: acc, false)
      | hd :: tl, hd' :: tl' -> (
          let res = diff_one hd hd' in
          match res with
          | Same same -> aux tl tl' (Same same :: acc, true && all)
          | Changed change ->
              aux tl tl' (Changed (Modified change) :: acc, false && all))
    in
    let list_diff, all_same = aux reference current ([], true) in
    if all_same then Same reference else Changed list_diff
end

module Option_ = struct
  type ('a, 'diff) t = ('a option, ('a, 'diff) entry) maybe_changed

  let diff ~diff_one ~reference ~current =
    match (reference, current) with
    | None, None -> Same None
    | Some ref, None -> Changed (Removed ref)
    | None, Some cur -> Changed (Added cur)
    | Some ref, Some cur -> (
        let res = diff_one ref cur in
        match res with
        | Same same -> Same (Some same)
        | Changed change -> Changed (Modified change))
end

let diff_list ~diff_one ~ref_list ~cur_list =
  let rec aux reference current (acc, all) =
    match (reference, current) with
    | [], [] -> (List.rev acc, all)
    | h1 :: t1, [] -> aux t1 [] (diff_one (Some h1) None :: acc, false)
    | [], h2 :: t2 -> aux [] t2 (diff_one None (Some h2) :: acc, false)
    | h1 :: t1, h2 :: t2 ->
        let res = diff_one (Some h1) (Some h2) in
        let same = match res with Same _ -> true | Changed _ -> false in
        aux t1 t2 (res :: acc, same && all)
  in
  let list_diff, all_same = aux ref_list cur_list ([], true) in
  if all_same then Same ref_list else Changed list_diff

let diff_map ~(diff_one : 'a -> 'a -> 'diff option) ~(ref_map : 'a String_map.t)
    ~(cur_map : 'a String_map.t) : ('a, 'diff) map =
  let same_seq, changed_seq =
    String_map.merge
      (fun _ ref_opt cur_opt ->
        match (ref_opt, cur_opt) with
        | None, None -> None
        | Some ref, None -> Some (Changed (Removed ref))
        | None, Some cur -> Some (Changed (Added cur))
        | Some ref, Some cur -> (
            match diff_one ref cur with
            | None -> Some (Same ref)
            | Some diff -> Some (Changed (Modified diff))))
      ref_map cur_map
    |> String_map.to_seq
    |> Seq.partition_map (fun (name, i) ->
           match i with
           | Same i -> Either.Left (name, i)
           | Changed i -> Either.Right (name, i))
  in
  {
    same_map = String_map.of_seq same_seq;
    changed_map = String_map.of_seq changed_seq;
  }
