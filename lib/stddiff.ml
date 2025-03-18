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

type 'item atomic_entry = ('item, 'item atomic_modification) entry
type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change

module List = struct
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

module Option = struct
  type ('a, 'diff) t = ('a, 'diff) entry

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

module Map = struct
  type ('a, 'diff) t = {
    same_map : 'a String_map.t;
    changed_map : ('a, 'diff) entry String_map.t;
  }

  let diff ~diff_one ~reference ~current =
    let same_seq, changed_seq =
      String_map.merge
        (fun _ ref_opt cur_opt ->
          match (ref_opt, cur_opt) with
          | None, None -> None
          | Some ref, None -> Some (Changed (Removed ref))
          | None, Some cur -> Some (Changed (Added cur))
          | Some ref, Some cur -> (
              match diff_one ref cur with
              | Same _ -> Some (Same ref)
              | Changed change -> Some (Changed (Modified change))))
        reference current
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
end
