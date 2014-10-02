open Types

module SM =  Map.Make(String)

let prune_duplicates decls =
  let keep = Array.make (List.length decls) true in

  let rec tag db index = function
    | (name, _, imp) :: tl when SM.mem name db ->
      (* previous value exists, one needs to be removed *)
      let prev_index, prev_imp = SM.find name db in
      if not imp && prev_imp then begin
        keep.(index) <- false;
        tag db (index + 1) tl
      end else begin
        keep.(prev_index) <- false;
        tag (SM.add name (index, imp) db) (index + 1) tl
      end
    | (name, _, imp) :: tl ->
      tag (SM.add name (index, imp) db) (index + 1) tl
    | [] -> ()
  in
  tag SM.empty 0 decls;

  let rec prune i = function
    | []                     -> []
    | hd :: tl when keep.(i) -> hd :: prune (i + 1) tl
    | _ :: tl                -> prune (i + 1) tl
  in
  prune 0 decls

let compress =
  Util.transform_stylesheet begin function
    | Statement (Ruleset (selectors, decls)) ->
      Statement (Ruleset (selectors, prune_duplicates decls))
    | v -> v
  end
