(* CSS shorthand creation based on:
 * - http://www.cssshorthand.com/
 *)

open Types
open Util

module KM = Map.Make(struct
  type t = string * bool
  let compare a b =
    match a, b with
    | (_, false), (_, true)    -> -1
    | (_, true), (_, false)    -> 1
    | (base_a, _), (base_b, _) -> String.compare base_a base_b
end)


let order = function
  | "background" -> ["color"; "image"; "repeat"; "attachment"; "position-x";
                     "position-y"]
  | "border"     -> ["width"; "style"; "color"]
  | "font"       -> ["style"; "variant"; "weight"; "size"; "family"]
  | "list-style" -> ["type"; "position"; "image"]
  | "outline"    -> ["color"; "style"; "width"]
  | "margin"
  | "padding"    -> ["top"; "right"; "bottom"; "left"]
  | _            -> failwith "not a shorthand property"

let fold_box_dims = function
  | [top; right; bottom; left]
    when top = bottom && right = left && top = right -> [top]
  | [top; right; bottom; left] when top = bottom && right = left -> [top; right]
  | [top; right; bottom; left] when right = left -> [top; right; bottom]
  | dims -> dims

let fold group base =
  let group_mem name =
    let rec mem = function
      | [] -> false
      | (nm, _, _) :: _ when nm = name -> true
      | _ :: tl -> mem tl
    in
    mem group
  in

  let group_find name =
    let rec wrap known = function
      | [] ->
        (match known with Some value -> value | None -> raise Not_found)
      | (nm, value, _) :: tl when nm = name ->
        wrap (Some value) tl
      | _ :: tl ->
        wrap known tl
    in
    wrap None group
  in

  let exists sub = group_mem (base ^ "-" ^ sub) in
  let find sub = group_find (base ^ "-" ^ sub) in

  let rec lookup = function
    | [] -> []
    (* `font-size` and `line-height` are slash-separated in `font` *)
    | "size" :: tl when base = "font" && group_mem "line-height" ->
      Nary ("/", [find "size"; group_find "line-height"]) :: lookup tl
    | name :: tl when exists name -> find name :: lookup tl
    | _ :: tl -> lookup tl
  in

  match base with

  (* `font-size` and `font-family` are required for `font` *)
  | "font" when exists "size" && exists "family" ->
    Some (Concat (lookup (order "font")))

  (* `border-style` is required for `border` *)
  | "border" when exists "style" ->
    Some (Concat (lookup (order "border")))

  (* others require at least one property, which is already the case when this
   * function is called *)
  | ("background" | "list-style" | "outline") as base ->
    Some (Concat (lookup (order base)))

  (* margin and padding can only be shorthanded when all directions are known,
   * merging into even shorter shorthands is done by `fold_box_dims` *)
  | "margin" | "padding"
    when exists "top" && exists "right" && exists "bottom" && exists "left" ->
    let dirs = [find "top"; find "right"; find "bottom"; find "left"] in
    Some (Concat (fold_box_dims dirs))

  | _ -> None

let rec list_from i = function
  | [] when i > 0 -> raise (Invalid_argument "l")
  | []            -> []  (* make the compiler happy *)
  | l when i = 0  -> l
  | _ :: tl       -> list_from (i - 1) tl

let is_width = function
  | Ident ("thin" | "thick" | "medium")
  | Number _ -> true
  | _ -> false

let rec unfold = function
  | [] -> []

  (* do not unfold "<shorthand>: inherit;" *)
  | (("background" | "border" | "font" | "list-style" | "outline" | "margin" |
      "padding"), Ident "inherit", _) as orig :: tl ->
    orig :: unfold tl

  (* background: [color] [image] [repeat] [attachment] [position] *)
  | ("background", Concat values, imp) :: tl ->
    let make sub value = ("background-" ^ sub, value, imp) in
    let id_color = function
      | [] -> []
      | [color] when Color_names.is_color color -> [make "color" color]
      | tl -> raise (Box_error (Expr (Concat tl), "invalid background shortcut"))
      (*| _ -> failwith "invalid background shortcut"*)
    in
    let id_repeat = function
      | repeat :: (Uri _ as image) :: tl ->
        make "repeat" repeat :: make "image" image :: id_color tl
      | Uri _ as image :: tl ->
        make "image" image :: id_color tl
      | tl -> id_color tl
    in
    let id_attachment = function
      | Ident _ as attachment :: (Ident _ as repeat) :: tl ->
        make "attachment" attachment :: make "repeat" repeat :: id_repeat tl
      | Ident ("scroll" | "fixed") as attachment :: (Uri url :: _ as tl) ->
        make "attachment" attachment :: id_repeat tl
      | (_ :: Uri _ :: _) as tl
      | tl -> id_repeat tl
    in
    let id_pos = function
      | Number _ as posy :: (Number _ as posx) :: tl
      | (Ident ("top" | "center" | "bottom") as posy) ::
        (Ident ("left" | "center" | "right") as posx) :: tl ->
        make "position-y" posy :: make "position-x" posx :: id_attachment tl
      | tl -> id_attachment tl
    in
    List.rev (id_pos (List.rev values)) @ unfold tl
  | ("background", (Uri _ as image), imp) :: tl ->
    ("background-image", image, imp) :: unfold tl
  | ("background", color, imp) :: tl ->
    ("background-color", color, imp) :: unfold tl

  (* border: [width] style [color] *)
  | ("border", Concat [Ident _ as style], imp) :: tl ->
    ("border-style", style, imp) :: unfold tl
  | ("border", Concat [width; Ident _ as style; color], imp) :: tl ->
    ("border-width", width, imp) ::
    ("border-style", style, imp) ::
    ("border-color", color, imp) :: unfold tl
  | ("border", Concat [Number _ as width; Ident _ as style], imp) :: tl ->
    ("border-width", width, imp) ::
    ("border-style", style, imp) :: unfold tl
  | ("border", Concat [Ident _ as style; color], imp) :: tl ->
    ("border-style", style, imp) ::
    ("border-color", color, imp) :: unfold tl

  (* font: [style] [variant] [weight] size[/line-height] family *)
  | ("font", Concat values, imp) as orig :: tl ->
    let replacement =
      let make sub value = ("font-" ^ sub, value, imp) in
      let identify options =
        let return sub = assert (List.mem sub options); sub in
        function
        | Ident "normal" -> List.hd options
        | Ident ("italic" | "oblique") -> return "style"
        | Ident "small-caps" -> return "variant"
        | _ -> return "weight"
      in
      match values with
      | [size; family] ->
        [make "size" size; make "family" family]
      | [first; size; family] ->
        [make (identify ["weight"; "variant"; "style"] first) first;
         make "size" size; make "family" family]
      | [first; second; size; family] ->
        [make (identify ["variant"; "style"] first) first;
         make (identify ["weight"; "variant"] second) second;
         make "size" size; make "family" family]
      | [style; variant; weight; size; family] ->
        [make "style" style; make "variant" variant; make "weight" weight;
         make "size" size; make "family" family]
      | _ -> [orig]
    in
    let rec split_size = function
      | [] -> []
      | ("font-size", Nary ("/", [size; line_height]), _) :: tl ->
        ("font-size", size, imp) ::
        ("line-height", line_height, imp) :: tl
      | hd :: tl -> hd :: split_size tl
    in
    split_size replacement @ unfold tl

  (* list-style: [type] [position] [image] *)
  | ("list-style", Concat [ltype; pos; image], imp) :: tl ->
    ("list-style-type", ltype, imp) ::
    ("list-style-position", pos, imp) ::
    ("list-style-image", image, imp) :: unfold tl
  | ("list-style", Concat [Ident _ as pos; Uri _ as image], imp) :: tl ->
    ("list-style-position", pos, imp) ::
    ("list-style-image", image, imp) :: unfold tl
  | ("list-style", Concat [ltype; Ident _ as pos], imp) :: tl ->
    ("list-style-type", ltype, imp) ::
    ("list-style-position", pos, imp) :: unfold tl

  (* margin: top right bottom left
   *       | top right-left bottom
   *       | top-bottom right-left
   *       | top right bottom left
   *       | all
   *)
  | (("margin" | "padding") as base, value, imp) :: tl ->
    let (top, right, bottom, left) =
      match value with
      | Concat [top; right; bottom; left] ->
        (top, right, bottom, left)
      | Concat [top; right; bottom] ->
        (top, right, bottom, right)
      | Concat [top; right] ->
        (top, right, top, right)
      | _ ->
        (value, value, value, value)
    in
    let make dir value = (base ^ "-" ^ dir, value, imp) in
    make "top" top :: make "right" right :: make "bottom" bottom ::
    make "left" left :: unfold tl

  | hd :: tl ->
    hd :: unfold tl

let pattern = Str.regexp ("^\\(background\\|border\\|font\\|list-style" ^
                          "\\|outline\\|padding\\|margin\\)-\\(.*\\)$")

let rec make_shorthands decls =
  (* unfold currently existing shorthands into separate properties for merging
   * with override properties that are defined later on *)
  (*let decls = unfold decls in
    XXX: done by main function for correct pruning of duplicate declarations*)

  let rec extract_groups decl_skipped groups rest =
    let rec find_in_group name = function
      | [] -> false
      | (nm, _, _) :: _ when nm = name -> true
      | _ :: tl -> find_in_group name tl
    in
    let should_skip base name imp =
      try find_in_group name (KM.find (base, imp) groups)
      with Not_found -> false
    in
    let add base imp value =
      let key = base, imp in
      let group = try KM.find key groups with Not_found -> [] in
      KM.add key (value :: group) groups
    in
    function
    | [] -> decl_skipped, groups, rest
    | (("line-height", _, imp) as hd) :: tl
      when should_skip "font" "line_height" imp ->
      extract_groups true groups (hd :: rest) tl
    | (("line-height", _, imp) as hd) :: tl ->
      extract_groups decl_skipped (add "font" imp hd) rest tl
    | ((name, _, imp) as hd) :: tl when Str.string_match pattern name 0 ->
      let base = Str.matched_group 1 name in
      let sub = Str.matched_group 2 name in
      let skip_this = should_skip base name imp in
      if not skip_this && List.mem sub (order base)
        then extract_groups decl_skipped (add base imp hd) rest tl
        else extract_groups (decl_skipped || skip_this) groups (hd :: rest) tl
    | hd :: tl -> extract_groups decl_skipped groups (hd :: rest) tl
  in
  let decl_skipped, groups, rest = extract_groups false KM.empty [] decls in

  let replace (base, important) group tl =
    match fold (List.rev group) base with
    | Some short_value -> (base, short_value, important) :: tl
    | None -> group @ tl
  in
  let shorthands = KM.fold replace groups [] in

  let decls = List.rev_append rest shorthands in
  if decl_skipped then make_shorthands decls else decls

let compress =
  Util.transform_stylesheet begin function
    | Statement (Ruleset (selectors, decls)) ->
      Statement (Ruleset (selectors, make_shorthands decls))
    | v -> v
  end

let unfold_stylesheet =
  Util.transform_stylesheet begin function
    | Statement (Ruleset (selectors, decls)) ->
      Statement (Ruleset (selectors, unfold decls))
    | v -> v
  end
