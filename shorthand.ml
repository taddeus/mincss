(* CSS shorthand creation based on:
 * - http://www.cssshorthand.com/
 *)

open Types
open Util
module SS = Set.Make(String)

let pattern = Str.regexp ("^\\(background\\|border\\|font\\|list-style" ^
                          "\\|outline\\|padding\\|margin\\)-\\(.*\\)$")

let order = function
  | "background" -> ["color"; "image"; "repeat"; "attachment"; "position"]
  | "border"     -> ["width"; "style"; "color"]
  | "font"       -> ["style"; "variant"; "weight"; "size"; "family"]
  | "list-style" -> ["type"; "position"; "image"]
  | "outline"    -> ["color"; "style"; "width"]
  | "margin"
  | "padding"    -> ["top"; "right"; "bottom"; "left"]
  | _            -> failwith "not a shorthand property"

let rec decls_mem name = function
  | [] -> false
  | (nm, _, _) :: _ when nm = name -> true
  | _ :: tl -> decls_mem name tl

(* find the value of the last declaration of some property (since the earlier
 * values are overridden), unless an earlier !important value was found *)
let decls_find name decls =
  let rec wrap known = function
    | [] -> known
    | (nm, value, true) :: _ when nm = name -> Some value
    | (nm, value, false) :: tl when nm = name -> wrap (Some value) tl
    | _ :: tl -> wrap known tl
  in
  match wrap None decls with
  | None -> raise Not_found
  | Some value -> value

let fold base decls =
  let rec filter = function
    | [] -> []

    (* `font-size` and `line-height` are slash-separated in `font` *)
    | "size" :: tl when base = "font" && decls_mem "line-height" decls ->
      let font_size = decls_find "font-size" decls in
      let line_height = decls_find "line-height" decls in
      Nary ("/", [font_size; line_height]) :: filter tl

    | name :: tl when decls_mem (base ^ "-" ^ name) decls ->
      decls_find (base ^ "-" ^ name) decls :: filter tl

    | _ :: tl -> filter tl
  in
  filter (order base)

let shorten_box_dims = function
  | [top; right; bottom; left]
    when top = bottom && right = left && top = right -> [top]
  | [top; right; bottom; left] when top = bottom && right = left -> [top; right]
  | [top; right; bottom; left] when right = left -> [top; right; bottom]
  | dims -> dims

let shorten decls = function
  (* `font-size` and `font-family` are required for `font` *)
  | "font" when decls_mem "font-size" decls && decls_mem "font-family" decls ->
    Some (Concat (fold "font" decls))

  (* `border-style` is required for `border` *)
  | "border" when decls_mem "border-style" decls ->
    Some (Concat (fold "border" decls))

  (* others require at least one property, which is the case when this function
   * is called *)
  | ("background" | "list-style" | "outline") as base ->
    Some (Concat (fold base decls))

  (* margin and padding can only be shorthanded when all directions are known,
   * merging into even shorter shorthands is done by `shorten_box_dims` *)
  | ("margin" | "padding") as base when
      let has dir = decls_mem (base ^ "-" ^ dir) decls in
      has "top" && has "right" && has "bottom" && has "left" ->
    let get dir = decls_find (base ^ "-" ^ dir) decls in
    Some (Concat (shorten_box_dims [get "top"; get "right";
                                    get "bottom"; get "left"]))

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
  | (("margin"| "padding") as base, value, imp) :: tl ->
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

let make_shorthands decls =
  (* unfold currently existing shorthands into separate properties for merging
   * with override properties that are defined later on *)
  let decls = unfold decls in

  (* find shorthand names for which properties are present *)
  let rec find_props = function
    | [] -> SS.empty
    | (name, value, false) :: tl when Str.string_match pattern name 0 ->
      let base = Str.matched_group 1 name in
      let sub = Str.matched_group 2 name in
      if List.mem sub (order base)
        then SS.add base (find_props tl)
        else find_props tl
    | _ :: tl -> find_props tl
  in
  let try_shorthands = find_props decls in

  (* try to generate shorthands for the matched base properties *)
  let rec replace base tl =
    match shorten decls base with
    | None -> tl
    | Some short_value -> (base, short_value, false) :: tl
  in
  let shorthands = SS.fold replace try_shorthands [] in

  (* filter out the original, partial properties, and append the shorthands *)
  let keep_prop = function
    | ("line-height", _, _) ->
      not (decls_mem "font" shorthands)
    | (name, _, _) ->
      not (Str.string_match pattern name 0) ||
      let base = Str.matched_group 1 name in
      let sub = Str.matched_group 2 name in
      not (List.mem sub (order base)) || not (decls_mem base shorthands)
  in
  List.filter keep_prop decls @ shorthands

let transform = function
  | Statement (Ruleset (selectors, decls)) ->
    Statement (Ruleset (selectors, make_shorthands decls))
  | v -> v

let compress = Util.transform_stylesheet transform

let transform_unfold = function
  | Statement (Ruleset (selectors, decls)) ->
    Statement (Ruleset (selectors, unfold decls))
  | v -> v

let unfold_stylesheet = Util.transform_stylesheet transform_unfold
