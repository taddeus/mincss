(* CSS shorthand creation based on:
 * - http://www.cssshorthand.com/
 *)

open Types
open Util
module SS = Set.Make(String)

let pattern = Str.regexp ("^\\(background\\|border\\|font\\|list-style" ^
                          "\\|outline\\|padding\\|margin\\)-\\(.*\\)$")

let subprops = function
  | "background" -> ["color"; "image"; "repeat"; "attachment"; "position"]
  | "border" -> ["width"; "style"; "color"]
  | "font" -> ["style"; "variant"; "weight"; "size"; "family"]
  | "list-style" -> ["type"; "position"; "image"]
  | "outline" -> ["color"; "style"; "width"]
  | "margin" | "padding" -> ["top"; "right"; "bottom"; "left"]
  | _ -> failwith "not a shorthand property"

let rec decls_mem name = function
  | [] -> false
  | (nm, _, false) :: _ when nm = name -> true
  | _ :: tl -> decls_mem name tl

let rec decls_find name = function
  | [] -> raise Not_found
  | (nm, value, false) :: _ when nm = name -> value
  | _ :: tl -> decls_find name tl

let order base decls =
  let rec filter = function
    | [] -> []
    | "size" :: tl when base = "font" && decls_mem "line-height" decls ->
      let font_size = decls_find "font-size" decls in
      let line_height = decls_find "line-height" decls in
      Nary ("/", [font_size; line_height]) :: filter tl
    | name :: tl when decls_mem (base ^ "-" ^ name) decls ->
      decls_find (base ^ "-" ^ name) decls :: filter tl
    | _ :: tl -> filter tl
  in
  filter (subprops base)

let rec shorten decls = function
  | "font" when not (decls_mem "font-size" decls) ->
    shorten (("font-size", Ident "medium", false) :: decls) "font"
  | "font" when decls_mem "font-family" decls ->
    Some (Concat (order "font" decls))
  | "border" when decls_mem "border-style" decls ->
    Some (Concat (order "border" decls))
  | ("background" | "list-style" | "outline") as base ->
    Some (Concat (order base decls))
  | ("margin" | "padding") as base when
      let has dir = decls_mem (base ^ "-" ^ dir) decls in
      has "top" && has "right" && has "bottom" && has "left" ->
    let get dir = decls_find (base ^ "-" ^ dir) decls in
    Some (Concat [get "top"; get "right"; get "bottom"; get "left"])
  | _ -> None

let make_shorthands decls =
  (* find shorthand names for which properties are present *)
  let rec find_props = function
    | [] -> SS.empty
    | (name, value, false) :: tl when Str.string_match pattern name 0 ->
      let base = Str.matched_group 1 name in
      let sub = Str.matched_group 2 name in
      if List.mem sub (subprops base)
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

  (* filter out the original, partial properties, and prepend the shorthands *)
  let keep_prop = function
    | (_, _, true) -> true
    | ("line-height", _, false) ->
      not (decls_mem "font" shorthands)
    | (name, _, false) ->
      not (Str.string_match pattern name 0) ||
      let base = Str.matched_group 1 name in
      let sub = Str.matched_group 2 name in
      not (List.mem sub (subprops base)) || not (decls_mem base shorthands)
  in
  shorthands @ List.filter keep_prop decls

let transform = function
  | Statement (Ruleset (selectors, decls)) ->
    Statement (Ruleset (selectors, make_shorthands decls))
  | v -> v

let compress = Util.transform_stylesheet transform
