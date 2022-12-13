open Printf
open Types

(** List utilities *)

let is_none = function None -> true | Some _ -> false
let is_some = function None -> false | Some _ -> true
let some_val = function None -> failwith "no value" | Some v -> v

let rec filter_none = function
  | [] -> []
  | None :: tl -> filter_none tl
  | Some hd :: tl -> hd :: filter_none tl

(** Reading input from file/stdin *)

let input_all ic =
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

let input_buffered ic chunksize =
  let rec read_all buf bufsize pos =
    match input ic buf pos (bufsize - pos) with
    | 0 -> close_in ic; Bytes.unsafe_to_string buf
    | nread when nread = bufsize - pos ->
      let bufsize = bufsize + chunksize in
      let pos = pos + nread in
      read_all (Bytes.cat buf (Bytes.create chunksize)) bufsize pos
    | nread ->
      read_all buf bufsize (pos + nread)
  in
  read_all (Bytes.create chunksize) chunksize 0

(** Error printing *)

let noloc = ("", 0, 0, 0, 0)

let tabwidth = 4

let count_tabs str upto =
  let rec count n = function
    | 0 -> n
    | i -> count (if String.get str (i - 1) = '\t' then n + 1 else n) (i - 1)
  in
  count 0 upto

let rec repeat s n = if n < 1 then "" else s ^ (repeat s (n - 1))

let retab str = Str.global_replace (Str.regexp "\t") (repeat " " tabwidth) str

let indent n = repeat (repeat " " (tabwidth - 1)) n

let prerr_loc (fname, ystart, yend, xstart, xend) =
  let file = open_in fname in

  (* skip lines until the first matched line *)
  for i = 1 to ystart - 1 do ignore (input_line file) done;

  (* for each line in `loc`, print the source line with an underline *)
  for l = ystart to yend do
    let line = input_line file in
    let linewidth = String.length line in
    let left = if l = ystart then xstart else 1 in
    let right = if l = yend then xend else linewidth in
    if linewidth > 0 then begin
      prerr_endline (retab line);
      prerr_string (indent (count_tabs line right));
      for i = 1 to left - 1 do prerr_char ' ' done;
      for i = left to right do prerr_char '^' done;
      prerr_endline "";
    end
  done

let prerr_loc_msg loc msg =
  let (fname, ystart, yend, xstart, xend) = loc in
  if loc != noloc then begin
    let line_s = if yend != ystart
      then sprintf "lines %d-%d" ystart yend
      else sprintf "line %d" ystart
    in
    let char_s = if xend != xstart || yend != ystart
      then sprintf "characters %d-%d" xstart xend
      else sprintf "character %d" xstart
    in
    eprintf "File \"%s\", %s, %s:\n" fname line_s char_s;
  end;
  eprintf "%s\n" msg;

  if loc != noloc then
    try prerr_loc loc
    with Sys_error _ -> ()

(** AST traversal *)

#define TRAV_ALL(id, constructor) \
  trav_all_##id l = \
    let rec filter_clear = function \
      | [] -> [] \
      | Clear :: tl -> filter_clear tl \
      | constructor hd :: tl -> hd :: filter_clear tl \
      | _ -> failwith ("expected " ^ #constructor ^ " or Clear") \
    in \
    filter_clear (List.map trav_##id l)

#define EXPECT(id, constructor) \
  expect_##id value = \
    match trav_##id value with \
    | constructor decl -> decl \
    | _ -> failwith ("expected " ^ #constructor)

let transform_stylesheet f stylesheet =
  let f x =
    match f x with
    | Expr (Concat [expr] | Nary (_, [expr])) -> Expr expr
    | value -> value
  in

  let rec trav_expr = function
    | Concat terms -> f (Expr (Concat (trav_all_expr terms)))
    | Function (name, arg) -> f (Expr (Function (name, expect_expr arg)))
    | Unary (op, opnd) -> f (Expr (Unary (op, expect_expr opnd)))
    | Nary (op, opnds) -> f (Expr (Nary (op, trav_all_expr opnds)))
    | value -> f (Expr value)
  and EXPECT(expr, Expr)
  and TRAV_ALL(expr, Expr) in

  let trav_declaration (name, value, important) =
    f (Declaration (name, expect_expr value, important))
  in
  let TRAV_ALL(declaration, Declaration) in

  let rec trav_selector = function
    | (No_element | All_elements | Element _) as elem ->
      f (Selector elem)
    | Id (base, id) ->
      f (Selector (Id (expect_selector base, id)))
    | Class (base, cls) ->
      f (Selector (Class (expect_selector base, cls)))
    | Attribute (base, attr, value) ->
      f (Selector (Attribute (expect_selector base, attr, value)))
    | Pseudo_class (base, cls, None) ->
      f (Selector (Pseudo_class (expect_selector base, cls, None)))
    | Pseudo_class (base, fn, Some args) ->
      let args = trav_all_pseudo_class_arg args in
      f (Selector (Pseudo_class (expect_selector base, fn, Some args)))
    | Pseudo_element (base, elem) ->
      f (Selector (Pseudo_element (expect_selector base, elem)))
    | Combinator (left, com, right) ->
      let left = expect_selector left in
      let right = expect_selector right in
      f (Selector (Combinator (left, com, right)))
  and EXPECT(selector, Selector)
  and TRAV_ALL(selector, Selector)

  and trav_pseudo_class_arg = function
    | Nested_selector s ->
      f (Pseudo_class_arg (Nested_selector (expect_selector s)))
    | Nth _ as elem ->
      f (Pseudo_class_arg elem)
  and TRAV_ALL(pseudo_class_arg, Pseudo_class_arg) in

  let trav_media_expr = function
    | (_, None) as value ->
      f (Media_expr value)
    | (name, Some value) ->
      let value =
        match trav_expr value with
        | Expr value -> Some value
        | Clear -> None
        | _ -> failwith "expected Expr or Clear"
      in
      f (Media_expr (name, value))
  in
  let TRAV_ALL(media_expr, Media_expr) in

  let trav_media_query (prefix, mtype, queries) =
    f (Media_query (prefix, mtype, trav_all_media_expr queries))
  in
  let TRAV_ALL(media_query, Media_query) in

  let trav_descriptor_declaration (name, value) =
    f (Descriptor_declaration (name, expect_expr value))
  in
  let TRAV_ALL(descriptor_declaration, Descriptor_declaration) in

  let trav_keyframe_ruleset (selector, decls) =
    f (Keyframe_ruleset (expect_expr selector, trav_all_declaration decls))
  in
  let TRAV_ALL(keyframe_ruleset, Keyframe_ruleset) in

  let trav_supports_declaration (name, value) =
    f (Supports_declaration (name, expect_expr value))
  in
  let EXPECT(supports_declaration, Supports_declaration) in

  let rec trav_condition = function
    | Not c -> f (Condition (Not (expect_condition c)))
    | And l -> f (Condition (And (trav_all_condition l)))
    | Or l -> f (Condition (Or (trav_all_condition l)))
    | Decl d -> f (Condition (Decl (expect_supports_declaration d)))
  and EXPECT(condition, Condition)
  and TRAV_ALL(condition, Condition) in

  let rec trav_statement = function
    | Ruleset (selectors, decls) ->
      let selectors = trav_all_selector selectors in
      let decls = trav_all_declaration decls in
      f (Statement (Ruleset (selectors, decls)))
    | Media (queries, rulesets) ->
      let queries = trav_all_media_query queries in
      let rulesets = trav_all_statement rulesets in
      f (Statement (Media (queries, rulesets)))
    | Import (target, queries) ->
      let target = expect_expr target in
      let queries = trav_all_media_query queries in
      f (Statement (Import (target, queries)))
    | Page (pseudo, decls) ->
      let decls = trav_all_declaration decls in
      f (Statement (Page (pseudo, decls)))
    | Font_face decls ->
      let decls = trav_all_descriptor_declaration decls in
      f (Statement (Font_face decls))
    | Namespace (prefix, uri) ->
      let uri = expect_expr uri in
      f (Statement (Namespace (prefix, uri)))
    | Keyframes (prefix, id, rules) ->
      let rules = trav_all_keyframe_ruleset rules in
      f (Statement (Keyframes (prefix, id, rules)))
    | Supports (condition, statements) ->
      let condition = expect_condition condition in
      let statements = trav_all_statement statements in
      f (Statement (Supports (condition, statements)))
    | s ->
      f (Statement s)
  and TRAV_ALL(statement, Statement) in

  trav_all_statement stylesheet

(** Expression identification *)

let is_color = Color_names.is_color

(** Sorting declarations *)

let sort_stylesheet =
  let pattern = Str.regexp "^\\([^-]+\\)-\\(.*\\)$" in
  transform_stylesheet begin function
    | Statement (Ruleset (selectors, decls)) ->
      let split x =
        if Str.string_match pattern x 0
          then Some (Str.matched_group 1 x, Str.matched_group 2 x)
          else None
      in
      let rec cmp a b =
        match split a, split b with
        | Some (base_a, sub_a), Some (base_b, sub_b) when base_a = base_b ->
          cmp sub_a sub_b
        | Some (base_a, _), Some (base_b, _) ->
          String.compare base_a base_b
        | Some (base_a, _), None when base_a = b -> 1
        | Some (base_a, _), None -> String.compare base_a b
        | None, Some (base_b, _) when a = base_b -> -1
        | None, Some (base_b, _) -> String.compare a base_b
        | None, None -> String.compare a b
      in
      let cmp_decls (a, _, _) (b, _, _) = cmp a b in
      Statement (Ruleset (selectors, List.stable_sort cmp_decls decls))
    | v -> v
  end

(** Misc *)
let is_int n = float_of_int (int_of_float n) = n
