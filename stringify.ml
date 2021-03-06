open Types
open Util

let tab = "    "

let indent = Str.global_replace (Str.regexp "^\\(.\\)") (tab ^ "\\1")

let prefix_space = function "" -> "" | s -> " " ^ s

let rec cat sep fn = function
  | [] -> ""
  | [hd] -> fn hd
  | hd :: tl -> fn hd ^ sep ^ cat sep fn tl

(*
 * Pretty-printing
 *)

let string_of_num n =
  if is_int n
    then string_of_int (int_of_float n)
    else string_of_float n

let rec string_of_expr = function
  | Ident id -> id
  | Strlit str -> "\"" ^ str ^ "\""
  | Uri uri when String.contains uri ')' -> "url(\"" ^ uri ^ "\")"
  | Uri uri -> "url(" ^ uri ^ ")"
  | Concat values -> cat " " string_of_expr values
  | Number (n, None) -> string_of_num n
  | Number (n, Some u) -> string_of_num n ^ u
  | Function (name, arg) -> name ^ "(" ^ string_of_expr arg ^ ")"
  | Hexcolor hex -> "#" ^ hex
  | Unary (op, opnd) -> op ^ string_of_expr opnd
  | Nary (",", opnds) -> cat ", " string_of_expr opnds
  | Nary (op, opnds) -> cat op string_of_expr opnds
  | Key_value (key, op, value) -> key ^ op ^ string_of_expr value

let string_of_declaration (name, value, important) =
  let imp = if important then " !important" else "" in
  name ^ ": " ^ string_of_expr value ^ imp ^ ";"

let rec stringify_selector w selector =
  let str = stringify_selector w in
  match selector with
  | No_element   -> ""
  | All_elements -> "*"
  | Element elem -> elem
  | Id (base, id) ->
    str base ^ "#" ^ id
  | Class (base, cls) ->
    str base ^ "." ^ cls
  | Attribute (base, attr, None) ->
    str base ^ "[" ^ attr ^ "]"
  | Attribute (base, attr, Some (op, value)) ->
    str base ^ "[" ^ attr ^ w ^ op ^ w ^ string_of_expr value ^ "]"
  | Pseudo_class (base, cls, None) ->
    str base ^ ":" ^ cls
  | Pseudo_class (base, fn, Some args) ->
    str base ^ ":" ^ fn ^ "(" ^ cat ("," ^ w) (stringify_arg w) args ^ ")"
  | Pseudo_element (base, elem) ->
    str base ^ "::" ^ elem
  | Combinator (left, " ", right) ->
    str left ^ " " ^ str right
  | Combinator (left, com, right) ->
    str left ^ w ^ com ^ w ^ str right

and stringify_arg w = function
  | Nested_selector s -> stringify_selector w s
  | Nth nth -> stringify_nth w nth

and stringify_nth w = function
  | Even           -> "even"
  | Odd            -> "odd"
  | Formula (0, b) -> string_of_int b
  | Formula (a, b) ->
    begin
      match a with
      | 1  -> "n"
      | -1 -> "-n"
      | a  -> string_of_int a ^ "n"
    end ^ begin
      match b with
      | 0            -> ""
      | b when b < 0 -> w ^ "-" ^ w ^ string_of_int (-b)
      | b            -> w ^ "+" ^ w ^ string_of_int b
    end

let string_of_selector = stringify_selector " "

let string_of_media_expr = function
  | (feature, None) -> "(" ^ feature ^ ")"
  | (feature, Some value) -> "(" ^ feature ^ ": " ^ string_of_expr value ^ ")"

let string_of_media_query =
  let features_str = cat " and " string_of_media_expr in
  function
  | (None, None, []) -> ""
  | (None, Some mtype, []) -> mtype
  | (Some pre, Some mtype, []) -> pre ^ " " ^ mtype
  | (None, None, features) -> features_str features
  | (None, Some mtype, features) -> mtype ^ " and " ^ features_str features
  | (Some pre, Some mtype, features) ->
    pre ^ " " ^ mtype ^ " and " ^ features_str features
  | (Some pre, None, _) ->
    failwith "unexpected media query prefix \"" ^ pre ^ "\""

let stringify_condition w c =
  let rec transform =
    let p c = `Parens (transform c) in
    function
    | Not c -> `Not (p c)
    | And c -> `And (List.map p c)
    | Or c -> `Or (List.map p c)
    | Decl (name, value) -> `Decl (name, value)
  in
  let rec str = function
    | `Not c -> "not " ^ str c
    | `And c -> cat " and " str c
    | `Or c -> cat " or " str c
    | `Decl (name, value) -> "(" ^ name ^ ":" ^ w ^ string_of_expr value ^ ")"
    | `Parens (`Decl _ as d) -> str d
    | `Parens c -> "(" ^ str c ^ ")"
  in
  str (transform c)

let string_of_condition = stringify_condition " "

let block = function "" -> " {}" | body -> " {\n" ^ indent body ^ "\n}"

let string_of_descriptor_declaration (name, value) =
  name ^ ": " ^ string_of_expr value ^ ";"

let string_of_keyframe_ruleset (expr, decls) =
  string_of_expr expr ^ block (cat "\n" string_of_declaration decls)

let rec string_of_statement = function
  | Ruleset (selectors, decls) ->
    cat ", " string_of_selector selectors ^
    block (cat "\n" string_of_declaration decls)
  | Media (queries, rulesets) ->
    "@media" ^ prefix_space (cat ", " string_of_media_query queries) ^
    block (cat "\n\n" string_of_statement rulesets)
  | Import (target, []) ->
    "@import " ^ string_of_expr target ^ ";"
  | Import (target, queries) ->
    "@import " ^ string_of_expr target ^ " " ^ cat ", " string_of_media_query queries ^ ";"
  | Charset charset ->
    "@charset \"" ^ charset ^ "\";"
  | Page (None, decls) ->
    "@page" ^ block (cat "\n" string_of_declaration decls)
  | Page (Some pseudo, decls) ->
    "@page :" ^ pseudo ^ block (cat "\n" string_of_declaration decls)
  | Font_face decls ->
    "@font-face" ^ block (cat "\n" string_of_descriptor_declaration decls)
  | Namespace (None, uri) ->
    "@namespace " ^ string_of_expr uri ^ ";"
  | Namespace (Some prefix, uri) ->
    "@namespace " ^ prefix ^ " " ^ string_of_expr uri ^ ";"
  | Keyframes (prefix, id, rules) ->
    "@" ^ prefix ^ "keyframes " ^ id ^
    block (cat "\n\n" string_of_keyframe_ruleset rules)
  | Supports (condition, statements) ->
    "@supports " ^ string_of_condition condition ^
    block (cat "\n\n" string_of_statement statements)
  | Viewport (prefix, decls) ->
    "@" ^ prefix ^ "viewport" ^ block (cat "\n" string_of_declaration decls)

let string_of_stylesheet = cat "\n\n" string_of_statement

(*
 * Minified stringification
 *)

let minify_num n =
  (* Round numbers to at most 2 decimal digits *)
  let round2 n = floor (100. *. n +. 0.5) /. 100. in

  if float_of_int (int_of_float n) = n then
    string_of_int (int_of_float n)
  else if n < 1.0 && n > -1.0 then
    let s = string_of_float (round2 n) in
    String.sub s 1 (String.length s - 1)
  else
    string_of_float (round2 n)

let rec minify_expr = function
  | Concat values -> cat " " minify_expr values
  | Function (name, arg) -> name ^ "(" ^ minify_expr arg ^ ")"
  | Unary (op, opnd) -> op ^ minify_expr opnd
  | Nary (",", opnds) -> cat "," minify_expr opnds
  | Nary (op, opnds) -> cat op minify_expr opnds
  | Number (n, None) -> minify_num n
  | Number (n, Some u) -> minify_num n ^ u
  | Key_value (key, op, value) -> key ^ op ^ minify_expr value
  | expr -> string_of_expr expr

let minify_declaration (name, value, important) =
  let imp = if important then "!important" else "" in
  name ^ ":" ^ minify_expr value ^ imp

let rec minify_selector = stringify_selector ""

let minify_media_feature = function
  | (feature, None) -> "(" ^ feature ^ ")"
  | (feature, Some value) -> "(" ^ feature ^ ":" ^ minify_expr value ^ ")"

let minify_media_query query =
  let features_str = cat "and " minify_media_feature in
  match query with
  | (None, None, features) -> features_str features
  | (None, Some mtype, features) -> mtype ^ " and " ^ features_str features
  | (Some pre, Some mtype, features) ->
    pre ^ " " ^ mtype ^ " and " ^ features_str features
  | _ -> string_of_media_query query

let rec minify_statement = function
  | Ruleset (selectors, decls) ->
    cat "," minify_selector selectors ^
    "{" ^ cat ";" minify_declaration decls ^ "}"
  | Media (queries, rulesets) ->
    "@media" ^ prefix_space (cat "," minify_media_query queries) ^
    "{" ^ cat "" minify_statement rulesets ^ "}"
  | Import (target, []) ->
    "@import " ^ minify_expr target ^ ";"
  | Import (target, queries) ->
    "@import " ^ minify_expr target ^ " " ^
    cat "," string_of_media_query queries ^ ";"
  | Page (None, decls) ->
    "@page{" ^ cat ";" minify_declaration decls ^ "}"
  | Page (Some pseudo, decls) ->
    "@page :" ^ pseudo ^ "{" ^ cat ";" minify_declaration decls ^ "}"
  | Font_face decls ->
    let minify_descriptor_declaration (name, value) =
      name ^ ":" ^ minify_expr value
    in
    "@font-face{" ^ cat ";" minify_descriptor_declaration decls ^ "}"
  | Keyframes (prefix, id, rules) ->
    let minify_keyframe_ruleset (expr, decls) =
      minify_expr expr ^ "{" ^ cat ";" minify_declaration decls ^ "}"
    in
    "@" ^ prefix ^ "keyframes " ^ id ^
    "{" ^ cat "" minify_keyframe_ruleset rules ^ "}"
  | Supports (condition, statements) ->
    "@supports " ^ stringify_condition "" condition ^
    "{" ^ cat "" minify_statement statements ^ "}"
  | Viewport (prefix, decls) ->
    "@" ^ prefix ^ "viewport{" ^ cat ";" minify_declaration decls ^ "}"
  | statement -> string_of_statement statement

let minify_stylesheet = cat "" minify_statement

(*
 * Stringify any AST node in a box
 *)

let string_of_box = function
  | Expr expr ->
    string_of_expr expr
  | Declaration declaration ->
    string_of_declaration declaration
  | Selector selector ->
    string_of_selector selector
  | Media_expr media_expr ->
    string_of_media_expr media_expr
  | Media_query media_query ->
    string_of_media_query media_query
  | Descriptor_declaration descriptor_declaration ->
    string_of_descriptor_declaration descriptor_declaration
  | Keyframe_ruleset keyframe_ruleset ->
    string_of_keyframe_ruleset keyframe_ruleset
  | Condition condition ->
    string_of_condition condition
  | Statement statement ->
    string_of_statement statement
  | Stylesheet stylesheet ->
    string_of_stylesheet stylesheet
  | Clear ->
    "<clear>"
  | _ ->
    raise (Invalid_argument "box")
