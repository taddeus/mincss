open Types

let tab = "    "

let indent = Str.global_replace (Str.regexp "^\\(.\\)") (tab ^ "\\1")

let prefix_space = function "" -> "" | s -> " " ^ s

let rec cat sep fn = function
  | [] -> ""
  | [hd] -> fn hd
  | hd :: tl -> fn hd ^ sep ^ cat sep fn tl

let string_of_num n =
  if float_of_int (int_of_float n) = n
    then string_of_int (int_of_float n)
    else string_of_float n

(* TODO: move this to utils *)
let rec filter_none = function
  | [] -> []
  | None :: tl -> filter_none tl
  | Some hd :: tl -> hd :: filter_none tl

(*
 * Pretty-printing
 *)

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

let string_of_declaration (name, value, important) =
  let imp = if important then " !important" else "" in
  name ^ ": " ^ string_of_expr value ^ imp ^ ";"

let rec string_of_selector = function
  | Simple simple -> simple
  | Combinator (left, " ", right) ->
    string_of_selector left ^ " " ^ string_of_selector right
  | Combinator (left, com, right) ->
    string_of_selector left ^ " " ^ com ^ " " ^ string_of_selector right

let string_of_media_feature = function
  | (feature, None) -> "(" ^ feature ^ ")"
  | (feature, Some value) -> "(" ^ feature ^ ": " ^ string_of_expr value ^ ")"

let string_of_media_query query =
  let features_str = cat " and " string_of_media_feature in
  match query with
  | (None, None, []) -> ""
  | (None, Some mtype, []) -> mtype
  | (Some pre, Some mtype, []) -> pre ^ " " ^ mtype
  | (None, None, features) -> features_str features
  | (None, Some mtype, features) -> mtype ^ " and " ^ features_str features
  | (Some pre, Some mtype, features) ->
    pre ^ " " ^ mtype ^ " and " ^ features_str features
  | (Some pre, None, _) ->
    failwith "unexpected media query prefix \"" ^ pre ^ "\""

let block body = " {\n" ^ indent body ^ "\n}"

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
    "@import " ^ string_of_expr target ^ " " ^ String.concat ", " queries ^ ";"
  | Charset charset ->
    "@charset \"" ^ charset ^ "\";"
  | Page (None, decls) ->
    "@page" ^ block (cat "\n" string_of_declaration decls)
  | Page (Some pseudo, decls) ->
    "@page :" ^ pseudo ^ block (cat "\n" string_of_declaration decls)
  | Fontface decls ->
    "@font-face " ^ block (cat "\n" string_of_declaration decls)
  | Namespace (None, uri) ->
    "@namespace \"" ^ uri ^ "\";"
  | Namespace (Some prefix, uri) ->
    "@namespace " ^ prefix ^ " \"" ^ uri ^ "\";"

let string_of_stylesheet = cat "\n\n" string_of_statement

(*
 * Minified stringification
 *)

let rec minify_expr = function
  | Concat values -> cat " " minify_expr values
  | Function (name, arg) -> name ^ "(" ^ minify_expr arg ^ ")"
  | Unary (op, opnd) -> op ^ minify_expr opnd
  | Nary (",", opnds) -> cat "," minify_expr opnds
  | Nary (op, opnds) -> cat op minify_expr opnds
  | expr -> string_of_expr expr

let minify_declaration (name, value, important) =
  let imp = if important then "!important" else "" in
  name ^ ":" ^ minify_expr value ^ imp

let rec minify_selector = function
  | Simple simple -> simple
  | Combinator (left, com, right) ->
    minify_selector left ^ com ^ minify_selector right

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
    "{" ^ (cat ";" minify_declaration decls) ^ "}"
  | Media (queries, rulesets) ->
    "@media" ^ prefix_space (cat "," minify_media_query queries) ^
    "{" ^ (cat "" minify_statement rulesets) ^ "}"
  | Import (target, []) ->
    "@import " ^ string_of_expr target ^ ";"
  | Import (target, queries) ->
    "@import " ^ string_of_expr target ^ " " ^ String.concat "," queries ^ ";"
  | Page (None, decls) ->
    "@page{" ^ cat "" minify_declaration decls ^ "}"
  | Page (Some pseudo, decls) ->
    "@page :" ^ pseudo ^ "{" ^ cat "" minify_declaration decls ^ "}"
  | Fontface decls ->
    "@font-face{" ^ cat "" minify_declaration decls ^ "}"
  | statement -> string_of_statement statement

let minify_stylesheet = cat "" minify_statement
