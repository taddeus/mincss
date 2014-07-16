open Types

let tab = "    "

let indent = Str.global_replace (Str.regexp "^\\(.\\)") (tab ^ "\\1")

let rec cat sep fn = function
  | [] -> ""
  | [hd] -> fn hd
  | hd :: tl -> fn hd ^ sep ^ cat sep fn tl

let string_of_num n =
  if float_of_int (int_of_float n) = n
    then string_of_int (int_of_float n)
    else string_of_float n

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

let block body = " {\n" ^ indent body ^ "\n}"

let rec string_of_statement = function
  | Ruleset (selectors, decls) ->
    cat ", " string_of_selector selectors ^
    block (cat "\n" string_of_declaration decls)
  | Media (queries, rulesets) ->
    "@media " ^ String.concat ", " queries ^
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

let rec minify_statement = function
  | Ruleset (selectors, decls) ->
    cat "," minify_selector selectors ^
    "{" ^ (cat ";" minify_declaration decls) ^ "}"
  | Media (queries, rulesets) ->
    "@media " ^ String.concat "," queries ^
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
