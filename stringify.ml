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

let rec string_of_value = function
  | Ident id -> id
  | Strlit str -> "\"" ^ str ^ "\""
  | Uri uri when String.contains uri ')' -> "url(\"" ^ uri ^ "\")"
  | Uri uri -> "url(" ^ uri ^ ")"
  | Concat values -> cat " " string_of_value values
  | Number n -> string_of_num n
  | Unit (n, u) -> string_of_num n ^ u
  | Function (name, args) -> name ^ "(" ^ cat "," string_of_value args ^ ")"
  | Hexcolor hex -> "#" ^ hex
  | Unop (op, opnd) -> op ^ string_of_value opnd
  | Binop (left, op, right) -> string_of_value left ^ op ^ string_of_value right
  | Prio value -> string_of_value value ^ " !important"

let string_of_declaration (name, value) =
  name ^ ": " ^ string_of_value value ^ ";"

let block body = " {\n" ^ indent body ^ "\n}"

let rec string_of_statement = function
  | Ruleset (selectors, decls) ->
    cat ", " (String.concat " ") selectors ^
    block (cat "\n" string_of_declaration decls)
  | Media (queries, rulesets) ->
    "@media " ^ String.concat ", " queries ^
    block (cat "\n\n" string_of_statement rulesets)
  | Import (filename, []) ->
    "@import \"" ^ filename ^ "\";"
  | Import (filename, queries) ->
    "@import \"" ^ filename ^ "\" " ^ String.concat ", " queries ^ ";"
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
