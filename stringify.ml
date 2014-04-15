open Types

let tab = "    "

let indent = Str.global_replace (Str.regexp "^\\(.\\)") (tab ^ "\\1")

let rec cat sep fn = function
  | [] -> ""
  | [hd] -> fn hd
  | hd :: tl -> fn hd ^ sep ^ cat sep fn tl

let rec value2str = function
  | Lit lit -> lit
  | Str str -> "\"" ^ str ^ "\""
  | Lst values -> cat " " value2str values
  | Dim (x, u) when float_of_int (int_of_float x) = x ->
    string_of_int (int_of_float x) ^ u
  | Dim (x, u) -> string_of_float x ^ u
  | Fn (name, arg) -> name ^ "(" ^ value2str arg ^ ")"
  | Imp -> "!important"

let prop2str (name, value) = name ^ ": " ^ value2str value ^ ";"

let block body = " {\n" ^ indent body ^ "\n}"

let rec decl2str = function
  | Group (selectors, props) ->
    cat ", " (String.concat " ") selectors ^ block (cat "\n" prop2str props)
  | Media (queries, groups) ->
    "@media " ^ String.concat ", " queries ^ block (cat "\n\n" decl2str groups)
  | Import (filename, []) ->
    "@import \"" ^ filename ^ "\";"
  | Import (filename, queries) ->
    "@import \"" ^ filename ^ "\" " ^ String.concat ", " queries ^ ";"
  | Charset charset ->
    "@charset \"" ^ charset ^ "\";"
  | Page (None, props) ->
    "@page" ^ block (cat "\n" prop2str props)
  | Page (Some query, props) ->
    "@page " ^ query ^ block (cat "\n" prop2str props)
  | Fontface props ->
    "@font-face " ^ block (cat "\n" prop2str props)
  | Namespace (None, uri) ->
    "@namespace \"" ^ uri ^ "\";"
  | Namespace (Some prefix, uri) ->
    "@namespace " ^ prefix ^ " \"" ^ uri ^ "\";"

let decls2str = cat "\n\n" decl2str
