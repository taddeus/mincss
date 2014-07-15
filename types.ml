type expr =
  | Ident of string
  | Strlit of string
  | Uri of string
  | Concat of expr list
  | Number of float * string option
  | Function of string * expr
  | Hexcolor of string
  | Unary of string * expr
  | Nary of string * expr list

type declaration = string * expr * bool

type selector = string list

type statement =
  | Ruleset of selector list * declaration list
  (* <selectors> { <declarations> } *)
  | Media of string list * statement list
  (* @media <queries> { <rulesets> } *)
  | Import of string * string list
  (* @import "<file>" [<media>]; *)
  | Charset of string
  (* @charset "<charset>"; *)
  | Page of string option * declaration list
  (* @page [<pseudo_page>] { <declarations> } *)
  | Fontface of declaration list
  (* @font-face { <declarations> } *)
  | Namespace of string option * string
  (* @namespace [<prefix>] "<uri>"; *)
  (* TODO: @document, @keyframes, @supports *)

type stylesheet = statement list

type args = {
  mutable infiles : string list;
  mutable outfile : string option;
  mutable verbose : int;
}

type loc = string * int * int * int * int

exception SyntaxError of string

exception LocError of loc * string
