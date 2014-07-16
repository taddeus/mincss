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

type selector =
  | Simple of string
  | Combinator of selector * string * selector

type media_expr = string * expr option
type media_query = string option * string option * media_expr list

type descriptor_declaration = string * expr

type keyframe_ruleset = expr * declaration list

type statement =
  | Ruleset of selector list * declaration list
  (* <selectors> { <declarations> } *)
  | Media of media_query list * statement list
  (* @media <queries> { <rulesets> } *)
  | Import of expr * media_query list
  (* @import <target> [<media>]; *)
  | Charset of string
  (* @charset "<charset>"; *)
  | Page of string option * declaration list
  (* @page [<pseudo_page>] { <declarations> } *)
  | Font_face of descriptor_declaration list
  (* @font-face { <declarations> } *)
  | Namespace of string option * expr
  (* @namespace [<prefix>] "<uri>"; *)
  | Keyframes of string * keyframe_ruleset list
  (* TODO: @document, @supports *)

type stylesheet = statement list

type args = {
  mutable infiles : string list;
  mutable outfile : string option;
  mutable verbose : int;
}

type loc = string * int * int * int * int

exception SyntaxError of string

exception LocError of loc * string
