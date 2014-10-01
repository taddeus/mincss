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
  | No_element
  | All_elements
  | Element of string
  | Id of selector * string
  | Class of selector * string
  | Pseudo of selector * string * selector list option
  | Attribute of selector * string * (string * expr) option
  | Combinator of selector * string * selector

  (*
type selector =
  | Simple of string
  | Combinator of selector * string * selector
  *)

type media_expr = string * expr option
type media_query = string option * string option * media_expr list

type descriptor_declaration = string * expr

type keyframe_ruleset = expr * declaration list

type supports_declaration = string * expr

type condition =
  | Not of condition
  | And of condition list
  | Or of condition list
  | Decl of supports_declaration
  (*XXX: | Enclosed of expr*)

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
  | Keyframes of string * string * keyframe_ruleset list
  (* @[-<prefix>-]keyframes <id> { <rulesets> } *)
  | Supports of condition * statement list
  (* @supports <condition> { <rulesets> } *)

type stylesheet = statement list

type box =
  | Expr of expr
  | Declaration of declaration
  | Selector of selector
  | Media_expr of media_expr
  | Media_query of media_query
  | Descriptor_declaration of descriptor_declaration
  | Keyframe_ruleset of keyframe_ruleset
  | Supports_declaration of supports_declaration
  | Condition of condition
  | Statement of statement
  | Stylesheet of stylesheet
  | Clear

type loc = string * int * int * int * int

exception Syntax_error of string

exception Loc_error of loc * string

exception Box_error of box * string

exception Exit_success
