type value =
  | Lit of string
  | Str of string
  | Lst of value list
  | Dim of float * string
  | Fn of string * value
  | Imp

type prop = string * value

type selector = string list

type decl =
  | Group of selector list * prop list  (* <selectors> { <props> } *)
  | Media of string list * decl list    (* @media <queries> { <groups> } *)
  | Import of string * string list      (* @import "<file>" [<media>]; *)
  | Charset of string                   (* @charset "<charset>"; *)
  | Page of string option * prop list   (* @page [<query>] { <props> } *)
  | Fontface of prop list               (* @font-face { <props> } *)
  | Namespace of string option * string (* @namespace [<prefix>] "<uri>"; *)
  (* TODO: @document, @keyframes, @supports *)

type args = {
  mutable infiles : string list;
  mutable outfile : string option;
  mutable verbose : int;
}

type loc = string * int * int * int * int

exception LocError of loc * string
