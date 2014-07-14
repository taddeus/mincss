%{
open Lexing
open Types

let prop2str (name, value) = name ^ ":" ^ Stringify.value2str value
%}

(* Tokens *)
%token S CDO CDC INCLUDES DASHMATCH STRING BAD_STRING IMPORT_SYM PAGE_SYM
%token MEDIA_SYM CHARSET_SYM IMPORTANT_SYM
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMICOL COLON
%token <int> EMS EXS PERCENTAGE NUMBER
%token <int * string> LENGTH ANGLE TIME FREQ DIMENSION
%token <string> IDENT HASH URI BAD_URI FUNCTION

(* Start symbol *)
%type <Types.decl list> stylesheet
%start stylesheet

%%

(* Left-recursive list (use List.rev to obtain correctly ordered list) *)
(*
llist(x):
  |            { [] }
  | tl=llist(x) hd=x { hd :: tl }
*)

stylesheet:
  | ( CDO | CDC | S | statement )*

statement:
  | ruleset
  | at_rule

at_rule:
  | ATKEYWORD S* any* ( block | SEMICOL S* )

block:
  | LBRACE S* ( any | block | ATKEYWORD S* | SEMICOL S* )* RBRACE S*

ruleset:
  | selectors=any+ LBRACE S* declaration? ( SEMICOL S* declaration? )* RBRACE S*

declaration:
  | name=IDENT S* COLON S* value=value
  { Property (name, value) }

value:
  | ( any | block | ATKEYWORD S* )+

any:
  | ( IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING | DELIM | URI | HASH |
  UNICODE-RANGE | INCLUDES | DASHMATCH | COLON | FUNCTION S* (any|unused)*
  RPAREN | LPAREN S* (any|unused)* RPAREN | LBRACK S* (any|unused)* RBRACK) S*

unused:
  | block
  | ATKEYWORD S*
  | SEMICOL S*
  | CDO S*
  | CDC S*
