%{
open Lexing
open Types

let prop2str (name, value) = name ^ ":" ^ Stringify.value2str value
%}

(* Tokens *)
%token LPAREN RPAREN LBRACE RBRACE SEMICOL COMMA COLON
%token MEDIA IMPORT CHARSET PAGE FONTFACE NAMESPACE
%token IMPORTANT EOF
%token <string> ID STRING SELECTOR

(* Start symbol *)
%type <Types.decl list> stylesheet
%start stylesheet

%%

(* Left-recursive list (use List.rev to obtain correctly ordered list) *)
llist(x):
  |            { [] }
  | tl=llist(x) hd=x { hd :: tl }

separated_llist(sep, x):
  |                      { [] }
  | tl=llist(x) sep hd=x { hd :: tl }

stylesheet:
  | decls=llist(decl) EOF
  { List.rev decls }

selector:
  | id=ID                    { [id] }
  | id=SELECTOR              { [id] }
  | tl=selector hd=ID        { hd :: tl }
  | tl=selector hd=SELECTOR  { hd :: tl }

value:
  | str=STRING                      { Str str }
  | lit=ID                          { Lit lit }
  | name=ID LPAREN arg=value RPAREN { Fn (name, arg) }
  | IMPORTANT                       { Imp }

prop:
  | name=ID COLON v=value+
  { (name, match v with [hd] -> hd | _ -> Lst v) }

propline:
  | p=prop SEMICOL
  { p }

props:
  | LBRACE p=llist(propline) last=prop? RBRACE
  { List.rev p @ (match last with None -> [] | Some p -> [p]) }

group:
  | s=separated_nonempty_list(COMMA, selector) p=props
  { Group (List.rev s, p) }

%inline media:
  | m=ID
  { m }
  | LPAREN p=prop RPAREN
  { "(" ^ prop2str p ^ ")" }

%inline stringopt: f=STRING | f=ID { f }

decl:
  | g=group
  { g }
  | MEDIA queries=separated_nonempty_list(COMMA, media) LBRACE groups=llist(group) RBRACE
  { Media (queries, List.rev groups) }
  | IMPORT f=stringopt q=separated_list(COMMA, ID) SEMICOL
  { Import (f, q) }
  | CHARSET c=stringopt SEMICOL
  { Charset c }
  | PAGE query=ID? p=props
  { Page (query, p) }
  | FONTFACE p=props
  { Fontface p }
  | NAMESPACE prefix=ID? uri=STRING SEMICOL
  { Namespace (prefix, uri) }

%%
