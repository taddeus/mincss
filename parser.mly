%{
open Lexing
open Types

let filter_none l =
  let rec filter l = function
    | [] -> []
    | None :: tl -> filter l tl
    | Some hd :: tl -> filter (hd :: l) tl
  in
  List.rev (filter [] l)
%}

(* Tokens *)
%token S CDO CDC IMPORT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM
%token IMPORTANT_SYM
%token <float> NUMBER
%token <float * string> UNIT_VALUE
%token <string> COMBINATOR RELATION STRING IDENT HASH URI FUNCTION
%token RPAREN LBRACE RBRACE LBRACK RBRACK SEMICOL COLON COMMA DOT PLUS MINUS
%token SLASH STAR

(* Start symbol *)
%type <Types.stylesheet> stylesheet
%start stylesheet

%%

%inline mylist(sep, x):
  | l=separated_list(sep, delimited(S*, x, S*))
  { l }

cd: CDO S* | CDC S* {}

%inline statement: r=ruleset | r=media | r=page { r }
stylesheet:
  | charset    = charset? S* cd*
    imports    = terminated(import, cd*)*
    statements = terminated(statement, cd*)*
  { let charset = match charset with None -> [] | Some c -> [c] in
    charset @ imports @ statements }

charset:
  | CHARSET_SYM set=STRING SEMICOL
  { Charset set }

%inline string_or_uri: s=STRING | s=URI { s }
import:
  | IMPORT_SYM S* tgt=string_or_uri media=mylist(COMMA, IDENT) SEMICOL S*
  { Import (tgt, media) }

media:
  | MEDIA_SYM S* queries=mylist(COMMA, IDENT) LBRACE S* rulesets=ruleset* RBRACE S*
  { Media (queries, rulesets) }

page:
  | PAGE_SYM S* pseudo=pseudo_page? decls=decls_block
  { Page (pseudo, decls) }

pseudo_page:
  | COLON pseudo=IDENT S*
  { pseudo }

decls_block:
  | LBRACE S* decls=mylist(SEMICOL, declaration?) RBRACE S*
  { filter_none decls }

ruleset:
  | selectors_hd = selector
    selectors_tl = separated_list(COMMA, preceded(S*, selector))
    decls        = decls_block
  { Ruleset (selectors_hd :: selectors_tl, decls) }

%inline combinator:
  | S* PLUS S*          { ["+"] }
  | S* c=COMBINATOR S*  { [c] }
  | S+                  { [] }
selector:
  | hd=simple_selector comb=combinator tl=selector
  { hd :: comb @ tl }
  | simple=simple_selector
  { [simple] }

simple_selector:
  | elem=element_name addons=element_addon*
  { elem ^ String.concat "" addons }
  | addons=element_addon+
  { String.concat "" addons }

element_addon:
  | a=HASH | a=cls | a=attrib | a=pseudo
  { a }

cls:
  | DOT name=IDENT
  { "." ^ name }

element_name:
  | tag=IDENT  { tag }
  | STAR       { "*" }

%inline rel_value:
  | S* id=IDENT S*  { id }
  | S* s=STRING S*  { s }
attrib:
  | LBRACK S* left=IDENT S* right=pair(RELATION, rel_value)? RBRACK
  { left ^ (match right with None -> "" | Some (rel, term) -> rel ^ term) }

pseudo:
  | COLON id=IDENT
  { ":" ^ id }
  | COLON f=FUNCTION S* arg=terminated(IDENT, S*)? RPAREN
  { let arg = match arg with None -> "" | Some id -> id in
    ":" ^ f ^ "(" ^ arg ^ ")" }

declaration:
  | name=IDENT S* COLON S* value=expr IMPORTANT_SYM S*
  { (name, Prio value) }
  | name=IDENT S* COLON S* value=expr
  { (name, value) }

%inline unary_operator:
  | MINUS  { "-" }
  | PLUS   { "+" }
expr:
  | left=expr right=expr
  { Concat [left; right] }
  | left=expr SLASH S* right=expr
  { Binop (left, "/", right) }
  | op=unary_operator n=NUMBER S*
  { Unop (op, Number n) }
  | op=unary_operator v=UNIT_VALUE S*
  { let (n, u) = v in Unop (op, Unit (n, u)) }
  | n=NUMBER S*
  { Number n }
  | v=UNIT_VALUE S*
  { let (n, u) = v in Unit (n, u) }
  | str=STRING S*
  { Strlit str }
  | id=IDENT S*
  { Ident id }
  | uri=URI S*
  { Uri uri }
  | fn=FUNCTION S* args=separated_list(COMMA, terminated(expr, S*)) RPAREN S*
  { Function (fn, args) }
  | hex=HASH S*
  { if Str.string_match (Str.regexp "\\d{3}\\d{3}?") hex 0
      then Hexcolor hex
      else raise (SyntaxError ("invalid color #" ^ hex)) }
