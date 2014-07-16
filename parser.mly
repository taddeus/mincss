%{
  (* CSS grammar based on:
   * - http://www.w3.org/TR/CSS2/grammar.html
   * - http://www.w3.org/TR/css3-mediaqueries/
   *)
  open Lexing
  open Types

  let ( |> ) a b = b a

  (* TODO: move this to utils *)
  let rec filter_none = function
    | [] -> []
    | None :: tl -> filter_none tl
    | Some hd :: tl -> hd :: filter_none tl

  type term_t = Term of expr | Operator of string

  let rec transform_value f = function
    | Concat terms -> Concat (List.map (transform_value f) terms)
    | Function (name, arg) -> Function (name, transform_value f arg)
    | Unary (op, term) -> Unary (op, transform_value f term)
    | Nary (op, terms) -> Nary (op, List.map (transform_value f) terms)
    | value -> f value

  let concat_terms terms =
    let rec transform_ops = function
      | [] -> []
      | Term left :: Operator op :: Term right :: tl ->
        transform_ops (Term (Nary (op, [left; right])) :: tl)
      | Term hd :: tl -> hd :: transform_ops tl
      | Operator op :: _ -> raise (SyntaxError ("unexpected operator \"" ^ op ^ "\""))
    in
    let rec flatten_nary = function
      | [] -> []
      | Nary (op, Nary (op2, left) :: right) :: tl when op2 = op ->
        Nary (op, flatten_nary left @ flatten_nary right) :: flatten_nary tl
      | hd :: tl -> hd :: flatten_nary tl
    in
    match terms |> transform_ops |> flatten_nary with
    | [hd] -> hd
    | l -> Concat l
%}

(* Tokens *)
%token S CDO CDC IMPORT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM
%token IMPORTANT_SYM
%token <float> NUMBER
%token <float * string> UNIT_VALUE
%token <string> COMBINATOR RELATION STRING IDENT HASH URI FUNCTION
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMICOL COLON COMMA DOT PLUS
%token MINUS SLASH STAR ONLY AND NOT EOF

(* Start symbol *)
%type <Types.stylesheet> stylesheet
%start stylesheet

%%

(* list with arbitrary whitespace between elements and separators *)
%inline wslist(sep, x): S? l=separated_list(sep, terminated(x, S?))  { l }
%inline wspreceded(prefix, x): p=preceded(pair(prefix, S?), x) { p }

cd: CDO S? | CDC S? {}

stylesheet:
  | charset    = charset? S? cd*
    imports    = terminated(import, cd*)*
    statements = terminated(statement, cd*)*
                 EOF
  { let charset = match charset with None -> [] | Some c -> [c] in
    charset @ imports @ statements }

statement:
  | s=ruleset | s=media | s=page
  { s }

charset:
  | CHARSET_SYM S? name=STRING S? SEMICOL
  { Charset name }

import:
  | IMPORT_SYM S? tgt=string_or_uri media=wslist(COMMA, media_type) SEMICOL S?
  { Import (tgt, media) }
%inline string_or_uri:
  | str=STRING  { Strlit str }
  | uri=URI     { Uri uri }

media:
  | MEDIA_SYM queries=media_query_list LBRACE S? rulesets=ruleset* RBRACE S?
  { Media (queries, rulesets) }
media_query_list:
  | S?
  { [] }
  | S? hd=media_query tl=wspreceded(COMMA, media_query)*
  { hd :: tl }
media_query:
  | prefix=only_or_not? typ=media_type S? feat=wspreceded(AND, media_expr)*
  { (prefix, Some typ, feat) }
  | hd=media_expr tl=wspreceded(AND, media_expr)*
  { (None, None, (hd :: tl)) }
%inline only_or_not:
  | ONLY S?   { "only" }
  | NOT S?    { "not" }
%inline media_type:
  | id=IDENT  { id }
media_expr:
  | LPAREN S? feature=media_feature S? value=wspreceded(COLON, expr)? RPAREN S?
  { (feature, value) }
%inline media_feature:
  | id=IDENT  { id }

page:
  | PAGE_SYM S? pseudo=pseudo_page? decls=decls_block
  { Page (pseudo, decls) }

pseudo_page:
  | COLON pseudo=IDENT S?
  { pseudo }

%inline decls_block:
  | LBRACE S? hd=declaration? tl=wspreceded(SEMICOL, declaration?)* RBRACE S?
  { filter_none (hd :: tl) }

ruleset:
  | selectors_hd = selector
    selectors_tl = wspreceded(COMMA, selector)*
    decls        = decls_block
  { Ruleset (selectors_hd :: selectors_tl, decls) }

selector:
  | simple=simple_selector S?
  { Simple simple }
  | left=simple_selector S right=selector
  { Combinator (Simple left, " ", right) }
  | left=simple_selector S? com=combinator right=selector
  { Combinator (Simple left, com, right) }
%inline combinator:
  | PLUS S?          { "+" }
  | c=COMBINATOR S?  { c }

simple_selector:
  | elem=element_name addons=element_addon*
  { elem ^ String.concat "" addons }
  | addons=element_addon+
  { String.concat "" addons }
%inline element_addon:
  | a=HASH | a=cls | a=attrib | a=pseudo
  { a }

element_name:
  | tag=IDENT  { tag }
  | STAR       { "*" }

cls:
  | DOT name=IDENT
  { "." ^ name }

attrib:
  | LBRACK S? left=IDENT S? right=pair(RELATION, rel_value)? RBRACK
  { let right = match right with None -> "" | Some (op, term) -> op ^ term in
    "[" ^ left ^ right ^ "]" }
%inline rel_value:
  | S? id=IDENT S?  { id }
  | S? s=STRING S?  { "\"" ^ s ^ "\"" }

pseudo:
  | COLON id=IDENT
  { ":" ^ id }
  | COLON f=FUNCTION S? arg=terminated(IDENT, S?)? RPAREN
  { let arg = match arg with None -> "" | Some id -> id in
    ":" ^ f ^ "(" ^ arg ^ ")" }

declaration:
  | name=IDENT S? COLON S? value=expr important=boption(pair(IMPORTANT_SYM, S?))
  { (String.lowercase name, value, important) }

expr:
  | l=exprl             { concat_terms l }
%inline exprl:
  | hd=term tl=opterm*  { Term hd :: List.concat tl }
%inline opterm:
  | t=term              { [Term t] }
  | op=operator t=term  { [Operator op; Term t] }
%inline operator:
  | SLASH S?            { "/" }
  | COMMA S?            { "," }

term:
  | op=unary_operator n=NUMBER S?
  { Unary (op, Number (n, None)) }
  | op=unary_operator v=UNIT_VALUE S?
  { let (n, u) = v in Unary (op, Number (n, Some u)) }
  | n=NUMBER S?
  { Number (n, None) }
  | v=UNIT_VALUE S?
  { let (n, u) = v in Number (n, Some u) }
  | str=STRING S?
  { Strlit str }
  | id=IDENT S?
  { Ident id }
  | uri=URI S?
  { Uri uri }
  | fn=FUNCTION arg=expr RPAREN S?
  { Function (fn, arg) }
  | hex=HASH S?
  { let h = "[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]" in
    if Str.string_match (Str.regexp ("^" ^ h ^ "\\(" ^ h ^ "\\)?$")) hex 0
      then Hexcolor (String.lowercase hex)
      else raise (SyntaxError ("invalid color #" ^ hex)) }

unary_operator:
  | MINUS  { "-" }
  | PLUS   { "+" }
