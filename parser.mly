%{
  (* CSS grammar based on:
   * - http://www.w3.org/TR/CSS2/grammar.html
   * - http://www.w3.org/TR/css3-mediaqueries/
   * - http://www.w3.org/TR/css3-fonts/
   * - http://www.w3.org/TR/css3-namespace/
   * - http://www.w3.org/TR/css3-animations/
   * - http://www.w3.org/TR/css3-conditional/
   *)
  open Lexing
  open Types
  open Util

  type term = Term of expr | Operator of string

  let concat_terms terms =
    let rec transform_ops = function
      | [] -> []
      | Term left :: Operator op :: Term right :: tl ->
        transform_ops (Term (Nary (op, [left; right])) :: tl)
      | Term hd :: tl -> hd :: transform_ops tl
      | Operator op :: _ -> raise (Syntax_error ("unexpected operator \"" ^ op ^ "\""))
    in
    let rec flatten_nary = function
      | [] -> []
      | Nary (op1, Nary (op2, left) :: right) :: tl when op1 = op2 ->
        flatten_nary (Nary (op1, flatten_nary left @ flatten_nary right) :: tl)
      | hd :: tl -> hd :: flatten_nary tl
    in
    match terms |> transform_ops |> flatten_nary with
    | [hd] -> hd
    | l -> Concat l

  (* TODO: move this to a normalization stage, because the syntax should be
   * preserved during parsing (e.g. for -echo command) *)
  let unary_number = function
    | Unary ("-", Number (n, u)) -> Number (-.n, u)
    | Unary ("+", (Number _ as n)) -> n
    | value -> value

  let rec append_addons base = function
    | [] ->
      base
    | `Id id :: tl ->
      append_addons (Id (base, id)) tl
    | `Class cls :: tl ->
      append_addons (Class (base, cls)) tl
    | `Attribute (attr, value) :: tl ->
      append_addons (Attribute (base, attr, value)) tl
    | `Pseudo_class (f, args) :: tl ->
      append_addons (Pseudo_class (base, f, args)) tl
    | `Pseudo_element elem :: tl ->
      append_addons (Pseudo_element (base, elem)) tl
%}

(* Tokens *)
%token S CDO CDC IMPORT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM FONT_FACE_SYM
%token NAMESPACE_SYM SUPPORTS_SYM IMPORTANT_SYM
%token <float> PERCENTAGE NUMBER
%token <float * string> UNIT_VALUE
%token <string> KEYFRAMES_SYM VIEWPORT_SYM COMBINATOR RELATION STRING IDENT HASH
%token <string> URI FUNCTION
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMICOL COLON DOUBLE_COLON
%token COMMA DOT PLUS MINUS SLASH STAR ONLY AND (*OR*) NOT FROM TO EOF
%token WS_AND WS_OR ODD EVEN
%token <int * int> FORMULA

(* Start symbol *)
%type <Types.stylesheet> stylesheet
%start stylesheet

%%

(* list with arbitrary whitespace between elements and separators *)
%inline ig2(a, b): a b {}
%inline ig3(a, b, c): a b c {}
%inline wslist(sep, x): S* l=separated_list(sep, terminated(x, S*))  { l }
%inline wspreceded(prefix, x): p=preceded(ig2(prefix, S*), x) { p }

%inline all_and: AND | WS_AND {}

cd: CDO S* | CDC S* {}

stylesheet:
  | charset    = charset? S* cd*
    imports    = terminated(import, cd*)*
    namespaces = terminated(namespace, cd*)*
    statements = terminated(nested_statement, cd*)*
                 EOF
  { let charset = match charset with None -> [] | Some c -> [c] in
    charset @ imports @ namespaces @ statements }

nested_statement:
  | s=ruleset | s=media | s=page | s=font_face_rule | s=keyframes_rule
  | s=supports_rule | s=viewport_rule
  { s }

group_rule_body:
  | LBRACE S* statements=nested_statement* RBRACE S*
  { statements }

charset:
  | CHARSET_SYM name=STRING S* SEMICOL
  { Charset name }

import:
  | IMPORT_SYM S* tgt=string_or_uri media=media_query_list SEMICOL S*
  { Import (tgt, media) }
%inline string_or_uri:
  | str=STRING  { Strlit str }
  | uri=URI     { Uri uri }

namespace:
  | NAMESPACE_SYM S* prefix=terminated(namespace_prefix, S*)? ns=string_or_uri S* SEMICOL S*
  { Namespace (prefix, ns) }
%inline namespace_prefix:
  | prefix=IDENT
  { prefix }

media:
  | MEDIA_SYM queries=media_query_list rulesets=group_rule_body
  { Media (queries, rulesets) }
media_query_list:
  | S*
  { [] }
  | S* hd=media_query tl=wspreceded(COMMA, media_query)*
  { hd :: tl }
media_query:
  | prefix=only_or_not? typ=media_type S* feat=wspreceded(all_and, media_expr)*
  { (prefix, Some typ, feat) }
  | hd=media_expr tl=wspreceded(all_and, media_expr)*
  { (None, None, (hd :: tl)) }
%inline only_or_not:
  | ONLY S*   { "only" }
  | NOT S*    { "not" }
%inline media_type:
  | id=IDENT  { id }
media_expr:
  | LPAREN S* feature=media_feature S* value=wspreceded(COLON, expr)? RPAREN S*
  { (feature, value) }
%inline media_feature:
  | id=IDENT  { id }

page:
  | PAGE_SYM S* pseudo=pseudo_page? decls=decls_block
  { Page (pseudo, decls) }
pseudo_page:
  | COLON pseudo=IDENT S*
  { pseudo }

font_face_rule:
  | FONT_FACE_SYM S* LBRACE S* hd=descriptor_declaration?
    tl=wspreceded(SEMICOL, descriptor_declaration?)* RBRACE S*
  { Font_face (filter_none (hd :: tl)) }
descriptor_declaration:
  | name=property COLON S* value=expr
  { (name, value) }

keyframes_rule:
  | pre=KEYFRAMES_SYM S* id=IDENT S* LBRACE S* rules=keyframe_ruleset* RBRACE S*
  { Keyframes (pre, id, rules) }
keyframe_ruleset:
  | selector=keyframe_selector S* decls=decls_block
  { (selector, decls) }
keyframe_selector:
  | FROM          { Ident "from" }
  | TO            { Ident "to" }
  | n=PERCENTAGE  { Number (n, Some "%") }

supports_rule:
  | SUPPORTS_SYM S* cond=supports_condition S* body=group_rule_body
  { Supports (cond, body) }
supports_condition:
  | c=supports_negation
  | c=supports_conjunction
  | c=supports_disjunction
  | c=supports_condition_in_parens
  { c }
supports_condition_in_parens:
  | LPAREN S* c=supports_condition S* RPAREN
  | c=supports_declaration_condition
  (*XXX: | c=general_enclosed*)
  { c }
supports_negation:
  | NOT S+ c=supports_condition_in_parens
  { Not c }
supports_conjunction:
  | hd=supports_condition_in_parens tl=preceded(WS_AND, supports_condition_in_parens)+
  { And (hd :: tl) }
supports_disjunction:
  | hd=supports_condition_in_parens tl=preceded(WS_OR, supports_condition_in_parens)+
  { Or (hd :: tl) }
supports_declaration_condition:
  | LPAREN S* decl=supports_declaration RPAREN
  { Decl decl }
supports_declaration:
  | name=property S* COLON S* value=expr
  { (name, value) }
  (*XXX:
general_enclosed:
  | ( FUNCTION | LPAREN ) ( any | unused )* RPAREN
  { Enclosed expr }

any:
[ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
              | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
              | DASHMATCH | ':' | FUNCTION S* [any|unused]* ')'
              | '(' S* [any|unused]* ')' | '[' S* [any|unused]* ']'
              ]
S*;
unused      : block | ATKEYWORD S* | ';' S* | CDO S* | CDC S*;
  *)

viewport_rule:
  | pre=VIEWPORT_SYM S* decls=decls_block
  { Viewport (pre, decls) }

%inline decls_block:
  | LBRACE S* hd=declaration? tl=wspreceded(SEMICOL, declaration?)* RBRACE S*
  { filter_none (hd :: tl) }

ruleset:
  | selectors_hd = selector
    selectors_tl = wspreceded(COMMA, selector)*
    decls        = decls_block
  { Ruleset (selectors_hd :: selectors_tl, decls) }

selector:
  | simple=simple_selector S*
  { simple }
  | left=simple_selector S+ right=selector
  { Combinator (left, " ", right) }
  | left=simple_selector S* com=combinator right=selector
  { Combinator (left, com, right) }
%inline combinator:
  | PLUS S*          { "+" }
  | c=COMBINATOR S*  { c }

simple_selector:
  | elem=element_name addons=element_addon*
  { append_addons elem addons }
  | addons=element_addon+
  { append_addons No_element addons }
%inline element_addon:
  | id=HASH  { `Id id }
  | addon=cls | addon=attrib | addon=pseudo_class  { addon }
element_name:
  | tag=IDENT  { Element (String.lowercase_ascii tag) }
  | STAR       { All_elements }
cls:
  | DOT name=IDENT
  { `Class name }
attrib:
  | LBRACK S* left=IDENT S* RBRACK
  { `Attribute (String.lowercase_ascii left, None) }
  | LBRACK S* left=IDENT S* op=RELATION right=rel_value RBRACK
  { `Attribute (String.lowercase_ascii left, Some (op, right)) }
%inline rel_value:
  | S* id=IDENT S*  { Ident id }
  | S* s=STRING S*  { Strlit s }
pseudo_class:
  | COLON id=IDENT
  { `Pseudo_class (String.lowercase_ascii id, None) }
  | COLON f=FUNCTION args=wslist(COMMA, function_arg) RPAREN
  { `Pseudo_class (String.lowercase_ascii f, Some args) }
  | DOUBLE_COLON id=IDENT
  { `Pseudo_element (String.lowercase_ascii id) }
function_arg:
  | s=simple_selector
  { Nested_selector s }
  | EVEN
  { Nth Even }
  | ODD
  { Nth Odd }
  | f=FORMULA
  { let a, b = f in Nth (Formula (a, b)) }
  | sign=sign? n=NUMBER
  {
    if is_int n then begin
      let b = int_of_float (match sign with Some MINUS -> -.n | _ -> n) in
      Nth (Formula (0, b))
    end else
      raise (Syntax_error ("unexpected float '" ^ string_of_float n ^
                           "', expected int"))
  }
%inline sign: PLUS { PLUS } | MINUS { MINUS }

declaration:
  | name=property S* COLON S* value=expr important=boption(ig2(IMPORTANT_SYM, S*))
  { (String.lowercase_ascii name, value, important) }
%inline property:
  | name=IDENT       { name }
  | STAR name=IDENT  { "*" ^ name }  (* IE7 property name hack *)

expr:
  | l=exprl             { concat_terms l }
%inline exprl:
  | hd=term tl=opterm*  { Term hd :: List.concat tl }
%inline opterm:
  | t=term              { [Term t] }
  | op=operator t=term  { [Operator op; Term t] }
%inline operator:
  | SLASH S*            { "/" }
  | COMMA S*            { "," }

term:
  | op=unary_operator v=numval S*   { unary_number (Unary (op, v)) }
  | v=numval S*                     { v }
  | str=STRING S*                   { Strlit str }
  | id=IDENT S*                     { Ident (String.lowercase_ascii id) }
  | ONLY S*                         { Ident "only" }
  | NOT S*                          { Ident "not" }
  | AND S*                          { Ident "and" }
  | FROM S*                         { Ident "from" }
  | TO S*                           { Ident "to" }
  | uri=URI S*                      { Uri uri }
  | fn=FUNCTION arg=expr RPAREN S*  { Function (String.lowercase_ascii fn, arg) }
  | key=IDENT S* COLON S* value=term
  { Key_value (key, ":", value) }
  | key=IDENT S* DOT S* value=term
  { Key_value (key, ".", value) }
  | key=IDENT S* rel=RELATION S* value=term
  {
    if rel = "="
      then Key_value (key, "=", value)
      else raise (Syntax_error ("unexpected '" ^ rel ^ "'"))
  }
  | hex=HASH S*
  {
    let h = "[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]" in
    if Str.string_match (Str.regexp ("^" ^ h ^ "\\(" ^ h ^ "\\)?$")) hex 0
      then Hexcolor (String.lowercase_ascii hex)
      else raise (Syntax_error ("invalid color #" ^ hex))
  }
unary_operator:
  | MINUS  { "-" }
  | PLUS   { "+" }
%inline numval:
  | n=NUMBER      { Number (n, None) }
  | v=UNIT_VALUE  { let n, u = v in Number (n, Some (String.lowercase_ascii u)) }
  | n=PERCENTAGE  { Number (n, Some "%") }
