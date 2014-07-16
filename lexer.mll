{
  (* Tokenizer according to definition at
   * http://www.w3.org/TR/CSS2/syndata.html#tokenization *)
  open Lexing
  open Parser
  open Types

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {
      pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum + 1
    }

  let strip_quotes s = String.sub s 1 (String.length s - 2)
}

let h           = ['0'-'9' 'a'-'f' 'A'-'F']
let wc          = '\r' '\n' | [' ' '\t' '\r' '\n' '\012']
let nonascii    = ['\160'-'\255']
let s           = [' ' '\t' '\r' '\n' '\012']+
let w           = s?
let nl          = '\n' | '\r' '\n' | '\r' | '\012'
let unicode     = '\\' h(h(h(h(h(h)?)?)?)?)? wc?
let escape      = unicode | '\\'[^'\r' '\n' '\012' '0'-'9' 'a'-'f' 'A'-'F']
let nmstart     = ['_' 'a'-'z' 'A'-'Z'] | nonascii | escape
let nmchar      = ['_' 'a'-'z' 'A'-'Z' '0'-'9' '-'] | nonascii | escape
let string1     = '"'([^'\n' '\r' '\012' '"'] | '\\'nl | escape)*'"'
let string2     = '\'' ([^'\n' '\r' '\012' '\''] | '\\' nl | escape)* '\''
let mystring    = string1 | string2
let badstring1  = '"' ([^'\n' '\r' '\012' '"'] | '\\'nl | escape)* '\\'?
let badstring2  = '\'' ([^'\n' '\r' '\012' '\''] | '\\'nl | escape)* '\\'?
let badstring   = badstring1 | badstring2
let badcomment1 = '/' '*'[^'*']*'*'+([^'/' '*'][^'*']*'*'+)*
let badcomment2 = '/' '*'[^'*']*('*'+[^'/' '*'][^'*']*)*
let badcomment  = badcomment1 | badcomment2
let baduri1     = "url(" w (['!' '#' '$' '%' '&' '*'-'[' ']'-'~'] | nonascii | escape)* w
let baduri2     = "url(" w mystring w
let baduri3     = "url(" w badstring
let baduri      = baduri1 | baduri2 | baduri3
let comment     = "/*" [^'*']* '*'+ ([^'/' '*'] [^'*']* '*'+) "*/"
let ident       = '-'? nmstart nmchar*
let name        = nmchar+
let num         = ['0'-'9']+ | ['0'-'9']*'.'['0'-'9']+
let url         = (['!' '#' '$' '%' '&' '*'-'~'] | nonascii | escape)*

rule token = parse
  | s                   { S }

  | comment             (* ignore comments *)
  | badcomment          (* unclosed comment at EOF *)

  | "<!--"              { CDO }
  | "-->"               { CDC }
  | ['~''|']?'=' as op  { RELATION op }
  | ['>''~'] as c       { COMBINATOR (Char.escaped c) }

  | mystring as s       { STRING (strip_quotes s) }
  | badstring           { raise (SyntaxError "bad string") }

  | ident as id         { IDENT id }

  | '#' (name as nm)    { HASH nm }

  | "@import"           { IMPORT_SYM }
  | "@page"             { PAGE_SYM }
  | "@media"            { MEDIA_SYM }
  | "@charset"          { CHARSET_SYM }

  | '!' (w | comment)* "important"  { IMPORTANT_SYM }

  | (num as n) ("em"|"ex"|"px"|"cm"|"mm"|"in"|"pt"|"pc"|"deg"|"rad"|"grad"|
                "ms"|"s"|"hz"|"khz"|"%"|ident as u)
  { UNIT_VALUE (float_of_string n, u) }
  | num as n            { NUMBER (float_of_string n) }

  | "url(" w (mystring as uri) w ")"  { URI (strip_quotes uri) }
  | "url(" w (url as uri) w ")"       { URI uri }
  | baduri                            { raise (SyntaxError "bad uri") }

  | (ident as fn) '('   { FUNCTION fn }

  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | ';'                 { SEMICOL }
  | ':'                 { COLON }
  | ','                 { COMMA }

  | '.'                 { DOT }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '/'                 { SLASH }
  | '*'                 { STAR }

  | eof | '\000'        { EOF }

  | _ as c { raise (SyntaxError ("unexpected '" ^ Char.escaped c ^ "'")) }
