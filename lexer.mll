{
  (* Tokenizer according to definition at
   * http://www.w3.org/TR/CSS2/syndata.html#tokenization *)
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {
      pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum + 1
    }
}

let h           = ['0'-'9''a'-'f']
let wc          = '\r''\n' | [' ''\t''\r''\n''\012']
let nonascii    = ['\160'-'\255']
let s           = [' ''\t''\r''\n''\012']+
let w           = s?
let nl          = '\n' | '\r''\n' | '\r' | '\012'
let unicode     = '\\' h(h(h(h(h(h)?)?)?)?)? wc?
let escape      = unicode | '\\'[^'\r''\n''\012''0'-'9''a'-'f']
let nmstart     = ['_''a'-'z'] | nonascii | escape
let nmchar      = ['_''a'-'z''0'-'9''-'] | nonascii | escape
let string1     = '"'([^'\n''\r''\012''"'] | '\\'nl | escape)*'"'
let string2     = '\''([^'\n''\r''\012''\''] | '\\'nl | escape)*'\''
let mystring    = string1 | string2
let badstring1  = '"'([^'\n''\r''\012''"'] | '\\'nl | escape)*'\\'?
let badstring2  = '\''([^'\n''\r''\012''\''] | '\\'nl | escape)*'\\'?
let badstring   = badstring1 | badstring2
let badcomment1 = '/''*'[^'*']*'*'+([^'/''*'][^'*']*'*'+)*
let badcomment2 = '/''*'[^'*']*('*'+[^'/''*'][^'*']*)*
let badcomment  = badcomment1 | badcomment2
let baduri1     = "url("w(['!''#''$''%''&''*'-'['']'-'~'] | nonascii | escape)*w
let baduri2     = "url("w mystring w
let baduri3     = "url("w badstring
let baduri      = baduri1 | baduri2 | baduri3
let comment     = '/''*'[^'*']*'*'+([^'/''*'][^'*']*'*'+)'*''/'
let ident       = '-'? nmstart nmchar*
let name        = nmchar+
let num         = ['0'-'9']+ | ['0'-'9']*'.'['0'-'9']+
let url         = (['!''#''$''%''&''*''-''~'] | nonascii | escape)*

rule token = parse
  | s                   { S }

  | comment             (* ignore comments *)
  | badcomment          (* unclosed comment at EOF *)

  | "<!--"              { CDO }
  | "-->"               { CDC }
  | "~="                { INCLUDES }
  | "|="                { DASHMATCH }

  | mystring            { STRING }
  | badstring           { BAD_STRING }

  | ident as id         { IDENT id }

  | '#' (name as name)  { HASH name }

  | "@import"           { IMPORT_SYM }
  | "@page"             { PAGE_SYM }
  | "@media"            { MEDIA_SYM }
  | "@charset"          { CHARSET_SYM }

  | '!' (w | comment)* "important"  { IMPORTANT_SYM }

  | (num as n) "em"     { EMS (int_of_string n) }
  | (num as n) "ex"     { EXS (int_of_string n) }
  | (num as n) "px"     { LENGTH (int_of_string n, "px") }
  | (num as n) "cm"     { LENGTH (int_of_string n, "cm") }
  | (num as n) "mm"     { LENGTH (int_of_string n, "mm") }
  | (num as n) "in"     { LENGTH (int_of_string n, "in") }
  | (num as n) "pt"     { LENGTH (int_of_string n, "pt") }
  | (num as n) "pc"     { LENGTH (int_of_string n, "pc") }
  | (num as n) "deg"    { ANGLE (int_of_string n, "deg") }
  | (num as n) "rad"    { ANGLE (int_of_string n, "rad") }
  | (num as n) "grad"   { ANGLE (int_of_string n, "grad") }
  | (num as n) "ms"     { TIME (int_of_string n, "ms") }
  | (num as n) "s"      { TIME (int_of_string n, "s") }
  | (num as n) "hz"     { FREQ (int_of_string n, "hz") }
  | (num as n) "khz"    { FREQ (int_of_string n, "khz") }
  | (num as n) "%"      { PERCENTAGE (int_of_string n) }
  | (num as n) (ident as dim)  { DIMENSION (int_of_string n, dim) }
  | num as n            { NUMBER (int_of_string n) }

  | "url(" w (mystring as uri) w ")"  { URI uri }
  | "url(" w (url as uri) w ")"       { URI uri }
  | baduri as uri                     { BAD_URI uri }

  | (ident as fn) '('   { FUNCTION fn }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | ';'                 { SEMICOL }
  | ':'                 { COLON }

  (*
  | _ as c { raise (SyntaxError ("illegal string character: " ^ Char.escaped c)) }
  *)
