{
  (* Tokenizer according to definition at
   * http://www.w3.org/TR/CSS2/syndata.html#tokenization *)
  open Lexing
  open Parser
  open Types

  let advance_pos lexbuf =
    let s = Lexing.lexeme lexbuf in
    let rec search from lines =
      try
        ignore (Str.search_forward (Str.regexp "\r\n\\|\r\\|\n") s from);
        search (Str.match_end ()) (lines + 1)
      with Not_found ->
        lines, String.length s - from
    in
    let lines, cols = search 0 0 in

    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {
      pos with pos_bol = lexbuf.lex_curr_pos - cols;
               pos_lnum = pos.pos_lnum + lines
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
let string1     = '"' ([^'\n' '\r' '\012' '"'] | '\\'nl | escape)* '"'
let string2     = '\'' ([^'\n' '\r' '\012' '\''] | '\\' nl | escape)* '\''
let mystring    = string1 | string2
let badstring1  = '"' ([^'\n' '\r' '\012' '"'] | '\\'nl | escape)* '\\'?
let badstring2  = '\'' ([^'\n' '\r' '\012' '\''] | '\\'nl | escape)* '\\'?
let badstring   = badstring1 | badstring2
let badcomment1 = "/*" [^'*']* '*'+ ([^'/' '*'] [^'*']* '*'+)*
let badcomment2 = "/*" [^'*']* ('*'+ [^'/' '*'] [^'*']*)*
let badcomment  = badcomment1 | badcomment2
let baduri1     = "url(" w (['!' '#' '$' '%' '&' '*'-'[' ']'-'~'] | nonascii | escape)* w
let baduri2     = "url(" w mystring w
let baduri3     = "url(" w badstring
let baduri      = baduri1 | baduri2 | baduri3
let comment     = "/*" [^'*']* '*'+ ([^'/' '*'] [^'*']* '*'+)* '/'
let ident       = '-'? nmstart nmchar*
let name        = nmchar+
let num         = ['0'-'9']+ | ['0'-'9']* '.' ['0'-'9']+
let url         = (['!' '#' '$' '%' '&' '*'-'~'] | nonascii | escape)*

let A = ['a' 'A']
let B = ['b' 'B']
let C = ['c' 'C']
let D = ['d' 'D']
let E = ['e' 'E']
let F = ['f' 'F']
let G = ['g' 'G']
let H = ['h' 'H']
let I = ['i' 'I']
let J = ['j' 'J']
let K = ['k' 'K']
let L = ['l' 'L']
let M = ['m' 'M']
let N = ['n' 'N']
let O = ['o' 'O']
let P = ['p' 'P']
let Q = ['q' 'Q']
let R = ['r' 'R']
let S = ['s' 'S']
let T = ['t' 'T']
let U = ['u' 'U']
let V = ['v' 'V']
let W = ['w' 'W']
let X = ['x' 'X']
let Y = ['y' 'Y']
let Z = ['z' 'Z']


rule token = parse
  | "\r\n" | '\r' | '\n'  { new_line lexbuf; S }
  | [' ' '\t' '\012']+  { S }
  | "/*"                { comment lexbuf }

  | "<!--"              { CDO }
  | "-->"               { CDC }
  | ['~' '^' '$' '*' '|']? '=' as op  { RELATION op }
  | ['>' '~'] as c      { COMBINATOR (Char.escaped c) }

  | mystring as s       { STRING (strip_quotes s) }
  | badstring           { raise (Syntax_error "bad string") }

  | '#' (name as nm)    { HASH nm }

  | '@' I M P O R T               { IMPORT_SYM }
  | '@' P A G E                   { PAGE_SYM }
  | '@' M E D I A                 { MEDIA_SYM }
  | "@charset "                   { CHARSET_SYM }
  | '@' F O N T '-' F A C E       { FONT_FACE_SYM }
  | '@' N A M E S P A C E         { NAMESPACE_SYM }
  | '@' S U P P O R T S           { SUPPORTS_SYM }
  | '@' (('-' ident '-')? as prefix) K E Y F R A M E S
  { KEYFRAMES_SYM (String.lowercase prefix) }
  | '@' (('-' ident '-')? as prefix) V I E W P O R T
  { VIEWPORT_SYM (String.lowercase prefix) }

  | (s | comment)* s comment* A N D comment* s (s | comment)*
  { advance_pos lexbuf; WS_AND }
  | (s | comment)* s comment* O R   comment* s (s | comment)*
  { advance_pos lexbuf; WS_OR }

  | O N L Y             { ONLY }
  | N O T               { NOT }
  | A N D               { AND }
  (*| O R                 { OR } removed in favor of WS_OR *)
  | F R O M             { FROM }
  | T O                 { TO }

  | ident as id         { IDENT id }

  | '!' (s | comment)* I M P O R T A N T  { IMPORTANT_SYM }

  |  (num as n) '%'     { PERCENTAGE (float_of_string n) }
  |  (num as n) (E M | E X | P X | C M | M M | I N | P T | P C | D E G |
                 G? R A D | M? S | K? H Z | D P (I | C M) | ident as u)
  { UNIT_VALUE (float_of_string n, u) }
  | num as n            { NUMBER (float_of_string n) }

  | "url(" w (mystring as uri) w ")"  { advance_pos lexbuf; URI (strip_quotes uri) }
  | "url(" w (url as uri) w ")"       { advance_pos lexbuf; URI uri }
  | baduri              { raise (Syntax_error "bad uri") }
  (*
  | "url("              { url_start lexbuf }
  *)

  | (ident as fn) '('   { FUNCTION fn }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | ';'                 { SEMICOL }
  | ':'                 { COLON }
  | "::"                { DOUBLE_COLON }
  | ','                 { COMMA }

  | '.'                 { DOT }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '/'                 { SLASH }
  | '*'                 { STAR }

  | eof | '\000'        { EOF }

  | _ as c { raise (Syntax_error ("unexpected '" ^ Char.escaped c ^ "'")) }

(* Comments *)
and comment = parse
  | '\r' | '\n' | "\r\n"  { new_line lexbuf; comment lexbuf }
  | "*/"                  { token lexbuf }
  | eof | '\000'          { raise (Syntax_error "unclosed comment") }
  | _                     { comment lexbuf }

(*
(* URLs *)
and url_start = parse
  | '\r' | '\n' | "\r\n"  { new_line lexbuf; url_start lexbuf }
  | [' ' '\t' '\012']+    { url_start lexbuf }
  | urlc+ as uri          { url_end uri lexbuf }
  | ')'                   { URI "" }
  | mystring as s         { url_end (strip_quotes s) lexbuf }
  | badstring             { raise (Syntax_error "bad string") }
  | (eof | '\000' | _) as c { raise (Syntax_error ("unexpected '" ^ c ^ "'")) }
and url_end uri = parse
  | '\r' | '\n' | "\r\n"  { new_line lexbuf; url_end uri lexbuf }
  | [' ' '\t' '\012']+    { url_end uri lexbuf }
  | ')'                   { URI uri }
  | (eof | '\000' | _) as c { raise (Syntax_error ("unexpected '" ^ c ^ "'")) }
*)
