{
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

rule token = parse
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ';' { SEMICOL }
  | ',' { COMMA }
  | ':' { COLON }

  | "@media"     { MEDIA }
  | "@import"    { IMPORT }
  | "@charset"   { CHARSET }
  | "@page"      { PAGE }
  | "@font-face" { FONTFACE }
  | "@namespace" { NAMESPACE }

  | "!important" { IMPORTANT }
  | ['A'-'Z''a'-'z''0'-'9''_''-''#''.']+ as id { ID id }
  | ['.''#'':']['A'-'Z''a'-'z''_''-']['A'-'Z''a'-'z''0'-'9''_''-''.''#'':']* as id { SELECTOR id }

  | '\r' | '\n' | "\r\n" { next_line lexbuf; token lexbuf }
  | [' ''\t']+           { token lexbuf }
  | "/*"                 { comment lexbuf }
  | '"'                  { str (Buffer.create 17) lexbuf }
  | eof | '\000'         { EOF }

  | _ as chr { raise (SyntaxError ("unexpected char: " ^ Char.escaped chr)) }

(* Multi-line comments *)
and comment = parse
  | '\r' | '\n' | "\r\n"  { next_line lexbuf; comment lexbuf }
  | "*/"                  { token lexbuf }
  | _                     { comment lexbuf }

(* Strings *)
and str buf = parse
  | '"'              { STRING (Buffer.contents buf) }
  | '\\''/'          { Buffer.add_char buf '/';    str buf lexbuf }
  | '\\''\\'         { Buffer.add_char buf '\\';   str buf lexbuf }
  | '\\''b'          { Buffer.add_char buf '\b';   str buf lexbuf }
  | '\\''f'          { Buffer.add_char buf '\012'; str buf lexbuf }
  | '\\''n'          { Buffer.add_char buf '\n';   str buf lexbuf }
  | '\\''r'          { Buffer.add_char buf '\r';   str buf lexbuf }
  | '\\''t'          { Buffer.add_char buf '\t';   str buf lexbuf }
  | [^'"''\\']+ as s { Buffer.add_string buf s;    str buf lexbuf }
  | eof              { raise (SyntaxError "unterminated string") }
  | _ as c { raise (SyntaxError ("illegal string character: " ^ Char.escaped c)) }
