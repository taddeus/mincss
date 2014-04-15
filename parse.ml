open Lexing
open Types

let loc_from_lexpos pstart pend =
  let (fname, ystart, yend, xstart, xend) = begin
    pstart.pos_fname,
    pstart.pos_lnum,
    pend.pos_lnum,
    (pstart.pos_cnum - pstart.pos_bol + 1),
    (pend.pos_cnum - pend.pos_bol)
  end in
  if ystart = yend && xend < xstart then
    (fname, ystart, yend, xstart, xstart)
  else
    (fname, ystart, yend, xstart, xend)

let get_loc lexbuf =
  loc_from_lexpos lexbuf.lex_curr_p lexbuf.lex_curr_p

let shift_loc (fname, ystart, yend, xstart, xend) yshift xshift =
  (fname, ystart + yshift, yend + yshift, xstart + xshift, xend + xshift)

let shift_back lexbuf =
  shift_loc (get_loc lexbuf) 0 (-1)

let parse_input display_name content =
  let lexbuf = Lexing.from_string content in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = display_name };
  try Parser.stylesheet Lexer.token lexbuf with
  | Lexer.SyntaxError msg ->
    raise (LocError (shift_back lexbuf, msg))
  | Parser.Error ->
    raise (LocError (shift_back lexbuf, "syntax error"))
