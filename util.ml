open Printf
open Str
open Types

let input_all ic =
  let n = in_channel_length ic in
  let buf = String.create n in
  really_input ic buf 0 n;
  close_in ic;
  buf

let input_buffered ic chunksize =
  let rec read_all buf bufsize pos =
    match input ic buf pos (bufsize - pos) with
    | 0 -> (close_in ic; buf)
    | nread when nread = bufsize - pos ->
      let bufsize = bufsize + chunksize in
      let pos = pos + nread in
      read_all (buf ^ String.create chunksize) bufsize pos
    | nread ->
      read_all buf bufsize (pos + nread)
  in
  read_all (String.create chunksize) chunksize 0

let output_css oc decls =
  output_string oc (Stringify.decls2str decls);
  output_char oc '\n'

let print_css = output_css stdout

let noloc = ("", 0, 0, 0, 0)

let tabwidth = 4

let count_tabs str upto =
  let rec count n = function
    | 0 -> n
    | i -> count (if String.get str (i - 1) = '\t' then n + 1 else n) (i - 1)
  in count 0 upto

let rec repeat s n = if n < 1 then "" else s ^ (repeat s (n - 1))

let retab str = global_replace (regexp "\t") (repeat " " tabwidth) str

let indent n = repeat (repeat " " (tabwidth - 1)) n

let prerr_loc (fname, ystart, yend, xstart, xend) =
  let file = open_in fname in

  (* skip lines until the first matched line *)
  for i = 1 to ystart - 1 do let _ = input_line file in () done;

  (* for each line in `loc`, print the source line with an underline *)
  for l = ystart to yend do
    let line = input_line file in
    let linewidth = String.length line in
    let left = if l = ystart then xstart else 1 in
    let right = if l = yend then xend else linewidth in
    if linewidth > 0 then begin
      prerr_endline (retab line);
      prerr_string (indent (count_tabs line right));
      for i = 1 to left - 1 do prerr_char ' ' done;
      for i = left to right do prerr_char '^' done;
      prerr_endline "";
    end
  done;
  ()

let prerr_loc_msg args loc msg =
  if args.verbose >= 1 then begin
    let (fname, ystart, yend, xstart, xend) = loc in
    if loc != noloc then begin
      let line_s = if yend != ystart
        then sprintf "lines %d-%d" ystart yend
        else sprintf "line %d" ystart
      in
      let char_s = if xend != xstart || yend != ystart
        then sprintf "characters %d-%d" xstart xend
        else sprintf "character %d" xstart
      in
      eprintf "File \"%s\", %s, %s:\n" fname line_s char_s;
    end;
    eprintf "%s\n" msg;

    if args.verbose >= 1 && loc != noloc then
        try prerr_loc loc
        with Sys_error _ -> ()
  end;
  ()
