open Lexing
open Types

type args = {
  infiles    : string list;
  outfile    : string option;
  verbose    : bool;
  whitespace : bool;
  simple     : bool;
  shorthands : bool;
  duplicates : bool;
  sort       : bool;
}

let usage =
  "Usage: " ^ Sys.argv.(0) ^
  " [<options>] [<file> ...]\n\
   \n\
   Generic options:\n \
   -h, --help        Show this help message\n \
   -v, --verbose     Verbose mode: show compression rate\n \
   -o <file>\n \
   --output=<file>   Output file (defaults to stdout)\n \
   <file> ...        Input files (defaults to stdin or \"-\")\n\
   \n\
   Optimization flags (default is -w -c -s -d):\n \
   -w, --whitespace  Eliminate unnecessary whitespaces (has the greatest \
                     effect, omit for pretty-printing)\n \
   -c, --simple      Shorten colors, font weights and nth-child\n \
   -s, --shorthands  Generate shorthand properties\n \
   -d, --duplicates  Prune duplicate properties (WARNING: may affect \
                     cross-browser hacks)\n \
   -p, --pretty      Shorthand for -c -s -d\n \
   \n\
   Formatting options:\n \
   -r, --sort        Sort declarations in each ruleset (always on when \
                     --shorthands is enabled)\n \
   -e, --echo        Just parse and pretty-print, no optimizations\n\
   "

let parse_args () =
  let infiles    = ref [] in
  let outfile    = ref None in
  let verbose    = ref false in
  let whitespace = ref false in
  let simple     = ref false in
  let shorthands = ref false in
  let duplicates = ref false in
  let sort       = ref false in
  let echo       = ref false in

  let show_usage () = prerr_string usage; raise Exit_success in
  let set_pretty () = simple := true; shorthands := true; duplicates := true in
  let add_infile  = function
    | "-"      -> infiles := []
    | filename -> infiles := filename :: !infiles
  in

  let specs = [
    ('h', "help",       Some show_usage, None);
    ('v', "verbose",    Getopt.set verbose true, None);
    ('o', "output",     None, Some (fun file -> outfile := Some file));
    ('w', "whitespace", Getopt.set whitespace true, None);
    ('c', "simple",     Getopt.set simple true, None);
    ('s', "shorthands", Getopt.set shorthands true, None);
    ('d', "duplicates", Getopt.set duplicates true, None);
    ('p', "pretty",     Some set_pretty, None);
    ('r', "sort",       Getopt.set sort true, None);
    ('e', "echo",       Getopt.set echo true, None);
  ] in

  Getopt.parse_cmdline specs add_infile;

  match {
    infiles    = List.rev !infiles;
    outfile    = !outfile;
    verbose    = !verbose;
    whitespace = !whitespace;
    simple     = !simple;
    shorthands = !shorthands;
    duplicates = !duplicates;
    sort       = !sort;
  } with

  (* disable optimizations when --echo is specified *)
  | args when !echo = true ->
    { args with
      whitespace = false;
      simple     = false;
      shorthands = false;
      duplicates = false }

  (* enable all optimizations by default *)
  | { whitespace = false;
      simple     = false;
      shorthands = false;
      duplicates = false;
      _ } as args ->
    { args with
      whitespace = true;
      simple     = true;
      shorthands = true;
      duplicates = true }

  | args -> args

let parse_files = function
  | [] ->
    let input = Util.input_buffered stdin 512 in
    (input, Parse.parse_input "<stdin>" input)
  | files ->
    let rec loop = function
      | [] -> []
      | filename :: tl ->
        if not (Sys.file_exists filename) then
          raise (Failure ("file " ^ filename ^ " does not exist"));
        let input = Util.input_all (open_in filename) in
        let stylesheet = Parse.parse_input filename input in
        (input, stylesheet) :: loop tl
    in
    let inputs, stylesheets = List.split (loop files) in
    (String.concat "" inputs, List.concat stylesheets)

let handle_args args =
  let write_output =
    match args.outfile with
    | None -> print_endline
    | Some name ->
      fun css -> let f = open_out name in output_string f css; close_out f
  in

  let switch flag fn = if flag then fn else fun x -> x in

  let input, css = parse_files args.infiles in
  let css = css
    (* unfold before pruning duplicates so that shorthand components are
     * correctly pruned *)
    |> switch args.shorthands Shorthand.unfold_stylesheet
    |> switch args.duplicates Duplicates.compress
    |> switch args.shorthands Shorthand.compress
    |> switch args.simple Simple.compress
    |> switch args.sort Util.sort_stylesheet
  in
  let output =
    if args.whitespace
      then Stringify.minify_stylesheet css
      else Stringify.string_of_stylesheet css
  in
  write_output output;

  if args.verbose then begin
    let il = String.length input in
    let ol = String.length output in
    Printf.fprintf stderr "compression: %d -> %d bytes (%d%% of original)\n"
    il ol (int_of_float (float_of_int ol /. float_of_int il *. 100.))
  end

(* Main function, returns exit status *)
let main () =
  begin
    try
      handle_args (parse_args ());
      exit 0
    with
    | Loc_error (loc, msg) ->
      Util.prerr_loc_msg loc ("Error: " ^ msg);
    | Box_error (box, msg) ->
      prerr_endline ("Error: " ^ msg ^ ": " ^ Stringify.string_of_box box);
    | Getopt.Error msg ->
      prerr_endline ("Error: " ^ msg ^ "\n");
      prerr_string usage;
    | Failure msg ->
      prerr_endline ("Error: " ^ msg);
    | Exit_success ->
      exit 0
  end;
  exit 1

let _ = main ()
