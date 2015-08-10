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
  echo       : bool;
  sort       : bool;
}

let parse_args () =
  let usage =
    "Usage: " ^ Sys.argv.(0) ^
    " [<options>] [<file> ...]\n\
     \n\
     Generic options:\n \
     -h, --help        Show this help message\n \
     -v, --verbose     Verbose mode: show compression rate\n \
     -o <file>         Output file (defaults to stdout)\n \
     <file> ...        Input files (default is to read from stdin)\n\
     \n\
     Optimization flags (if none are specified, all are enabled):\n \
     -w, --whitespace  Eliminate unnecessary whitespaces (has the greatest \
                       effect, omit for pretty-printing)\n \
     -c, --simple      Shorten colors, font weights and nth-child\n \
     -s, --shorthands  Generate shorthand properties\n \
     -d, --duplicates  Prune duplicate properties (WARNING: may affect \
                       cross-browser hacks)\n \
     -p, --pretty      Shorthand for -c -s -d\n \
     -e, --echo        Just parse and pretty-print, no optimizations\n\
     \n\
     Formatting options:\n \
     --sort            Sort declarations in each selector group\n\
     "
  in

  let default_args = {
    infiles    = [];
    outfile    = None;
    verbose    = false;
    whitespace = false;
    simple     = false;
    shorthands = false;
    duplicates = false;
    echo       = false;
    sort       = false;
  } in

  let rec handle args = function
    | ("-v" | "--verbose") :: tl ->
      handle {args with verbose = true} tl
    | ("-w" | "--whitespace") :: tl ->
      handle {args with whitespace = true} tl
    | ("-c" | "--simple") :: tl ->
      handle {args with simple = true} tl
    | ("-s" | "--shorthands") :: tl ->
      handle {args with shorthands = true} tl
    | ("-d" | "-duplicates") :: tl ->
      handle {args with duplicates = true} tl
    | ("-p" | "--pretty") :: tl ->
      handle {args with simple = true; shorthands = true; duplicates = true} tl
    | ("-e" | "--echo") :: tl ->
      handle {args with echo = true} tl
    | "--sort" :: tl ->
      handle {args with sort = true} tl

    | ("-h" | "--help") :: tl ->
      prerr_string usage;
      raise Exit_success

    | ["-o"] ->
      raise (Failure ("missing output file name"))
    | "-o" :: next :: tl when next.[0] = '-' ->
      raise (Failure ("missing output file name"))
    | "-o" :: filename :: tl ->
      handle {args with outfile = Some filename} tl

    | arg :: tl when arg.[0] = '-' ->
      prerr_string usage;
      raise (Failure ("unknown option " ^ arg))

    | filename :: tl ->
      handle {args with infiles = args.infiles @ [filename]} tl

    | [] -> args
  in

  match handle default_args (List.tl (Array.to_list Sys.argv)) with
  | { echo = true; _ } as args ->
    { args with
      whitespace = false;
      simple     = false;
      shorthands = false;
      duplicates = false }

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
    |> switch args.simple Simple.compress
    |> switch args.duplicates Duplicates.compress
    |> switch args.shorthands Shorthand.compress
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
    | Failure msg ->
      prerr_endline ("Error: " ^ msg);
    | Exit_success ->
      exit 0
  end;
  exit 1

let _ = main ()
