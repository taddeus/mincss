open Lexing
open Types

type args = {
  mutable infiles : string list;
  mutable outfile : string option;
  mutable verbose : int;
  mutable echo    : bool;
  mutable pretty  : bool;
  mutable unfold  : bool;
}

(* Parse command-line arguments *)
let parse_args () =
  let args = {
    infiles = [];
    outfile = None;
    verbose = 1;
    echo = false;
    pretty = false;
    unfold = false;
  } in
  let args_spec = [
    ("<file> ...", Arg.Rest (fun _ -> ()),
                 " Optional input files (default is to read from stdin)");

    ("-o", Arg.String (fun s -> args.outfile <- Some s),
         "<file>   Output file (defaults to stdout)");

    ("-v", Arg.Int (fun i -> args.verbose <- i),
         "<num>    Set verbosity (0: nothing, 1: errors (default), \
          2: compression rate, 3: debug)");

    ("--echo", Arg.Unit (fun _ -> args.echo <- true),
             "     Don't minify, just pretty-print the parsed CSS");

    ("--pretty", Arg.Unit (fun _ -> args.pretty <- true),
               "   Minify, but pretty-print the parsed CSS (for debugging)");

    ("--unfold", Arg.Unit (fun _ -> args.unfold <- true),
               "   Only unfold shorthands (for debugging)");
  ] in

  let usage =
    "Usage: " ^ Sys.argv.(0) ^ " [-o <file>] [-v <verbosity>] [<file> ...] " ^
    "[--pretty | --echo]"
  in

  Arg.parse args_spec (fun f -> args.infiles <- args.infiles @ [f]) usage;
  args

let parse_files = function
  | [] ->
    let input = Util.input_buffered stdin 512 in
    (input, Parse.parse_input "<stdin>" input)
  | files ->
    let rec loop = function
      | [] -> []
      | filename :: tl ->
        let input = Util.input_all (open_in filename) in
        let stylesheet = Parse.parse_input filename input in
        (input, stylesheet) :: loop tl
    in
    let inputs, stylesheets = List.split (loop files) in
    (String.concat "" inputs, List.concat stylesheets)

let handle_args args =
  let input, stylesheet = parse_files args.infiles in

  let write_output =
    match args.outfile with
    | None -> print_endline
    | Some name ->
      fun css -> let f = open_out name in output_string f css; close_out f
  in

  match args with
  | {echo = true} ->
    write_output (Stringify.string_of_stylesheet stylesheet)
  | {unfold = true} ->
    let stylesheet = Shorthand.unfold_stylesheet stylesheet in
    write_output (Stringify.string_of_stylesheet stylesheet)
  | _ ->
    let stylesheet = Color.compress stylesheet in
    let stylesheet = Shorthand.compress stylesheet in

    let stringify =
      if args.pretty
        then Stringify.string_of_stylesheet
        else Stringify.minify_stylesheet
    in
    let output = stringify stylesheet in

    write_output output;

    if args.verbose >= 2 then begin
      let il = String.length input in
      let ol = String.length output in
      Printf.fprintf stderr "compression: %d -> %d bytes (%d%% of original)\n"
      il ol (int_of_float (float_of_int ol /. float_of_int il *. 100.))
    end

(* Main function, returns exit status *)
let main () =
  let args = parse_args () in
  begin
    try
      handle_args args;
      exit 0
    with
    | Loc_error (loc, msg) ->
      Util.prerr_loc_msg (args.verbose >= 1) loc ("Error: " ^ msg);
    | Box_error (box, msg) ->
      prerr_endline ("Error: " ^ msg ^ ": " ^ Stringify.string_of_box box);
    | Failure msg ->
      prerr_endline ("Error: " ^ msg);
  end;
  exit 1

let _ = main ()
