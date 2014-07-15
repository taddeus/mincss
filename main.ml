open Lexing
open Types

(* Parse command-line arguments *)
let parse_args () =
  let args = {
    infiles = [];
    outfile = None;
    verbose = 1;
  } in
  let args_spec = [
    ("<file> ...", Arg.Rest (fun _ -> ()),
                 " Optional input files (default is to read from stdin)");

    ("-o", Arg.String (fun s -> args.outfile <- Some s),
         "<file>   Output file (defaults to stdout)");

    ("-v", Arg.Int (fun i -> args.verbose <- i),
         "<num>    Set verbosity (0: nothing, 1: errors (default), \
          2: compression rate, 3: debug)");
  ] in

  let usage =
    "Usage: " ^ Sys.argv.(0) ^ " [-o <file>] [-v <verbosity>] [<file> ...]"
  in

  Arg.parse args_spec (fun f -> args.infiles <- args.infiles @ [f]) usage;
  args

(* Main function, returns exit status
 * Command-line arguments are stored in lobals.args *)
let main () =
  let args = parse_args () in
  try
    let css =
      match args.infiles with
      | [] ->
        let input = Util.input_buffered stdin 512 in
        Parse.parse_input "<stdin>" input
      | files ->
        let rec loop = function
          | [] -> []
          | filename :: tl ->
            let input = Util.input_all (open_in filename) in
            let css = Parse.parse_input filename input in
            css @ loop tl
        in
        loop files
    in
    Util.print_css css;
    exit 0
  with
  | LocError (loc, msg) ->
    Util.prerr_loc_msg args loc ("Error: " ^ msg);
  | Failure err ->
    prerr_endline ("Error: " ^ err);
  exit 1

let _ = main ()
