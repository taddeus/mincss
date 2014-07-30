open Lexing
open Types

type args = {
  mutable infiles : string list;
  mutable outfile : string option;
  mutable verbose : bool;
  mutable prune   : bool;
  mutable unfold  : bool;
  mutable upto    : int option;
}

(* Parse command-line arguments *)
let parse_args () =
  let args = {
    infiles = [];
    outfile = None;
    verbose = false;
    prune = true;
    unfold = true;
    upto = None;
  } in

  let args_spec = [
    ("<file> ...", Arg.Rest (fun _ -> ()),
                 "   Input files (default is to read from stdin)");

    ("-o", Arg.String (fun s -> args.outfile <- Some s),
         "<file>     Output file (defaults to stdout)");

    ("-v", Arg.Unit (fun _ -> args.verbose <- true),
         "           Verbose mode: show compression rate");

    ("-no-prune", Arg.Unit (fun _ -> args.prune <- false),
                "    Don't prune duplicate properties (skip step 5 below)");

    ("-no-unfold", Arg.Unit (fun _ -> args.unfold <- false),
                 "   Only minify whitespace, colors and shorthands \
                     (skip steps 2-7 below)");

    ("-upto", Arg.Int (fun i -> args.upto <- Some i),
            "<step>  Stop after the specified step (for debugging): \
                                              \n                \
                     1: parse                 \n                \
                     2: unfold shorthands     \n                \
                     3: unfold selectors      \n                \
                     4: unfold blocks         \n                \
                     5: prune duplicates      \n                \
                     6: combine selectors     \n                \
                     7: concatenate blocks    \n                \
                     8: optimize blocks       \n                \
                     9: minify");
  ] in

  let usage =
    "Usage: " ^ Sys.argv.(0) ^ " [-o <file>] [-v] [-no-prune] [-upto <step>] \
                                 [<file> ...] "
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
  let steps =
    (*let switch flag fn = if flag then fn else fun s -> s in*)
    [
      (*
      switch args.unfold Unfold.unfold_shorthands;
      switch args.unfold Unfold.unfold_selectors;
      switch args.unfold Unfold.unfold_blocks;
      switch (args.unfold && args.prune) Unfold.prune_duplicates;
      switch args.unfold Combine.combine_selectors;
      switch args.unfold Concat.concat_blocks;
      Optimize.optimize_blocks;
      *)
    ]
  in

  let write_output =
    match args.outfile with
    | None -> print_endline
    | Some name ->
      fun css -> let f = open_out name in output_string f css; close_out f
  in

  let upto = match args.upto with Some i -> i | None -> 0 in

  let input, stylesheet = parse_files args.infiles in

  let rec do_steps i stylesheet = function
    | _ when i = upto ->
      write_output (Stringify.string_of_stylesheet stylesheet)

    | [] ->
      let output = Stringify.minify_stylesheet stylesheet in
      write_output output;

      if args.verbose then begin
        let il = String.length input in
        let ol = String.length output in
        Printf.fprintf stderr "compression: %d -> %d bytes (%d%% of original)\n"
        il ol (int_of_float (float_of_int ol /. float_of_int il *. 100.))
      end

    | step :: tl ->
      do_steps (i + 1) (step stylesheet) tl
  in

  do_steps 1 stylesheet steps


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
  end;
  exit 1

let _ = main ()
