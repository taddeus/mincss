open Types

let hex6 = Str.regexp "\\([0-9a-f]\\)\\1\\([0-9a-f]\\)\\2\\([0-9a-f]\\)\\3"

let is_num = function
  | Number (n, (None | Some "%")) -> true
  | _ -> false

let clip = function
  | Number (n, u) when n < 0.          -> Number (0., u)
  | Number (n, None) when n > 255.     -> Number (255., None)
  | Number (n, Some "%") when n > 100. -> Number (100., Some "%")
  | value -> value

let rec short = function
  (* #aabbcc -> #abc *)
  | Hexcolor h when Str.string_match hex6 h 0 ->
    let gr n = Str.matched_group n h in
    Hexcolor (gr 1 ^ gr 2 ^ gr 3)

  (* rgb(r,g,b) -> #rrggbb *)
  | Function ("rgb", Nary (",", [r; g; b]))
    when is_num r && is_num g && is_num b ->
    let i c =
      match clip c with
      | Number (n, None) -> int_of_float n
      | Number (n, Some "%") -> int_of_float (n *. 2.55 +. 0.5)
      | _ -> assert false
    in
    short (Hexcolor (Printf.sprintf "%02x%02x%02x" (i r) (i g) (i b)))

  (* clip rgb values, e.g. rgb(-1,256,0) -> rgb(0,255,0) *)
  | Function ("rgb", Nary (",", [r; g; b])) ->
    Function ("rgb", Nary (",", [clip r; clip g; clip b]))

  (* rgba(r,g,b,1.0) -> rgb(r,g,b) *)
  | Function ("rgba", Nary (",", [r; g; b; Number (1., None)])) ->
    short (Function ("rgb", Nary (",", [r; g; b])))

  (* TODO: hsl[a](...) *)

  (* transform color names to shorter hex codes and vice-versa *)
  | v -> Color_names.compress v

let transform = function
  | Expr value -> Expr (short value)
  | v -> v

let compress = Util.transform_stylesheet transform
