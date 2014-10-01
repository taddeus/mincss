open Types

let rec shorten = function
  | Ident "normal" -> Number (400.0, None)
  | Ident "bold"   -> Number (700.0, None)
  | v -> v

let transform = function
  | Declaration ("font-weight", value, imp) ->
    Declaration ("font-weight", shorten value, imp)
  | v -> v

let compress = Util.transform_stylesheet transform
