open Types

(* This file defines functions that construct an element tree with applied
 * stylesheet based on:
 * - http://www.w3.org/TR/CSS2/cascade.html#specificity
 *)

let is_pseudo_class = function
  | Pseudo (_, ("link" | "hover" | "visited" | "active"), None) -> true
  | _ -> false

(* Specificity (a, b, c, d):
 * a = 0 (1 if in style="" definition which is always false in a stylesheet)
 * b = number of ID attributes
 * c = number of other (pseudo-)attributes
 * d = number of (pseudo-)elements
 *)
let rec specificity =
  let add (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h) in
  function
  | No_element | All_elements ->
    (0, 0, 0, 0)
  | Element _ ->
    (0, 0, 0, 1)
  | Id (base, _) ->
    add (0, 1, 0, 0) (specificity base)
  | Class (base, _) | Attribute (base, _, _) ->
    add (0, 0, 1, 0) (specificity base)
  | Pseudo (base, _, _) as addon when is_pseudo_class addon ->
    add (0, 0, 1, 0) (specificity base)
  | Pseudo (base, _, _) ->
    add (0, 0, 0, 1) (specificity base)
  | Combinator (left, _, right) ->
    add (specificity left) (specificity right)

let precedes (a, b, c, d) (e, f, g, h) =
  let rec loop = function
    | []                -> true
    | 0 :: tl           -> loop tl
    | n :: _ when n > 0 -> true
    | _                 -> false
  in
  loop [a - e; b - f; c - g; d - h]

let overwrites selector1 selector2 =
  precedes (specificity selector1) (specificity selector2)

let can_match_same selector1 selector2 =
  let unfold =
    let rec loop classes ids pseudos attrs = function
      | No_element ->
        ("", classes, ids, pseudos, attrs)
      | All_elements ->
        ("*", classes, ids, pseudos, attrs)
      | Element elem ->
        (elem, classes, ids, pseudos, attrs)
      | Id (base, id) ->
        loop classes (id :: ids) pseudos attrs base
      | Class (base, cls) ->
        loop (cls :: classes) ids pseudos attrs base
      | Pseudo (base, f, arg) ->
        (* XXX: what about :not(...) ? *)
        loop classes ids ((f, arg) :: pseudos) attrs base
      | Attribute (base, attr, _) ->
        loop classes ids pseudos (attr :: attrs) base
      | Combinator (_, _, right) ->
        loop classes ids pseudos attrs right
    in
    loop [] [] [] []
  in
  let rec intersect l = function
    | [] -> false
    | hd :: _ when List.mem hd l -> true
    | _ :: tl -> intersect l tl
  in
  let elem1, classes1, ids1, pseudos1, attrs1 = unfold selector1 in
  let elem2, classes2, ids2, pseudos2, attrs2 = unfold selector2 in
  elem1 = "*" || elem2 = "*" || elem1 = elem2 && elem1 <> "" ||
  intersect classes1 classes2 ||
  intersect ids1 ids2 ||
  intersect pseudos1 pseudos2 ||
  intersect attrs1 attrs2
