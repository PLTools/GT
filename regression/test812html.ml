type t = { a : int; b : string }
[@@deriving gt ~options:{show; html}]

(* type t2 = A of int | B of string | C of float * int | D of t
 * [@@deriving gt ~options:{show; html}] *)

let () =
  let t  = {a=5; b="beeeee"} in
  (* let t2 = A 5655 in *)

  Tyxml.Html.pp_elt () Format.std_formatter (html_t t)
