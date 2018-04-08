open GT

let show_tuple2 f g (a,b) = Printf.sprintf "(%s,%s)" (f a) (g b)

type ('a, 'b) a = B of ('a, 'b) b | D of 'a
and  ('a, 'b) b = A of ('a, 'b) a | C of 'a * 'b | E
[@@deriving gt ~gmap ~show]

(*
type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
   doesn't work because of non-regualrity
*)

