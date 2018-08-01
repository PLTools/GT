open GT;;

@type ('a, 'b) a = B of ('a, 'b) b | D of 'a
and ('a, 'b) c = ('a, 'b) b
and ('a, 'b) b = A of ('a, 'b) a | C of 'a * 'b | E
with show,gmap

(*
type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
   doesn't work because of non-regualrity
*)

