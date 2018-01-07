
type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
                     [@@deriving gt ~gmap ~show ]



(* type ('a,'b) demo = 'a * 'b *)
(* type nonrec ('a,'b) demo = ('a,'b) demo list *)
