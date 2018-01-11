open GT

type t = [ `A | `B ] list
[@@deriving gt ~gmap ~show ]

(* type ('a,'b) demo = 'a * 'b *)
(* type nonrec ('a,'b) demo = ('a,'b) demo list *)
