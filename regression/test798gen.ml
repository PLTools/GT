type ('a,'b) glist = Nil | Cons of 'a * 'b
[@@deriving gt {show}]

(* type 'a list = ('a, 'a list) glist
[@@deriving gt {show}] *)
