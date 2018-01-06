(* type t = int [@@deriving gt] *)

type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt ~gmap ~show ]

type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ]

type 'a llist = ('a logic, 'b) alist logic as 'b [@@deriving gt ~gmap ~show ]
