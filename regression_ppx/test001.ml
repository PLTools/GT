type t = int [@@deriving gt]
type 'a list = Nil | Cons of 'a * 'a list [@@deriving gt { gmap; show } ]
