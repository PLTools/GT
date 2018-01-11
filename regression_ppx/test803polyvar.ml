open GT

type ('a, 'b) pv = [ `A of 'a | `B of 'b ] [@@deriving gt ~show ~gmap]

