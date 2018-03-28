open GT

type 'a maybe = Just of 'a | Nothing [@@deriving gt ~gmap ~show ]

module P = struct
  type t = (int -> string) maybe [@@deriving gt
    ~gmapA:{ _1 = (fun _inh x -> x)}
    ~showA:{ _1 = (fun _inh _ -> "<fun>") }
]
end



(* open P
 * type nonrec ('a,'b) p = ('a,'b) p maybe [@@deriving gt ~gmap ~show ]
 *
 * (\* There is an issue with nonrec that when we will define a class
 *    we will not be able to see previous type `p`.
 * *\)
 * module Test2 = struct
 *   open P
 *   type ('a,'b) p = ('a,'b) p maybe [@@deriving gt ~gmap ~show ]
 * end *)
