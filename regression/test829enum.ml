type t =  A | B | C of int [@@deriving gt ~plugins:{enum}]

let () = Format.printf "%d %d %d\n%!" (GT.enum t A) (GT.enum t B) (GT.enum t (C 1))


type u = [ `A | `B | `C of int ] [@@deriving gt ~options:{enum}]
let () = Format.printf "%d %d %d\n%!" (GT.enum u `A) (GT.enum u `B) (GT.enum u (`C 1))



type arr = GT.int GT.array [@@deriving gt ~options:{enum}]

type list = (GT.int -> GT.int) GT.list [@@deriving gt ~options:{enum}]

type rec1 = { rec1_f1: int } [@@deriving gt ~options:{enum}]
let __ { rec1_f1 }= { rec1_f1 = 23 + rec1_f1 }

[@@@ocaml.warning "-27"]

type 'a alg_constr = Constr1 of { count: 'a } [@@deriving gt ~options:{enum}]

let __ =  Constr1 { count = 23. }