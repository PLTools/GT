type 'a logic =
  | Var
  | Value of 'a
[@@deriving gt ~options:{ gmap }]
(*
type nonrec ('a, 'a0) targ_fuly = T of 'a0 * 'a [@@deriving gt ~options:{ gmap }]

type nonrec ('a, 'a1, 'a0) jtyp_fuly =
  | Array of 'a1
  | V of 'a0
[@@deriving gt ~options:{ gmap }]

type 'a targ_logic = ('a, 'a jtyp_logic) targ_fuly logic

and 'a jtyp_logic = ('a, 'a jtyp_logic, 'a targ_logic) jtyp_fuly logic
[@@deriving gt ~options:{ gmap }]

let (_ : ('a -> 'b) -> 'a targ_logic -> 'b targ_logic) = fun eta -> GT.gmap targ_logic eta

let __ : 'a 'b. ('a -> 'b) -> 'a jtyp_logic -> 'b jtyp_logic =
 fun eta -> GT.gmap jtyp_logic eta
;; *)

type nonrec ('a, 'a0) targ_fuly = T of 'a0 * 'a [@@deriving gt ~options:{ gmap }]

type nonrec ('a, 'a1, 'a0) jtyp_fuly =
  | Array of 'a1
  | V of 'a0
[@@deriving gt ~options:{ gmap }]

type 'a targ_logic = ('a, 'a jtyp_logic) targ_fuly logic

and 'a jtyp_logic = ('a, 'a jtyp_logic, 'a targ_logic) jtyp_fuly logic
[@@deriving gt ~options:{ gmap }]

let __ (type a b) : (a -> b) -> a jtyp_logic -> b jtyp_logic =
 fun eta -> GT.gmap jtyp_logic eta
;;
