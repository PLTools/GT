(* The same as test 086 but in PPX syntax *)

module T : sig
  type t2 = GT.int * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t3 = GT.int * 'a * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t4 = GT.bytes [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a t1 = 'a [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type bindings = (GT.string * GT.int) GT.list [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u1 = 'a GT.option [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]
  type 'a u2 = 'a GT.Lazy.t [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type 'a u3 = {aa: GT.int; bb:GT.string} [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

  type 'a r1 = 'a GT.ref [@@deriving gt ~options:{fmt; html}]

  type ('a,'b) arr1 = ('a * 'b) GT.array [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]
end = struct
  type nonrec t2 = GT.int * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec 'a t3 = GT.int * 'a * GT.string [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec 'a t4 = GT.bytes [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec 'a t1 = 'a [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec bindings = (GT.string * GT.int) GT.list [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec 'a u1 = 'a GT.option [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]
  type nonrec 'a u2 = 'a GT.Lazy.t [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; stateful; html}]

  type nonrec 'a u3 = {aa: GT.int; bb:GT.string} [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

  type nonrec 'a r1 = 'a GT.ref [@@deriving gt ~options:{fmt; html}]

  type nonrec ('a,'b) arr1 = ('a * 'b) GT.array [@@deriving gt ~options:{show; gmap; foldl; eq; compare; eval; html}]

end

let _ = GT.show T.t1 (GT.show GT.int) 5
let _ = GT.show T.t2 (5,"")
let _ = GT.show T.t3 (GT.show GT.int) (5, 2, "")
let _ = GT.show T.t4 (GT.show GT.int) (Bytes.of_string "")
let _ = GT.show T.bindings []
let _ = GT.show T.u1 (GT.show GT.int) (Some 1)
let __crashes () = GT.show T.u2 (GT.show GT.int) (lazy 1)
let _ = GT.show T.u3 (GT.show GT.int) { T.aa = 452; bb = "" }
let _ = Format.asprintf "%a" (GT.fmt T.r1 (GT.fmt GT.int)) (ref 5)
let _ = GT.show T.arr1 (GT.show GT.int) (GT.show GT.int) [| 1,1 |]
