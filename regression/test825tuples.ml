type ('a, 'b) p = GT.int * (GT.string * 'a)
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

type ('a, 'b, 'c) triple = 'a * 'b * 'c
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

type ('a, 'b, 'c) quattre = 'a * 'b * 'c * 'c
  [@@deriving gt ~options:{show; gmap; compare; foldl }]

include (struct
type t = GT.int * GT.string
  [@@deriving gt ~options:{show; gmap; compare; eq; eval; foldl; html; stateful }]
end : sig
type t = GT.int * GT.string
  [@@deriving gt ~plugins:{show; gmap; compare; eq; eval; foldl; html; stateful }]
end)

let () =
  Format.printf "%s\n\n%!" @@ (GT.show p (GT.show GT.int) (GT.show GT.int)) (1, ("", 42))

let () =
  print_endline @@
  (GT.foldl triple (GT.foldl GT.string)(GT.foldl GT.string)(GT.foldl GT.string))
  ""
   ("a", "b", "c")

let () =
  (GT.gmap quattre (GT.gmap GT.int) (GT.gmap GT.int) (GT.gmap GT.int))
    (1,2,3,4)
  |> GT.show quattre (GT.show GT.int)(GT.show GT.int)(GT.show GT.int)
  |> print_endline

let () =
  assert (GT.compare t (1,"") (1,"") = GT.EQ)