module M: sig
  type foo
  [@@deriving gt ~options: { gmap } ]

  type t = T of foo
  [@@deriving gt ~options: { gmap } ]

  val make_t : int -> t
end = struct
  type foo = Foo of GT.string * GT.int
  [@@deriving gt ~options: { gmap } ]

  type t = T of foo
  [@@deriving gt ~options: { gmap } ]

  let make_t n = T (Foo (string_of_int n, n))
end

let () =
  let _ = [%gmap: M.t] () (M.make_t 42) in
  ()