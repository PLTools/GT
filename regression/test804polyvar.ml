open GT

type 'a maybe = Just of 'a | Nothing
[@@deriving gt ~options:{show; fmt }]

type 'a pv = [ `A of 'a ]
[@@deriving gt ~options:{show; fmt}]


let () =
  let sh x = GT.show pv GT.id x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")


include (struct
  type 'a wtf = [ `C of 'a | 'a pv ] maybe
  [@@deriving gt ~options:{show; fmt}]
end : sig
   type 'a wtf = [ `C of 'a | 'a pv ] maybe
   [@@deriving gt ~options:{show; fmt}]
 end)

let () =
  let sh x = GT.show wtf GT.id x in
  Printf.printf "%s\t%s\n%!" (sh @@ Just(`A "a")) (sh @@ Just (`C "ccc"))
