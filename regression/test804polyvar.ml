open GT

type 'a maybe = Just of 'a | Nothing
[@@deriving gt ~options:{show; fmt }]

type 'a pv = [ `A of 'a ]
[@@deriving gt ~options:{show; fmt}]


let () =
  let sh x = show_pv (fun () x -> x) x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")


include (struct
  type 'a wtf = [ `C of 'a | 'a pv ] maybe
  [@@deriving gt ~options:{show; fmt}]
end : sig
   type 'a wtf = [ `C of 'a | 'a pv ] maybe
   [@@deriving gt ~options:{show; fmt}]
 end)

let () =
  let sh x = show_wtf (fun () x -> x) x in
  Printf.printf "%s\t%s\n%!" (sh @@ Just(`A "a")) (sh @@ Just (`C "ccc"))
