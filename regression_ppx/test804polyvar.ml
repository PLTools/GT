open GT

type 'a maybe = Just of 'a | Nothing
[@@deriving gt ~show ~gmap]

type 'a pv = [ `A of 'a ]
[@@deriving gt ~show ~gmap]



let () =
  let sh x = show_pv (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")


include (struct
  type 'a wtf = [ `C of 'a | 'a pv ] maybe
  [@@deriving gt ~show ~gmap]
end : sig
  type 'a wtf = [ `C of 'a | 'a pv ] maybe
  [@@deriving gt ~show ~gmap]

end)

let () =
  let sh x = show_wtf (fun () x -> x) () x in
  Printf.printf "%s\t%s\n%!" (sh @@ Just(`A "a")) (sh @@ Just (`C "ccc"))
