open GT

type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
[@@deriving gt ~show ~gmap]

type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
[@@deriving gt ~show ~gmap]

let () =
  let sh x = show_pv_ext (fun () -> string_of_int) (fun () x -> x) () x in
  Printf.printf "%s\t%s\t%s\n%!" (sh @@ `A 5) (sh @@ `B "xxx") (sh @@ `C 32)
