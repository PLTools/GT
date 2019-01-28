type 'a t = C of 'a
[@@deriving gt ~options:{ show }]


let () =
  print_endline @@ GT.show(t) (GT.show GT.int) (C 1)
