@type 'a tree = Leaf | Node of 'a * 'a tree GT.list with show, gmap

let _ =
  let x = Node (1, [ Node (2, [Leaf])
                   ; Node (3, [Leaf])
                   ; Node (4, [Node (5, []); Leaf])
                   ]
               ) in
  let y = GT.gmap tree (GT.lift string_of_int) () x in
  Printf.printf "%s\n" @@ GT.show tree (GT.lift string_of_int) x;
  Printf.printf "%s\n" @@ GT.show tree (GT.lift id)            y
