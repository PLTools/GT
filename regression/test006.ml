@type 'a tree = Leaf | Node of 'a * 'a tree GT.list with show, gmap

let _ =
  let x = Node (1, [ Node (2, [Leaf])
                   ; Node (3, [Leaf])
                   ; Node (4, [Node (5, []); Leaf])
                   ]
               ) in
  let y = gmap_tree (fun x -> string_of_int x) x in
  Printf.printf "%s\n" @@ show_tree (fun x -> string_of_int x) x;
  Printf.printf "%s\n" @@ show_tree (fun x -> x)               y
