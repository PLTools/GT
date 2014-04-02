@type 'a tree = Leaf | Node of 'a * 'a tree GT.list with show, map

let _ =
  let x = Node (1, [Node (2, [Leaf]); Node (3, [Leaf]); Node (4, [Node (5, []); Leaf])]) in
  let y = GT.transform(tree) (fun _ x -> string_of_int x) new @map[tree] () x in
  Printf.printf "%s\n" (GT.transform(tree) (fun _ x -> string_of_int x) new @show[tree] () x);
  Printf.printf "%s\n" (GT.transform(tree) (fun _ x -> x) new @show[tree] () y) 
  
