@type 'a tree = Node of 'a * 'a tree GT.list deriving show, fold

let _ =
  let x = Node (1, [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, [])])]) in
  let n = GT.transform(tree) (fun s x -> s + x) new @fold[tree] 0 x in
  Printf.printf "%s\n" (GT.transform(tree) (fun _ x -> string_of_int x) new @show[tree] () x);
  Printf.printf "%d\n" n
