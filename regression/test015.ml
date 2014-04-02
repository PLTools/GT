@type 'a tree = Node of 'a * 'a tree GT.list with show, foldl, foldr

let _ =
  let x  = Node (1, [Node (2, [Node (5, [])]); Node (3, []); Node (4, [Node (6, [])])]) in
  let n  = GT.transform(tree) (fun s x -> s + x) new @foldl[tree] 0 x in
  let sl = GT.transform(tree) (fun s x -> if s = "" then string_of_int x else s ^ ", " ^ string_of_int x) new @foldl[tree] "" x in
  let sr = GT.transform(tree) (fun s x -> if s = "" then string_of_int x else s ^ ", " ^ string_of_int x) new @foldr[tree] "" x in
  Printf.printf "%s\n" (GT.transform(tree) (fun _ x -> string_of_int x) new @show[tree] () x);
  Printf.printf "%d\n" n;
  Printf.printf "%s\n" sl;
  Printf.printf "%s\n" sr

