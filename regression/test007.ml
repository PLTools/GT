@type ('a, 'b) t = A of 'a GT.list * 'b GT.list with show, map

let _ =
  let x = A ([1; 2; 3], ["4"; "5"; "6"]) in
  let y = GT.transform(t) (fun _ x -> string_of_int x) (fun _ x -> int_of_string x) new @map[t] () x in
  Printf.printf "%s\n" (GT.transform(t) (fun _ x -> string_of_int x) (fun _ x -> x) new @show[t] () x);
  Printf.printf "%s\n" (GT.transform(t) (fun _ x -> x) (fun _ x -> string_of_int x) new @show[t] () y);
