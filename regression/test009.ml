@type ('a, 'b) a = A of 'a | B of 'b with show, gmap
@type ('a, 'b) t = X of ('a, 'b) a * ('a, 'b) t GT.list | 
                   Y of ('b, 'a) a * ('a, 'b) t GT.list with show, gmap

let _ =
  let x = X (A 1, [Y (A "2", []); X (A 2, []); Y (A "3", [])]) in
  let y = GT.transform(t) (fun _ x -> string_of_int x) (fun _ x -> int_of_string x) new @t[gmap] () x in
  Printf.printf "%s\n" (GT.transform(t) (fun _ x -> string_of_int x) (fun _ x -> x) new @t[show] () x);
  Printf.printf "%s\n" (GT.transform(t) (fun _ x -> x) (fun _ x -> string_of_int x) new @t[show] () y)
