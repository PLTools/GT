@type 'a a = [`A of 'a | `B of GT.string] with show, eq, compare
@type 'a b = [`C of 'a | `D of GT.string] with show, eq, compare
@type ('a, 'b) c = ['a a | 'b b] with show, eq, compare

let _ =
  let x = `A 3 in
  let y = `C 2 in
  Printf.printf "%s\n" (GT.transform(a) (fun _ x -> string_of_int x) (new @show[a]) () x);
  Printf.printf "%s\n" (GT.transform(b) (fun _ x -> string_of_int x) (new @show[b]) () y);
  Printf.printf "%s\n" (GT.transform(c) (fun _ x -> string_of_int x) (fun _ x -> string_of_int x) (new @show[c]) () x);
  Printf.printf "%s\n" (GT.transform(c) (fun _ x -> string_of_int x) (fun _ x -> string_of_int x) (new @show[c]) () y);
  Printf.printf "%b\n" (GT.transform(a) (rewrap_a (fun x y -> x = y)) (new @eq[a]) (`t x) x);
  Printf.printf "%b\n" (GT.transform(b) (rewrap_b (fun x y -> x = y)) (new @eq[b]) (`t y) y);
  Printf.printf "%b\n" (GT.transform(c) (rewrap_a (fun x y -> x = y)) (rewrap_b (fun x y -> x = y)) (new @eq[c]) (`t x) x);
  Printf.printf "%b\n" (GT.transform(c) (rewrap_a (fun x y -> x = y)) (rewrap_b (fun x y -> x = y)) (new @eq[c]) (`t y) y);
  Printf.printf "%b\n" (GT.transform(c) (rewrap_a (fun x y -> x = y)) (rewrap_b (fun x y -> x = y)) (new @eq[c]) (`t x) y)

