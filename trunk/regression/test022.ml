@type 'a t = A of 'a GT.option GT.list with show, eq

let _ =
  let x = A [Some 1; None; Some 2; None]   in
  let y = A [Some 1; None; Some 2; Some 4] in
  let z = A [] in
  let si _ = string_of_int in
  Printf.printf "x=%s\n" (GT.transform(t) si (new @show[t]) () x);
  Printf.printf "x=%s\n" (GT.transform(t) si (new @show[t]) () y);
  Printf.printf "x=%s\n" (GT.transform(t) si (new @show[t]) () z);
  Printf.printf "x == x = %b\n" (GT.transform(t) (=) (new @eq[t]) x x);
  Printf.printf "x == y = %b\n" (GT.transform(t) (=) (new @eq[t]) x y);
  Printf.printf "x == z = %b\n" (GT.transform(t) (=) (new @eq[t]) x z)

