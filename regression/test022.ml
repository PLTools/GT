@type 'a t = A of 'a GT.option GT.list with show, eq

let _ =
  let x = A [Some 1; None; Some 2; None]   in
  let y = A [Some 1; None; Some 2; Some 4] in
  let z = A [] in
  let si = string_of_int in
  Printf.printf "x=%s\n" (GT.transform0(t) (new @t[show] si) x);
  Printf.printf "x=%s\n" (GT.transform0(t) (new @t[show] si) y);
  Printf.printf "x=%s\n" (GT.transform0(t) (new @t[show] si) z);
  Printf.printf "x == x = %b\n" (GT.transform(t) (new @t[eq] (=)) x x);
  Printf.printf "x == y = %b\n" (GT.transform(t) (new @t[eq] (=)) x y);
  Printf.printf "x == z = %b\n" (GT.transform(t) (new @t[eq] (=)) x z)
