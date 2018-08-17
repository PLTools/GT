open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
[@@deriving gt ~options:{show}]


let _ =
  printf "%s\n" @@ show_a (GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);
  ()
