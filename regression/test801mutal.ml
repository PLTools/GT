open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
[@@deriving gt ~options:{show;gmap}]


let _ =
  printf "Testing Show\n";
  printf "%s\n" @@ show_a (GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);

  printf "Testing Gmap\n";
  printf "%s\n" @@ show_a (GT.show GT.int) @@ gmap_a ((+)1) (D 6);

  ()

let b =
  {
    GT.gcata = gcata_b;
    GT.plugins = (object method gmap = gmap_b method show = show_b end)
  }
let a =
  {
    GT.gcata = gcata_a;
    GT.plugins = (object method gmap = gmap_a method show = show_a end)
  }

(* ************************************ *)
type c = b GT.list [@@deriving gt ~options:{show;gmap}]
type 'a d = 'a GT.list a GT.list  [@@deriving gt ~options:{show;gmap}]
