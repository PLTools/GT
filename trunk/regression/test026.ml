@type ('a, 'b) a = A of 'a | B of 'b with show, eq
@type ('a, 'b) b = ('b, 'a) a with show, eq

let _ =
  let x = A 3 in
  let y = A "2" in
  let int _ x = string_of_int x in
  let string _ x = x in
  Printf.printf "%s\n" (GT.transform(a) int string (new @a[show]) () x);
  Printf.printf "%s\n" (GT.transform(a) string int (new @a[show]) () y);
  Printf.printf "%s\n" (GT.transform(b) string int (new @b[show]) () x);
  Printf.printf "%s\n" (GT.transform(b) int string (new @b[show]) () y);
  Printf.printf "%b\n" (GT.transform(a) (=) (=) (new @a[eq]) x x);
  Printf.printf "%b\n" (GT.transform(b) (=) (=) (new @b[eq]) x x);


