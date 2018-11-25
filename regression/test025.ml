@type ('a, 'b) a = [`A of 'a | `B of 'b] with show, eq
@type ('a, 'b) b = [('b, 'a) a] with show, eq

let _ =
  let x = `A 3 in
  let y = `A "2" in
  let int x = string_of_int x in
  let string x = x in
  Printf.printf "%s\n" (GT.transform0(a) (new @a[show] int string) x);
  Printf.printf "%s\n" (GT.transform0(a) (new @a[show] string int) y);
  Printf.printf "%s\n" (GT.transform0(b) (new @b[show] string int) x);
  Printf.printf "%s\n" (GT.transform0(b) (new @b[show] int string) y);
  Printf.printf "%b\n" (GT.transform(a) (new @a[eq] (=) (=)) x x);
  Printf.printf "%b\n" (GT.transform(b) (new @b[eq] (=) (=)) x x);
