(* The same as test026 but with polymorphic variants *)
@type ('a, 'b) a = [`A of 'a | `B of 'b] with show, eq
@type ('a, 'b) b = [('b, 'a) a] with show, eq

let _ =
  let x = `A 3 in
  let y = `A "2" in
  let int x = string_of_int x in
  let string x = x in
  Printf.printf "%s\n" (GT.show(a) (GT.lift int) (GT.lift string)  x);
  Printf.printf "%s\n" (GT.show(a) (GT.lift string) (GT.lift int)  y);
  Printf.printf "%s\n" (GT.show(b) (GT.lift string) (GT.lift int)  x);
  Printf.printf "%s\n" (GT.show(b) (GT.lift int) (GT.lift string)  y);
  Printf.printf "%b\n" (GT.eq(a) (=) (=) x x);
  Printf.printf "%b\n" (GT.eq(b) (=) (=) x x);
