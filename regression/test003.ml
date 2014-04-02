@type a = A of b | C of GT.int GT.list with show
and   b = B of a | D of GT.string with show

class show_a' =
  object(this)
    inherit @show[a] as super
    method c_C i x y = "new " ^ super#c_C i x y
  end

let _ =
  let x = A (B (C [1; 2; 3; 4])) in
  let y = B (A (D "3")) in
  Printf.printf "%s\n" (GT.transform(a) (new @show[a]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @show[b]) () y);
  Printf.printf "%s\n" (GT.transform(a) (new show_a') () x);

