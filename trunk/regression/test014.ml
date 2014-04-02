@type a = A of b | C of GT.int GT.list with show
and   b = B of c | D of GT.string with show
and   c = E of a with show

class show_a' =
  object(this)
    inherit @show[a] as super
    method c_C i x y = "new " ^ super#c_C i x y
    method c_A _ _ x = "new A " ^ GT.transform(b) (new +show[b] (ref this)) () x
  end

let _ =
  let x = A (B (E (C [1; 2; 3; 4]))) in
  let y = B (E (A (D "3"))) in  
  Printf.printf "%s\n" (GT.transform(a) (new @show[a]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @show[b]) () y);
  Printf.printf "%s\n" (GT.transform(a) (new show_a') () x);

