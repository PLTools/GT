@type a = A of b | C of GT.int GT.list with show
and   b = B of c | D of GT.string with show
and   c = E of a with show

class show_a' prereq fself =
  object(this)
    inherit [_] @a[show] fself as super
    method c_C () x y = "new " ^ super#c_C () x y
    method c_A () _ x = "new A " ^ (prereq.show_b.show_b_trf () x)
  end

let show_a' =
  let { show_a } = show_fix_a ~a0:{ show_a_func = new show_a' } ()
  in
  show_a.show_a_trf

let _ =
  let x = A (B (E (C [1; 2; 3; 4]))) in
  let y = B (E (A (D "3"))) in
  Printf.printf "%s\n" (GT.transform(a) (new @a[show]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show]) () y);
  Printf.printf "%s\n" (show_a' () x);
