@type a = A of b | C of GT.int GT.list with show
and   b = B of a | D of GT.string with show

class show_a2stub prereq fself =
  object
    inherit [_] show_a_t_stub prereq fself as super
    method c_C () a ys = "new " ^ super#c_C () a ys
  end

let show_a_new =
  let { show_a } = show_fix_a ~a0:{ show_a_func = new show_a2stub } ()
  in
  show_a.show_a_trf

let a = { a with plugins = object
                   method show = show_a_new
                 end}
let _ =
  let x = A (B (C [1; 2; 3; 4])) in
  let y = B (A (D "3")) in
  Printf.printf "%s\n" (GT.transform0(a) (new @a[show]) x);
  Printf.printf "%s\n" (GT.transform0(b) (new @b[show]) y);
  Printf.printf "%s\n" (GT.show(a) x);
