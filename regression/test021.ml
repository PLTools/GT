@type a = [`A of b | `C of GT.int GT.list] with show
and   b = [`B of a | `D of GT.string] with show

class show_a' prereq fself = object
  inherit [_] show_a_t_stub prereq fself
  method c_C () a _ys = "new " ^ (prereq.show_a.show_a_trf a)
end

let show_a' =
  let { show_a } = show_fix_a ~a0:{ show_a_func = new show_a' } ()
  in
  show_a.show_a_trf


let _ =
  let x = `A (`B (`C [1; 2; 3; 4])) in
  let y = `B (`A (`D "3")) in
  Printf.printf "%s\n" (GT.transform0(a) (new @a[show]) x);
  Printf.printf "%s\n" (GT.transform0(b) (new @b[show]) y);
  Printf.printf "%s\n" (show_a' x)
