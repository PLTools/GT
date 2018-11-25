@type a = [`A of GT.int | `B of b] with show
and   b = [`C of GT.string | `D of a] with show

@type c = [a | b] with show

(* Doesn't work for now *)
class show_c' fself =
  object
    inherit [_] @c[show] fself
    method c_C () _ s = "new C " ^ s
  end


class show_b' prereq fself =
  object
    inherit [_] show_b_t_stub prereq fself
    method c_C () _ s = "new C " ^ s
  end

let show_b_new =
  let { show_b } = show_fix_a ~b0:{ show_b_func = new show_b' } ()
  in
  show_b.show_b_trf

let _ =
  let y = `D (`B (`C "5")) in
  Printf.printf "%s\n" (GT.transform0(c) (new @c[show]) y);
  Printf.printf "%s\n" (show_b_new y)
