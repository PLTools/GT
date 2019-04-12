(* Camlp5 version of test802 *)
open Printf

@type a = [`A of b | `C of GT.int] with show
and   b = [`B of a | `D of GT.string] with show

module Show2 = struct
  class ['self] show_b_t_stub2 (for_a,for_b) fself = object
    inherit ['self] show_b_t_stub (for_a,for_b) fself
    method c_C () (_ :b) a  = Printf.sprintf "new C (%s)" (for_a () a)
    method c_D () _ s  = Printf.sprintf "new D %s" s
  end

  let showa0 a b  = Printf.printf "new!\n"; new show_a_t_stub  a b
  let showb0 a b  = Printf.printf "new!\n"; new show_b_t_stub2 a b

  let show_a () s =
    (fst @@ fix_a showa0 showb0) () s

  let show_b () s =
    (snd @@ fix_a showa0 showb0) () s

  let _ = Printf.printf "%s\n" (show_a () (`A (`B (`A (`D "4")))))
end

@type c = [`E of GT.int GT.list | b] with show

let x = `A (`B (`C 3))
let y = `B (`A (`D "3"))
let z = `E [1; 2; 3]

let () =
  Printf.printf "%s\n" (GT.show(a)   x);
  Printf.printf "%s\n" (GT.show(b)   y);
  Printf.printf "%s\n" (GT.show(c)   z);
  Printf.printf "%s\n" (GT.show(c)   y);

class ['self] show_c_stub2 make_clas (fself: _ -> _ -> _) =
  let show_a2,show_b2 =
    Show2.(fix_a
             showa0
             (fun _ _ -> ((make_clas fself ()) :> 'self show_b_t_stub) ))
  in
  object
    inherit [unit, _, string] c_t
    inherit [ 'self] show_b_t_stub (show_a2,show_b2) fself
    method! c_B () _ a  = sprintf "new `B (%s)" (show_a2 () a)
    method! c_D () _ s  = sprintf "new `D %s" s
    method  c_E () _ s  = sprintf "new `E %s" (GT.(show list) GT.(show int) s)
  end

let rec showc0 fself () = Printf.printf "new c0!\n"; new show_c_stub2 showc0 fself

let show_c () s =
  let rec trait () s = gcata_c (showc0 trait ()) () (s :> c)
  in
  trait () s


let () =
  Printf.printf "%s\n" (show_c () z);
  Printf.printf "%s\n" (show_c () y)
