@type a = [`A of b | `C of GT.int] with show
and   b = [`B of a | `D of GT.string] with show

@type c = [`E of GT.int GT.list | b] with show

let x = `A (`B (`C 3))
let y = `B (`A (`D "3"))
let z = `E [1; 2; 3]

let () =
  Printf.printf "%s\n" (GT.transform(a) (new @a[show] show_a_fix) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show] show_b_fix) () y);
  Printf.printf "%s\n" (GT.transform(c) (new @c[show] show_c_fix) () z);
  Printf.printf "%s\n" (GT.transform(c) (new @c[show] show_c_fix) () y);

class show_c' fixC fixB fself =
  object(this)
    inherit [_] @c[show] fixC fself as super
    inherit [_] @b[show] fixB fself as superB
    method! c_E () s y = "new " ^ super#c_E () s y
    method! c_B () s y = "new " ^ super#c_B () s y
    method! c_D () s y = "new " ^ super#c_D () s y
  end


let show_c_fix2 =
  Fix_show_c.fixv (fun f ->
      { call =
         fun (type a) (sym : a Ishow_c.i) ->
           (match sym with Ishow_c.C ->
              GT.transform_gc gcata_c (new show_c' f show_b_fix) : a) })


let show_a0' f () s = GT.transform_gc gcata_a (new show_a_t f) () s

let show_ab_fix2 =
  Fix_show_a.fixv (fun f ->
      { call =
         (fun (type w) -> fun (sym : w Ishow_a.i) ->
           (match sym with
            | Ishow_a.A -> show_a0' f
            | Ishow_a.B ->
              GT.transform_gc gcata_c (new show_c' show_c_fix2 f)) : w)
      })

let () =
  Printf.printf "%s\n" (GT.transform(c) (new show_c' show_c_fix2 show_b_fix) () z);
  Printf.printf "%s\n" (GT.transform(c) (new show_c' show_c_fix2 show_b_fix) () y)
