@type a = [`A of b | `C of GT.int GT.list] with show
and   b = [`B of a | `D of GT.string] with show

class show_a' fix fself = object
  inherit [_] show_a_t fix fself as super
  method c_C () a xs = "new " ^ (super#c_C () a xs)
end

let show_a_fix2 =
  Fix_show_a.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ishow_a.i) ->
           (match sym with
              Ishow_a.A -> GT.transform_gc gcata_a (new show_a'  f)
            | Ishow_a.B -> GT.transform_gc gcata_b (new show_b_t f) :
            a)})

let show_a' s = show_a_fix2.Fix_show_a.call Ishow_a.A () s

let _ =
  let x = `A (`B (`C [1; 2; 3; 4])) in
  let y = `B (`A (`D "3")) in
  Printf.printf "%s\n" (GT.transform(a) (new @a[show] show_a_fix) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show] show_b_fix) () y);
  Printf.printf "%s\n" (show_a' x)
