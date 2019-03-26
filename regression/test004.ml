@type a = [`A of b | `C of GT.int] with show
and   b = [`B of a GT.list | `D of GT.string] with show

let x = `A (`B [`C 3; `C 4])
let y = `B [`A (`D "3"); `C 5]

let () =
  Printf.printf "%s\n" (GT.transform(a) (new @a[show] show_a_fix) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show] show_b_fix) () y)

class show_a' fix fself =
  object(this)
    inherit [_] @a[show] fix fself as super
    method c_C i x y = "new " ^ super#c_C i x y
    method c_A _ _ x = Printf.sprintf "new A %a" (fix.Fix_show_a.call Ishow_a.B) x
  end

let new_fix =
  Fix_show_a.fixv
    (fun f ->
       {call =
         fun (type a) (sym : a Ishow_a.i) ->
           (match sym with
              Ishow_a.A -> GT.transform_gc gcata_a (new show_a'  f)
            | Ishow_a.B -> GT.transform_gc gcata_b (new show_b_t f)
            : a) } )

let () =
  Printf.printf "%s\n" (GT.transform(a) (new show_a'  new_fix) () x)
