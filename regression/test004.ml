@type a = [`A of b | `C of GT.int] with show
and   b = [`B of a GT.list | `D of GT.string] with show

class show_a' =
  object(this)
    inherit @show[a] as super
    method c_C i x y = "new " ^ super#c_C i x y
    method c_A _ _ x = "new A " ^ GT.transform(b) (new +show[b] (ref this)) () x
  end

let _ =
  let x = `A (`B [`C 3; `C 4]) in
  let y = `B [`A (`D "3"); `C 5] in
  Printf.printf "%s\n" (GT.transform(a) (new @show[a]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @show[b]) () y);
  Printf.printf "%s\n" (GT.transform(a) (new show_a') () x);

