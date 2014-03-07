@type a = [`A of b | `C of int] deriving show
and   b = [`B of a | `D of string] deriving show

class show_a' =
  object(this)
    inherit @show[a]
    method c_A _ _ x = GT.transform(b) (new +show[b] this) () x
  end

let _ =
  let x = `A (`B (`C 3)) in
  let y = `B (`A (`D "3")) in
  Printf.printf "%s\n" (GT.transform(a) (new @show[a]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @show[b]) () y);
  Printf.printf "%s\n" (GT.transform(a) (new show_a') () x);

