@type a = [`A of GT.int | `B of b] deriving show
and   b = [`C of GT.string | `D of a] deriving show

@type c = [a | b] deriving show

class show_c' =
  object
    inherit @show[c]
    method c_C _ _ s = "new C " ^ s
  end

let _ =
  let y = `D (`B (`C "5")) in
  Printf.printf "%s\n" (GT.transform(c) new @show[c] () y);
  Printf.printf "%s\n" (GT.transform(c) new show_c' () y)
