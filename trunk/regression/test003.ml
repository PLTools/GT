@type ('a, 'b) t = A of ['a] * ['b] | C of [('b, 'a) t] deriving show

let _ =
  let x = C (C (A (2, "3"))) in
  Printf.printf "%s\n" (GT.transform(t) (fun _ -> string_of_int) (fun _ x -> x) (new @show[t]) () x)
