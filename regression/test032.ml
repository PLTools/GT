open GT

@type test = string with show, map, foldl, foldr, eq, compare

let _ = 
  Printf.printf "%s\n" (transform(test) (new @show[test]) () "abc")

