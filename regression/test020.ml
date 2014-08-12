@type 'a t = A of GT.int | B of GT.string | C of 'a GT.list with eq, compare

let _ =
  let x   = A 1 in
  let y   = B "2" in
  let z   = C [4] in  
  let z'  = C [4] in
  let z'' = C [] in
  let compare x y = 
    match GT.transform(t) (fun y -> GT.transform(GT.int) new @GT.compare[int] y) new @compare[t] x y with
    | GT.GT -> "GT"
    | GT.LT -> "LT"
    | GT.EQ -> "EQ"
  in
  Printf.printf "x   == x: %s\n" (compare x   x);
  Printf.printf "x   == y: %s\n" (compare x   y);
  Printf.printf "y   == x: %s\n" (compare y   x);
  Printf.printf "x   == z: %s\n" (compare x   z);
  Printf.printf "z'  == z: %s\n" (compare z'  z);
  Printf.printf "z'' == z: %s\n" (compare z'' z)


