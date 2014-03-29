@type ('a, 'b, 'c) t = A of 'a | B of 'b | C of 'c deriving eq

let _ =
  let x = A 1 in
  let y = B "2" in
  let z = C "4" in  
  let compare x y = GT.transform(t) (function (`at_0 x) -> fun y -> x = y | _ -> fun _ -> false) 
                                    (function (`at_1 x) -> fun y -> x = y | _ -> fun _ -> false) 
                                    (function (`at_2 x) -> fun y -> x = y | _ -> fun _ -> false) new @eq[t] (`t x) y in
  Printf.printf "x == x: %b\n" (compare x x);
  Printf.printf "x == y: %b\n" (compare x y);
  Printf.printf "x == z: %b\n" (compare x z)
