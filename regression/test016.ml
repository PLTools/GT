@type ('a, 'b, 'c) t = A of 'a | B of 'b | C of 'c deriving eq

let _ =
  let x = A 1 in
  let y = B "2" in
  let z = C "4" in  
  let compare x y = GT.transform(t) (function (`aa x) -> fun y -> x = y | _ -> fun _ -> false) 
                                    (function (`ab x) -> fun y -> x = y | _ -> fun _ -> false) 
                                    (function (`ac x) -> fun y -> x = y | _ -> fun _ -> false) new @eq[t] (`tt x) y in
  Printf.printf "x == x: %b\n" (compare x x);
  Printf.printf "x == y: %b\n" (compare x y);
  Printf.printf "x == z: %b\n" (compare x z)
