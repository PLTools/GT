@type ('a, 'b, 'c) t = A of 'a | B of 'b | C of 'c deriving eq, compare

let _ =
  let x = A 1 in
  let y = B "2" in
  let z = C "4" in  
  let compare x y = 
    match 
      GT.transform(t) 
	(function (`aa x) -> fun y -> GT.compare_primitive x y | _ -> fun _ -> invalid_arg "type error (should not happen)") 
        (function (`ab x) -> fun y -> GT.compare_primitive x y | _ -> fun _ -> invalid_arg "type error (should not happen)") 
        (function (`ac x) -> fun y -> GT.compare_primitive x y | _ -> fun _ -> invalid_arg "type error (should not happen)") new @compare[t] (`tt x) y 
    with
    | GT.EQ -> "EQ"
    | GT.LT -> "LT"
    | GT.GT -> "GT"
  in
  Printf.printf "x == x: %s\n" (compare x x);
  Printf.printf "x == y: %s\n" (compare x y);
  Printf.printf "x == z: %s\n" (compare x z)
