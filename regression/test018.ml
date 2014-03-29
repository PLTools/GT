@type ('a, 'b, 'c) t = A of 'a | B of 'b | C of 'c deriving eq, compare

let _ =
  let x = A 1 in
  let y = B "2" in
  let z = C "4" in  
  let compare x y = 
    match 
      GT.transform(t) 
        (rewrap_t  (fun x y -> GT.compare_primitive x y))
        (rewrap_t1 (fun x y -> GT.compare_primitive x y))
        (rewrap_t2 (fun x y -> GT.compare_primitive x y))
	new @compare[t] (`t x) y 
    with
    | GT.EQ -> "EQ"
    | GT.LT -> "LT"
    | GT.GT -> "GT"
  in
  Printf.printf "x == x: %s\n" (compare x x);
  Printf.printf "x == y: %s\n" (compare x y);
  Printf.printf "x == z: %s\n" (compare x z)
