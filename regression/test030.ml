open GT

@type ('a, 'b) t = A of ('a * 'b) with show, map, foldl, foldr, eq, compare

class ['a,'b] print =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, unit] @t
    method c_A _ _ (a, b) = Printf.printf "A (%s, %s)\n" (a.fx ()) (b.fx ())
  end

let _ =
  let cs    = function EQ -> "EQ" | GT -> "GT" | LT -> "LT" in  
  let c x y = if x = y then EQ else if x < y then LT else GT in
  let x = A (1, "2") in
  let y = A (1, "3") in
  Printf.printf "x == x: %b\n" (transform(t) (=) (=) (new @eq[t]) x x);
  Printf.printf "x == y: %b\n" (transform(t) (=) (=) (new @eq[t]) x y);
  Printf.printf "compare (x, x) = %s\n" (cs (transform(t) c c (new @compare[t]) x x));
  Printf.printf "compare (x, y) = %s\n" (cs (transform(t) c c (new @compare[t]) x y));
  Printf.printf "compare (y, x) = %s\n" (cs (transform(t) c c (new @compare[t]) y x));
  Printf.printf "%s\n" 
    (transform(t) 
       (fun _ s -> s) 
       (fun _ n -> string_of_int n) 
       (new @show[t]) 
       () 
       (transform(t) (fun _ x -> string_of_int x) (fun _ x -> int_of_string x) (new @map[t]) () (A (1, "2")))
    );
  transform(t) (fun _ n -> string_of_int n) (fun _ s -> s) (new print) () (A (1, "2"))
