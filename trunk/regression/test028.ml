open GT

@type ('a, 'b) t = (int * string * 'a * 'b)

class ['a, 'b] print =
  object 
    inherit ['a, unit, 'b, unit, unit, unit] @t
    method value _ _ x y a b = 
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y; 
      a.fx (); 
      b.fx ()
  end

let _ =
  transform(t) 
    (fun _ a -> Printf.printf "%s\n" a) 
    (fun _ -> function `B -> Printf.printf "`B\n") 
    (new print) 
    () 
    (1, "2", "a", `B)
