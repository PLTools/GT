open GT

@type ('a, 'b) t = {x: int; y: string; a: 'a; b: 'b} with show, map

class ['a, 'b] print =
  object 
    inherit ['a, unit, unit, 'b, unit, unit, unit, unit] @t
    method value _ _ x y a b = 
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y; 
      a.fx (); 
      b.fx ()
  end

let _ =
  Printf.printf "%s\n" 
    (transform(t) 
       (fun _ a -> string_of_int a) 
       (fun _ x -> x)
       (new @show[t])
       ()
       (transform(t)
	  (fun _ a -> int_of_string a)
	  (fun _ `B -> "`B")
	  (new @map[t])
	  ()
	  {x=1; y="2"; a="3"; b=`B}
       )
    );
  transform(t) 
    (fun _ a -> Printf.printf "%s\n" a) 
    (fun _ -> function `B -> Printf.printf "`B\n") 
    (new print) 
    () 
    {x=1; y="2"; a="a"; b=`B}
