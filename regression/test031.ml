open GT

@type ('a, 'b) t = {x: int; y: string; a: 'a; b: 'b} with show, gmap

class ['a, 'b] print fa fb _ =
  object 
    inherit [unit, 'a, unit, unit, 'b, unit, unit, _, unit] @t
    method do_t () {x; y; a; b } =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y; 
      let () = fa a in
      let () = fb b in
      ()
  end

let _ =
  Printf.printf "%s\n"
    (transform0(t) (new @t[show] string_of_int (fun x -> x)) @@
     transform0(t) (new @t[gmap] int_of_string (fun `B -> "`B")) @@
	   {x=1; y="2"; a="3"; b=`B}
    )
    ;
  transform0(t) (new print (Printf.printf "%s\n") (function `B -> Printf.printf "`B\n") )
    {x=1; y="2"; a="a"; b=`B}
