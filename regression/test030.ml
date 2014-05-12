open GT

@type ('a, 'b) t = A of ('a * 'b) with show, map

class ['a,'b] print =
  object
    inherit ['a, string, 'b, string, unit, unit] @t
    method c_A _ _ (a, b) = Printf.printf "A (%s, %s)\n" (a.fx ()) (b.fx ())
  end

let _ =
  Printf.printf "%s\n" 
    (transform(t) 
       (fun _ s -> s) 
       (fun _ n -> string_of_int n) 
       (new @show[t]) 
       () 
       (transform(t) (fun _ x -> string_of_int x) (fun _ x -> int_of_string x) (new @map[t]) () (A (1, "2")))
    );
  transform(t) (fun _ n -> string_of_int n) (fun _ s -> s) (new print) () (A (1, "2"))
