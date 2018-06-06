@type ('a, 'b) t = GT.int * (GT.string * ('a * 'b))
with show, gmap, eq, compare, foldl,foldr

class ['a, 'b] print (fa: 'a -> unit) fb =
  object
    inherit [unit, 'a, unit, unit, 'b, unit, unit, _, unit] @t
    method c_Pair () x (y, (a, b)) =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y;
      fa a;
      fb b
  end

let printer fa fb subj =
  GT.fix0 (fun self -> GT.transform t (new print fa fb) ()) subj

let _ =
  let cs    = function GT.EQ -> "EQ" | GT -> "GT" | LT -> "LT" in
  let c x y = if x = y then GT.EQ else if x < y then LT else GT in
  let x = (1, ("2", ("a", `B))) in
  let y = (1, ("2", ("3", `B))) in
  let eq1 a b = GT.transform(t) (new @t[eq] (fun _ -> assert false) (=) (=) ) a b in
  Printf.printf "x == x: %b\n" (eq1 x x);
  Printf.printf "x == y: %b\n" (eq1 x y);
  let cmp1 a b = GT.transform(t) (new @t[compare] (fun _ -> assert false) c c) a b in
  Printf.printf "compare (x, x) = %s\n" (cs @@ cmp1 x x);
  Printf.printf "compare (x, y) = %s\n" (cs @@ cmp1 x y);
  Printf.printf "compare (y, x) = %s\n" (cs @@ cmp1 y x);
  Printf.printf "%s\n" @@
  GT.transform(t)
    (new @t[show] (fun _ -> assert false) string_of_int (function `B -> "`B")) () @@
  t.GT.plugins#gmap int_of_string (fun x -> x) y
  ;
  GT.transform(t)
    (new print (fun a -> Printf.printf "%s\n" a) (function `B -> Printf.printf "`B\n"))
    ()
    x
