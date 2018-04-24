open GT

type ('a, 'b) t = int * (string * ('a * 'b)) (* with show, gmap, eq, compare, foldl, foldr *)
(* [@@deriving gt ~show ~eq ~compare] *)

class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'extra] class_t =
  object
    inherit
      [int,int,int,(string * ('a * 'b)),(string * ('a * 'b)),(string * ('a *
                                                               'b)),'inh,
      'syn,'extra] GT.class_tuple2
  end
let gcata_t = GT.gcata_tuple2
class ['a,'b,'extra] show_t fself  fa  fb =
  object
    inherit  ['a,unit,string,'b,unit,string,unit,string,'extra] class_t
    constraint 'inh = unit
    inherit  (([int,(string * ('a * 'b)),'extra] GT.show_tuple2) fself
      (GT.int.GT.plugins#show )
      (fun subj  ->
         ((let open GT in pair.GT.plugins)#show
            (fun subj  -> (let open GT in string.GT.plugins)#show subj)
            (fun subj  -> ((let open GT in pair.GT.plugins)#show fa fb) subj))
           subj))
  end
let rec show_t fa fb subj =
  GT.fix0 (fun self  -> (gcata_t ((new show_t) self fa fb)) ()) subj
class ['a,'b] compare_t fself  fa  fb =
  object
    inherit
      ['a,'a,GT.comparison,'b,'b,GT.comparison,('a,'b) t,GT.comparison,
      'extra] class_t
    inherit  (([int,(string * ('a * 'b))] GT.compare_tuple2) fself
      (fun inh  ->
         fun subj  -> (let open GT in int.GT.plugins)#compare inh subj)
      (fun inh  ->
         fun subj  ->
           ((let open GT in pair.GT.plugins)#compare
              (fun inh  ->
                 fun subj  ->
                   (let open GT in string.GT.plugins)#compare inh subj)
              (fun inh  ->
                 fun subj  ->
                   ((let open GT in pair.GT.plugins)#compare fa fb) inh subj))
             inh subj))
  end
let rec compare_t fa fb the_init subj =
  GT.fix0 (fun self  -> gcata_t ((new compare_t) self fa fb)) the_init subj
class ['a,'b] eq_t fself  fa  fb =
  object
    inherit  ['a,'a,bool,'b,'b,bool,('a,'b) t,bool,'extra] class_t
    inherit  (([int,(string * ('a * 'b))] GT.eq_tuple2) fself
      (fun inh  -> fun subj  -> (let open GT in int.GT.plugins)#eq inh subj)
      (fun inh  ->
         fun subj  ->
           ((let open GT in pair.GT.plugins)#eq
              (fun inh  ->
                 fun subj  -> (let open GT in string.GT.plugins)#eq inh subj)
              (fun inh  ->
                 fun subj  ->
                   ((let open GT in pair.GT.plugins)#eq fa fb) inh subj)) inh
             subj))
  end
let rec eq_t fa fb the_init subj =
  GT.fix0 (fun self  -> gcata_t ((new eq_t) self fa fb)) the_init subj
let t =
  {
    GT.gcata = gcata_t;
    GT.plugins =
      (object
         method show = show_t
         method compare = compare_t
         method eq = eq_t
       end)
  }

class ['a, 'b] print _ fa fb =
  object 
    inherit ['a, unit, unit, 'b, unit, unit, unit, unit] class_t
    method value _ x (y, (a, b)) =
      Printf.printf "%d\n" x;
      Printf.printf "%s\n" y; 
      fa () a;
      fb () b;
  end

let _ =
  let cs    = function EQ -> "EQ" | GT -> "GT" | LT -> "LT" in  
  let c x y = if x = y then EQ else if x < y then LT else GT in
  let x = (1, ("2", ("a", `B))) in
  let y = (1, ("2", ("3", `B))) in
  Printf.printf "x == x: %b\n" (transform(t) (=) (=) (new eq_t) x x);
  Printf.printf "x == y: %b\n" (transform(t) (=) (=) (new eq_t) x y);
  Printf.printf "compare (x, x) = %s\n" (cs (transform(t) c c (new compare_t) x x));
  Printf.printf "compare (x, y) = %s\n" (cs (transform(t) c c (new compare_t) x y));
  Printf.printf "compare (y, x) = %s\n" (cs (transform(t) c c (new compare_t) y x));
  Printf.printf "%s\n"
    (transform(t)
       (fun _ a -> string_of_int a)
       (fun _ -> function `B -> "`B")
       (new show_t)
       ()
       (transform(t) (fun _ a -> int_of_string a) (fun _ x -> x) (new gmap_t) () y)
    );
  transform(t) 
    (fun _ a -> Printf.printf "%s\n" a) 
    (fun _ -> function `B -> Printf.printf "`B\n") 
    (new print) 
    () 
    x
