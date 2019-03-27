module M : sig
@type 'a t1 = [`A | `B of 'a] with show, gmap;;
@type 'a t2 = [`C | `D of 'a] with show, gmap;;
@type 'a t  = ['a t1 | 'a t2] with show, gmap;;
end = struct
  @type 'a t1 = [`A | `B of 'a] with show, gmap;;
  @type 'a t2 = [`C | `D of 'a] with show, gmap;;
  @type 'a t  = ['a t1 | 'a t2] with show, gmap;;

let _ =
  let a = `B (`B `A) in
  let rec mapt1 () x =
    GT.transform(t1) (new gmap_t1_t gmap_t1_fix mapt1) () x
  in
  let rec show1 () x =
    GT.transform(t1) (new show_t1_t show_t1_fix show1) () x
  in
  Printf.printf "a=%s, map a=%s\n" (show1 () a) (show1 () (mapt1 () a));

  let b = `D (`D `C) in
  let rec mapt2 () x =
    GT.transform(t2) (new gmap_t2_t gmap_t2_fix mapt2) () x
  in
  let rec show2 () x =
    GT.transform(t2) (new show_t2_t show_t2_fix show2) () x
  in
  Printf.printf "b=%s, map b=%s\n" (show2 () b) (show2 () (mapt2 () b));

  let c = `D (`B (`D `A)) in
  let rec mapt () x =
    GT.transform(t) (new gmap_t_t gmap_t_fix mapt) () x
  in
  let rec show () x =
    GT.transform(t) (new show_t_t show_t_fix show) () x
  in
  Printf.printf "c=%s, map c=%s\n" (show () c) (show () (mapt () c))
end
