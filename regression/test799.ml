open Printf

(* FIRST PART *)
type ('a,'b) t = OK of 'a | Error of 'b
[@@deriving gt {show}] 
(* type ('a) t = OK of 'a | Error of string [@@deriving gt {show}] *)

let () =
  let show fa fb (e: (_,_) t) =
    t.GT.gcata (GT.lift fa) (GT.lift fb) (new show_t_t) () e in
  printf "%s\n%!" (show string_of_int (fun x -> x) (OK 1));
  printf "%s\n%!" (show string_of_int (fun x -> x) (Error "error1"));
  ()

(*
(* SECOND PART *)
type 'a     t2 = ('a, string) t [@@deriving gt {show}]

let () =
  let show fa (e: _ t2) =
    t2.GT.gcata (GT.lift fa) (new show_t2) () e in
  printf "%s\n%!" (show string_of_float (OK 2.));
  printf "%s\n%!" (show string_of_int (Error "error2"));
  ()

(* THIRD PART *)
type t3 = char t2  [@@deriving gt {show}]

let () =
  let show (e: t3) = t3.GT.gcata (new show_t3) () e in
  printf "%s\n%!" (show (OK '3'));
  printf "%s\n%!" (show (Error "error3"));
()
*)
