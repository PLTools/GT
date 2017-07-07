type ('a,'b) glist = Nil | Cons of 'a * 'b
[@@deriving gt {show}]


let () =
  let rec show fa xs =
    glist_gcata (GT.lift fa) (GT.lift @@ show fa) (new show_glist) () xs
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Cons (2, Nil))));
()

(* type 'a list = ('a, 'a list) glist
[@@deriving gt {show}] *)
