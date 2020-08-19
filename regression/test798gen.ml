type ('a,'b) glist = Nil | Cons of 'a * 'b
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs = glist.GT.plugins#show fa (show fa) xs
    (* glist_gcata (GT.lift fa) (GT.lift @@ show fa) (new show_glist) () xs *)
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (2, Cons (2, Nil))));
()

type 'a list = ('a, 'a list) glist
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs =
    list_gcata (GT.lift fa) (new show_list (show fa)) () xs
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show (fun x -> x) (Cons ("WTF", Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (3, Cons (4, Nil))));
  ()

type intlist = int list
[@@deriving gt ~options:{show}]

let () =
  let rec show xs = intlist_gcata (new show_intlist show) () xs in
  Printf.printf "%s\n%!" (show  Nil);
  Printf.printf "%s\n%!" (show  (Cons (6, Nil)));
  Printf.printf "%s\n%!" (show  (Cons (7, Cons (8, Nil))));
  ()
