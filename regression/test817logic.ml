type ('a,'b) glist = Nil | Cons of 'a * 'b
[@@deriving gt ~options:{show}]

let () =
  let rec show fa xs = GT.show glist fa (show fa) xs
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
    GT.transform list  (new show_list_t (GT.lift fa)) () xs
  in
  Printf.printf "%s\n%!" (show string_of_int (Nil));
  Printf.printf "%s\n%!" (show (fun x -> x) (Cons ("WTF", Nil)));
  Printf.printf "%s\n%!" (show string_of_int (Cons (3, Cons (4, Nil))));
  ()

type intlist = GT.int list
[@@deriving gt ~options:{show}]

let () =
  let rec show xs = GT.transform intlist (new show_intlist_t) () xs in
  Printf.printf "%s\n%!" (show  Nil);
  Printf.printf "%s\n%!" (show  (Cons (6, Nil)));
  Printf.printf "%s\n%!" (show  (Cons (7, Cons (8, Nil))));
  ()


module Lo = struct
  type 'a t = Var of GT.int | Value of 'a [@@deriving gt ~options:{show}]
end

let () =
  let show xs = GT.show Lo.t (GT.show GT.int) xs in
  Printf.printf "Default logic values\n%!";
  Printf.printf "\t%s\n%!" (show  (Var 5));
  Printf.printf "\t%s\n%!" (show  (Value 6));
  ()

module Lo2 = struct
  type 'a t = 'a Lo.t = Var of GT.int | Value of 'a [@@deriving gt ~options:{show}]

  class ['a, 'self] my_show fa fself = object
    inherit ['a, 'self] Lo.show_t_t fa fself
    method c_Value () _ x = fa () x
  end

  let t =
    { Lo.t with
      GT.plugins = object
        method show fa xs =
          GT.transform (Lo.t) (new my_show (GT.lift fa)) () xs
      end }
end

let () =
  let show xs = GT.show Lo2.t (GT.show GT.int) xs in
  Printf.printf "Modified logic values\n%!";
  Printf.printf "\t%s\n%!" (show  (Var 5));
  Printf.printf "\t%s\n%!" (show  (Value 6));
  ()


module LList2 = struct
  type 'a t = ('a, 'a t) glist Lo2.t [@@deriving gt ~options:{show}]
end

let () =
  let show xs = GT.show LList2.t (GT.show GT.int) xs in
  Printf.printf "Modified logic list values\n%!";
  Printf.printf "\t%s\n%!" (show  @@ Value Nil);
  Printf.printf "\t%s\n%!" (show  @@ Value (Cons (6, Value Nil)));
  ()
