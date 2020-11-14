let id x = x

module AL : sig
  type ('a,'b) alist = [] [@name "nil"] | (::) of 'a * 'b [@name "cons"]
(* with show,foldl,gmap *)
end  = struct
  @type ('a,'b) alist  = [] [@name "nil"] | (::) of 'a * 'b [@name "cons"]
(* with show,foldl,gmap *)
end

(*

let () =
  let open AL in
  let sh xs = GT.show alist id id xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()

module L : sig
  @type 'a list = ('a, 'a list) AL.alist with show,gmap,foldl
end = struct
  @type 'a list = ('a, 'a list) AL.alist with show,gmap,foldl
end

let () =
  let open L in
  let sh x = GT.show list id x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))

module Lo : sig
  @type 'a logic = Var of GT.int | Value of 'a with show,gmap,foldl
end = struct
  @type 'a logic = Var of GT.int | Value of 'a with show,gmap,foldl
end

let () =
  let open Lo in
  let sh x = GT.show logic id x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")


module LList : sig
  @type 'a llist = ('a, 'a llist) AL.alist Lo.logic with show,gmap,foldl
end = struct
  @type 'a llist = ('a, 'a llist) AL.alist Lo.logic with show,gmap,foldl
end

let () =
  let sh x = GT.show LList.llist id x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) )


module Reworked = struct
  class ['a, 'self] my_show fa fself = object
    inherit ['a, 'self] Lo.show_logic_t fa fself
    method c_Value () _ x = fa () x
  end

  let logic =
    { Lo.logic with
      GT.plugins = object
        method show fa xs =
          GT.transform (Lo.logic) (new my_show (GT.lift fa)) () xs
      end }
end

let () =
  let open Reworked in
  let sh x = GT.show logic id x in
  Printf.printf "Modified implementation:\n%!";
  Printf.printf "\t%s\n%!" (sh @@ Var 5);
  Printf.printf "\t%s\n%!" (sh @@ Value "asdf");
  ()



(* module ReworkedLList = struct
  type 'a llist = 'a LList.llist

  class ['a, 'self] my_show fa fself = object
    inherit [string, _] LList.show_llist_t fa fself
    method! c_Value () _ (x: string) =
        assert false
        (* fa () x *)
  end

  let llist =
    { LList.llist with
      GT.plugins = object
        method show fa xs =
          GT.transform (LList.llist) (new my_show (GT.lift fa)) () xs
      end
    }
  let (_:int) = GT.show llist
end

let () =
  let sh x = GT.show ReworkedLList.llist (fun x -> x) x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) ) *) *)
