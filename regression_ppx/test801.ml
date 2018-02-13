open GT
type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt ~gmap ~show ~foldl ]

let () =
  let sh xs = show_alist  (fun () -> id) (fun () -> id) () xs in
  let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb"))

(* type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ~foldl ]
 *
 * let () =
 *   let sh x = show_list (fun () x -> x) () x in
 *   Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))
 *
 * type 'a logic = Var of int | Value of 'a [@@deriving gt ~gmap ~show ~foldl ]
 *
 *
 * let () =
 *   let sh x = show_logic (fun () x -> x) () x in
 *   Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")
 *
 * type 'a llist = ('a, 'a llist) alist logic [@@deriving gt ~gmap ~show ~foldl ]
 *
 * let () =
 *   let sh x = show_llist (fun () x -> x) () x in
 *   Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) ) *)
