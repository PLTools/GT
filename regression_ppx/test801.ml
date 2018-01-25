open GT
type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt ~gmap ~show ]

let () =
  let sh x = show_alist (fun () x -> x) (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"))

type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ]

let () =
  let sh x = show_list (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))

type 'a logic = Var of int | Value of 'a [@@deriving gt ~gmap ~show]


let () =
  let sh x = show_logic (fun () x -> x) () x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")

type 'a llist = ('a, 'a llist) alist logic [@@deriving gt ~gmap ~show ]

let () =
  let sh x = show_llist (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) )
