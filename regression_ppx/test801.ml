(* open GT *)



let show_int () = GT.show GT.int
let gmap_int () = GT.gmap GT.int

module AList : sig
  type ('a,'b) alist = Nil | Cons of 'a * 'b
  (* [@@deriving gt ~gmap ~show ~foldl ] *)
    class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] class_alist :
      object
        method  virtual c_Nil : 'inh -> 'syn
        method  virtual c_Cons : 'inh -> 'a -> 'b -> 'syn
      end
    val gcata_alist :
      < c_Nil: 'inh -> 'syn  ;c_Cons: 'inh -> 'a -> 'b -> 'syn   ;.. >  ->
        'inh -> ('a,'b) alist -> 'syn
    class ['a,'b] show_alist : object
      (* inherit ['a,unit,string,'b,unit,string,unit,string] class_alist *)
      method c_Nil : (unit) -> (string )
      method c_Cons : 'inh -> 'a -> 'b -> 'syn
      constraint 'inh = unit
      constraint 'syn = string
    end
    val show_alist :
      (unit -> 'a -> string) ->
        (unit -> 'b -> string) -> unit -> ('a,'b) alist -> string

end = struct
  type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt (* ~gmap *) ~show  ]
end
open AList

let () =
  let sh xs = show_alist  (fun () -> id) (fun () -> id) () xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()


(* module L : sig
 *   type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ~foldl ]
 * end = struct
 *   type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ~foldl ]
 * end
 * open L
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
