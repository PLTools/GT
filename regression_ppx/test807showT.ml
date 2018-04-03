let id x = x
let show_int () = GT.show GT.int
let gmap_int () = GT.gmap GT.int
let foldl_int x = GT.foldl GT.int x

module AL : sig
  type ('a,'b) alist = Nil | Cons of 'a * 'b
  [@@deriving gt ~show_typed ]
end = struct
  type ('a,'b) alist  = Nil | Cons of 'a * 'b
  [@@deriving gt ~show_typed ]
end

let show_typed_string = ("string", fun () -> id)

let () =
  let open AL in
  let sh xs = show_typed_alist show_typed_string show_typed_string () xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()

(* module L : sig
 *   type 'a list = ('a, 'a list) AL.alist
 *   [@@deriving gt ~show_typed ]
 * end = struct
 *   type 'a list = ('a, 'a list) AL.alist
 *   [@@deriving gt ~show_typed ]
 * end *)

module L :
  sig
    type 'a list = ('a,'a list) AL.alist
    (* [@@deriving gt ~show_typed] *)
    class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_list :
      object
        inherit ['a,'ia,'sa,'a list,'ia list,'sa list,'inh,'syn,'extra]
          AL.class_alist
      end
    val gcata_list :
      ('a,_,'sa,'inh,'syn,_)#class_list -> 'inh -> 'a list -> 'syn
    class ['a,'extra] show_typed_list :
      (unit -> 'a list -> string) ->
        (string * (unit -> 'a -> string)) ->
          object
            constraint 'inh = unit
            inherit ['a,'a list,'extra] AL.show_typed_alist
          end
    val show_typed_list :
      (string * (unit -> 'a -> string)) -> unit -> 'a list -> string
    module type MT_list  =
      sig
        val gcata :
          ('a,_,'sa,'inh,'syn,_)#class_list -> 'inh -> 'a list -> 'syn
        val show_typed :
          (string * (unit -> 'a -> string)) -> unit -> 'a list -> string
      end
    val list : (module MT_list)
  end =
  struct
    type 'a list = ('a,'a list) AL.alist
    (* [@@deriving gt ~show_typed] *)
    class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_list =
      object
        inherit  ['a,'ia,'sa,'a list,'ia list,'sa list,'inh,'syn,'extra]
          AL.class_alist
      end
    let gcata_list = AL.gcata_alist
    class ['a,'extra] show_typed_list _fself  fa =
      object
        inherit  ['a,unit,string,'inh,string,'extra] class_list
        constraint 'inh = unit
        inherit  ((['a,'a list,'extra] AL.show_typed_alist) _fself (snd fa)
          _fself)
      end
    let show_typed_list fa the_init t =
      GT.fix0 (fun self  -> gcata_list ((new show_typed_list) self fa))
        the_init t

    module type MT_list  =
      sig
        val gcata :
          ('a,_,'sa,'inh,'syn,_)#class_list -> 'inh -> 'a list -> 'syn
        val show_typed :
          (string * (unit -> 'a -> string)) -> unit -> 'a list -> string
      end
    let list : (module MT_list) = (module
      struct let gcata = gcata_list
             let show_typed = show_typed_list  end)

  end

let () =
  let open L in
  let sh x = show_typed_list show_typed_string () x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))

(* module Lo : sig
 *   type 'a logic = Var of int | Value of 'a
 *   [@@deriving gt ~show_typed ]
 * end = struct
 *   type 'a logic = Var of int | Value of 'a
 *   [@@deriving gt ~show_typed ]
 * end
 *
 * let () =
 *   let open Lo in
 *   let sh x = show_typed_logic show_typed_string () x in
 *   Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf") *)

(* module LList : sig
 *   type 'a llist = ('a, 'a llist) AL.alist Lo.logic
 *   [@@deriving gt ~show_typed ]
 * end = struct
 *   type 'a llist = ('a, 'a llist) AL.alist Lo.logic
 *   [@@deriving gt ~show_typed ]
 * end
 *
 * let () =
 *   let sh x = LList.show_llist show_typed_string () x in
 *   Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) ) *)
