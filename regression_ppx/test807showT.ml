let id x = x

module AL : sig
  type ('a,'b) alist = Nil | Cons of 'a * 'b
  [@@deriving gt ~show_typed ]
end = struct
  type ('a,'b) alist  = Nil | Cons of 'a * 'b
  [@@deriving gt ~show_typed]
end

let show_typed_string = ("string", Printf.sprintf "\"%s\"")

let () =
  let open AL in
  let sh xs = snd (show_typed_alist show_typed_string show_typed_string) xs in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));

  ()

module L : sig
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~show_typed ]
end = struct
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~show_typed ]
end


let () =
  let open L in
  let sh x = snd (show_typed_list show_typed_string) x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))


module GT = struct
  include GT
  module type MT_int2 = sig
    include MT_int
    val gcata: ('inh, 'syn) # GT.int_tt -> 'inh -> int -> 'syn
    val show    : int -> string
    val gmap    : int -> int
    val show_typed   : string * (int -> string)
  end

  let int : (module MT_int2) =
    (module struct
      include (val GT.int : GT.MT_int)
      let show_typed = ("int", string_of_int )
    end)
end

module Lo : sig
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~show_typed ]
end = struct
  type 'a logic = Var of GT.int | Value of 'a
  [@@deriving gt ~show_typed ]

end

let () =
  let open Lo in
  let sh x = snd (show_typed_logic show_typed_string) x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")

module LList : sig
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~show_typed ]
end = struct
  type 'a llist = ('a, 'a llist) AL.alist Lo.logic
  [@@deriving gt ~show_typed ]
end

let () =
  let sh x = snd (LList.show_llist show_typed_string)  x in
  Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) )
