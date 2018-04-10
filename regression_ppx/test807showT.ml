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
  (* [@@deriving gt ~show_typed ] *)
    class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_logic =
      object
        method virtual  c_Var : 'inh -> GT.int -> 'syn
        method virtual  c_Value : 'inh -> 'a -> 'syn
      end
    let gcata_logic tr inh t =
      match t with
      | Var _a -> tr#c_Var inh _a
      | Value _a -> tr#c_Value inh _a
    let _ = gcata_logic
    class ['a,'extra] show_typed_logic fself  fa =
      object
        inherit  ['a,unit,string,'inh,string,'extra] class_logic
        constraint 'inh = unit
        method c_Var () _a =
          Format.sprintf "Var(%s)"
            ((fun subj -> (let (module Op)  = GT.int in Op.show_typed) subj)
               _a)
        method c_Value () _a = Format.sprintf "Value(%s)" ((snd fa) _a)
      end
    let rec show_typed_logic fa =
      ("wtf",
        (fun subj ->
           GT.fix0
             (fun self -> (gcata_logic ((new show_typed_logic) self fa)) ())
             subj))
    let _ = show_typed_logic
    module type MT_logic  =
      sig
        val gcata :
          < c_Var: 'inh -> GT.int -> 'syn  ;c_Value: 'inh -> 'a -> 'syn   ;..
            >  -> 'inh -> 'a logic -> 'syn
        val show_typed :
          (string * ('a -> string)) -> (string * ('a logic -> string))
      end
    let logic = ((module
      struct
        let gcata = gcata_logic
        let _ = gcata
        let show_typed = show_typed_logic
        let _ = show_typed
      end) : (module MT_logic))

end

let () =
  let open Lo in
  let sh x = show_typed_logic show_typed_string () x in
  Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")

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
