let id x = x
(* let show_int () = GT.show GT.int
 * let gmap_int () = GT.gmap GT.int
 * let foldl_int x = GT.foldl GT.int x *)

(* module AL : sig
 *   type ('a,'b) alist = Nil | Cons of 'a * 'b
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end = struct
 *   type ('a,'b) alist  = Nil | Cons of 'a * 'b
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end *)

module AL :
  sig
    type ('a, 'b) alist =
      | Nil
      | Cons of 'a * 'b
    (* [@@deriving gt ~gmap ~show ~foldl] *)
    include
      sig
        [@@@ocaml.warning "-32"]
        class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'extra] class_alist :
          object
            method  virtual c_Nil : 'inh -> 'syn
            method  virtual c_Cons : 'inh -> 'a -> 'b -> 'syn
          end
        val gcata_alist :
          < c_Nil: 'inh -> 'syn  ;c_Cons: 'inh -> 'a -> 'b -> 'syn   ;.. >
            -> 'inh -> ('a, 'b) alist -> 'syn
        class ['a,'b,'extra] show_alist :
          (('a, 'b) alist -> string) ->
            ('a -> string) ->
              ('b -> string) ->
                object
                  constraint 'inh = unit
                  method  c_Nil : unit -> string
                  method  c_Cons : unit -> 'a -> 'b -> string
                end
        val show_alist :
          ('a -> string) -> ('b -> string) -> ('a, 'b) alist -> string
        class ['a,'a_2,'b,'b_2,'extra] gmap_alist :
          (('a, 'b) alist -> ('a_2, 'b_2) alist) ->
            ('a -> 'a_2) ->
              ('b -> 'b_2) ->
                object
                  method  c_Nil : unit -> ('a_2, 'b_2) alist
                  method  c_Cons : unit -> 'a -> 'b -> ('a_2, 'b_2) alist
                end
        val gmap_alist :
          ('a -> 'a_2) ->
            ('b -> 'b_2) -> ('a, 'b) alist -> ('a_2, 'b_2) alist
        class ['a,'b,'syn] foldl_alist :
          ('syn -> ('a, 'b) alist -> 'syn) ->
            ('syn -> 'a -> 'syn) ->
              ('syn -> 'b -> 'syn) ->
                object
                  method  c_Nil : 'syn -> 'syn
                  method  c_Cons : 'syn -> 'a -> 'b -> 'syn
                end
        val foldl_alist :
          ('syn -> 'a -> 'syn) ->
            ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) alist -> 'syn
        module type MT_alist  =
          sig
            val gcata :
              < c_Nil: 'inh -> 'syn  ;c_Cons: 'inh -> 'a -> 'b -> 'syn   ;..
                >  -> 'inh -> ('a, 'b) alist -> 'syn
            val show :
              ('a -> string) -> ('b -> string) -> ('a, 'b) alist -> string
            val gmap :
              ('a -> 'a_2) ->
                ('b -> 'b_2) -> ('a, 'b) alist -> ('a_2, 'b_2) alist
            val foldl :
              ('syn -> 'a -> 'syn) ->
                ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) alist -> 'syn
          end
        val alist : (module MT_alist)
      end
  end =
  struct
    type ('a, 'b) alist =
      | Nil
      | Cons of 'a * 'b
    (* [@@deriving gt ~gmap ~show ~foldl] *)
    let _ = fun (_ : ('a, 'b) alist) -> ()
    class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'extra] class_alist =
      object
        method virtual  c_Nil : 'inh -> 'syn
        method virtual  c_Cons : 'inh -> 'a -> 'b -> 'syn
      end
    let gcata_alist tr inh t =
      match t with
      | Nil -> tr#c_Nil inh
      | Cons (_a, _b) -> tr#c_Cons inh _a _b
    let _ = gcata_alist
    class ['a,'b,'extra] show_alist _fself  fa  fb =
      object
        inherit  ['a,unit,string,'b,unit,string,'inh,string,'extra]
          class_alist
        constraint 'inh = unit
        method c_Nil () = "Nil"
        method c_Cons () _a _b =
          Format.sprintf "Cons(%s, %s)" (fa _a) (fb _b)
      end
    let rec show_alist fa fb subj =
      (GT.fix0 (fun self -> gcata_alist ((new show_alist) self fa fb))) ()
        subj
    let _ = show_alist
    class ['a,'a_2,'b,'b_2,'extra] gmap_alist _fself  fa  fb =
      object
        inherit  ['a,unit,'a_2,'b,unit,'b_2,'inh,('a_2, 'b_2) alist,'extra]
          class_alist
        method c_Nil () = Nil
        method c_Cons () _a _b = Cons ((fa _a), (fb _b))
      end
    let rec gmap_alist fa fb subj =
      (GT.fix0 (fun self -> gcata_alist ((new gmap_alist) self fa fb))) ()
        subj
    let _ = gmap_alist
    class ['a,'b,'syn] foldl_alist _fself  fa  fb =
      object
        inherit  ['a,'syn,'syn,'b,'syn,'syn,'inh,'syn,'extra] class_alist
        method c_Nil inh = inh
        method c_Cons inh _a _b = fb (fa inh _a) _b
      end
    let rec foldl_alist fa fb the_init subj =
      (GT.fix0 (fun self -> gcata_alist ((new foldl_alist) self fa fb)))
        the_init subj
    let _ = foldl_alist
    module type MT_alist  =
      sig
        val gcata :
          < c_Nil: 'inh -> 'syn  ;c_Cons: 'inh -> 'a -> 'b -> 'syn   ;.. >
            -> 'inh -> ('a, 'b) alist -> 'syn
        val show :
          ('a -> string) -> ('b -> string) -> ('a, 'b) alist -> string
        val gmap :
          ('a -> 'a_2) ->
            ('b -> 'b_2) -> ('a, 'b) alist -> ('a_2, 'b_2) alist
        val foldl :
          ('syn -> 'a -> 'syn) ->
            ('syn -> 'b -> 'syn) -> 'syn -> ('a, 'b) alist -> 'syn
      end
    let alist = ((module
      struct
        let gcata = gcata_alist
        let _ = gcata
        let show = show_alist
        let _ = show
        let gmap = gmap_alist
        let _ = gmap
        let foldl = foldl_alist
        let _ = foldl
      end) : (module MT_alist))
    let _ = alist
  end


let () =
  let open AL in
  let sh xs = show_alist id id xs in
  (* let fo xs = foldl_alist (fun () -> id) (fun () -> id) "" xs in *)
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", "bbb"));
  (* Printf.printf "%s\n%!" (fo @@ Cons ("aaa", "bbb")); *)
  ()

module L : sig
  type 'a list = ('a, 'a list) AL.alist
  [@@deriving gt ~gmap ~show ~foldl ]

end = struct
  type 'a list = ('a, 'a list) AL.alist
  (* [@@deriving gt ~gmap ~show ~foldl ] *)
    let _ = fun (_ : 'a list) -> ()
    class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_list =
      object
        inherit  ['a,'ia,'sa,'a list,'ia list,'sa list,'inh,'syn,'extra]
          AL.class_alist
      end
    let gcata_list = AL.gcata_alist
    let _ = gcata_list
    class ['a,'extra] show_list _fself  fa =
      object
        inherit  ['a,unit,string,'inh,string,'extra] class_list
        constraint 'inh = unit
        inherit  ((['a,'a list,'extra] AL.show_alist) _fself fa _fself)
      end
    (* let (_:int) = new show_list *)
    let rec show_list fa subj =
      (GT.fix0 (fun self -> gcata_list ((new show_list) self fa))) () subj
    let _ = show_list
    class ['a,'a_2,'extra] gmap_list _fself  fa =
      object
        inherit  ['a,unit,'a_2,'inh,'a_2 list,'extra] class_list
        inherit  ((['a,'a_2,'a list,'a_2 list,'extra] AL.gmap_alist) _fself
          fa _fself)
      end
    let rec gmap_list fa subj =
      (GT.fix0 (fun self -> gcata_list ((new gmap_list) self fa))) () subj
    let _ = gmap_list
    class ['a,'syn] foldl_list _fself  fa =
      object
        inherit  ['a,'syn,'syn,'inh,'syn,'extra] class_list
        inherit  ((['a,'a list,'syn] AL.foldl_alist) _fself fa _fself)
      end
    let rec foldl_list fa the_init subj =
      (GT.fix0 (fun self -> gcata_list ((new foldl_list) self fa))) the_init
        subj
    let _ = foldl_list
    module type MT_list  =
      sig
        val gcata :
          ('a,_,'sa,'inh,'syn,_)#class_list -> 'inh -> 'a list -> 'syn
        val show : ('a -> string) -> 'a list -> string
        val gmap : ('a -> 'a_2) -> 'a list -> 'a_2 list
        val foldl : ('syn -> 'a -> 'syn) -> 'syn -> 'a list -> 'syn
      end
    let list = ((module
      struct
        let gcata = gcata_list
        let _ = gcata
        let show = show_list
        let _ = show
        let gmap = gmap_list
        let _ = gmap
        let foldl = foldl_list
        let _ = foldl
      end) : (module MT_list))
    let _ = list

end

let () =
  let open L in
  let sh x = show_list id x in
  Printf.printf "%s\n%!" (sh @@ Cons ("aaa", Cons ("bbb", Nil)))

(* module Lo : sig
 *   type 'a logic = Var of GT.int | Value of 'a
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end = struct
 *   type 'a logic = Var of GT.int | Value of 'a
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end
 *
 * let () =
 *   let open Lo in
 *   let sh x = show_logic id x in
 *   Printf.printf "%s\t%s\n%!" (sh @@ Var 5) (sh @@ Value "asdf")
 *
 * module LList : sig
 *   type 'a llist = ('a, 'a llist) AL.alist Lo.logic
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end = struct
 *   type 'a llist = ('a, 'a llist) AL.alist Lo.logic
 *   [@@deriving gt ~gmap ~show ~foldl ]
 * end
 *
 * let () =
 *   let sh x = LList.show_llist id x in
 *   Printf.printf "%s\n%!" (sh @@ Value (Cons ("aaa", Value (Cons ("bbb", Var 15)))) ) *)
