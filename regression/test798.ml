(* There you can find manual implementation of the code to be derived from
      type ('a,'b) glist = Nil | Cons of 'a * 'b
  and
      type 'a list = ('a, 'a list) glist
*)

open Printf
let (!!!) = Obj.magic
(*
thanks to def`

# class ['self] b = fun (f : 'self -> 'a) -> object end;;
class ['self] b : ('self -> 'a) -> object end
# class virtual a = object (this : 'self)
    inherit['self] b (fun this -> this#foo)
    method virtual foo : int
  end;;
class virtual a : object method virtual foo : int end
*)

type ('a, 'b) glist = Nil | Cons of 'a * 'b
(* [@@deriving gt {show}] *)

(* gcata goes in the beginnigbecause it doesn't depend on anything *)
let rec glist_meta_gcata fa fb (tpo: 'tpoT) trans (initial_inh: 'inh) subj : 'syn =
  let self = glist_meta_gcata fa fb tpo trans in
   match subj with
   | Nil  -> trans#c_Nil initial_inh (GT.make self subj tpo)
   | Cons (p0,p1) ->
       trans#c_Cons initial_inh (GT.make self subj tpo) (fa p0) (fb p1)

let glist_gcata fa fb transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa method b = fb end  in
  glist_meta_gcata
    (fun x  -> GT.make fa x parameter_transforms_obj)
    (fun x  -> GT.make fb x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj

class type virtual
   ['inh,'syn,'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a,'gt_a_for_b] glist_meta_tt =
   object
     method  virtual c_Nil  : 'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> 'syn
     method  virtual c_Cons : 'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a ->
           'gt_a_for_a -> 'gt_a_for_b -> 'syn
   end
class type virtual ['inh,'syn, 'a,'ia,'sa,'b,'ib,'sb] glist_tt =
  object
    inherit
      [ 'inh, 'syn
      , < a: 'ia -> 'a -> 'sa; b: 'ib -> 'b -> 'sb >  as 'tpoT
      , ('a, 'b) glist, 'gt_a_for_self
      , ('ia,'a,'sa,'tpoT) GT.a
      , ('ib,'b,'sb,'tpoT) GT.a
      ] glist_meta_tt
      method  t_glist :
        ('ia -> 'a -> 'sa) ->
        ('ib -> 'b -> 'sb) -> 'inh -> ('a,'b) glist -> 'syn
  end

class virtual ['inh,'syn,'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a,'gt_a_for_b] glist_meta_t =
  object (self : 'self)
    constraint 'self =
      ('inh,'syn, 'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a,'gt_a_for_b) #glist_meta_tt
end
class virtual [ 'inh,'syn, 'tpoT
  ,'a,'ia,'sa,'gt_a_for_a
  ,'b,'ib,'sb,'gt_a_for_b
  ] glist_t =
  object
    inherit ['inh,'syn, 'tpoT
            , ('a,'b) glist, ('a,'b) glist        (* ???? *)
            ,'gt_a_for_a,'gt_a_for_b] glist_meta_t
end

class ['tpoT,'a,'a_holder,'b,'b_holder,'self_holder] show_meta_glist
    (for_a: unit -> 'a_holder -> string)
    (for_b: unit -> 'b_holder -> string)
    for_me
  =
  object (this)
    inherit
      [ unit,string, 'tpoT
      , 'a,unit,string,'a_holder
      , 'b,unit,string,'b_holder
      ] glist_t
     method c_Cons inh subj (p0 : 'a_holder) (p1 : 'b_holder) =
       sprintf "Cons (%s,%s)" (for_a inh p0) (for_b inh p1)
     method c_Nil inh subj = "Nil ()"
   end

class ['a,'b] show_glist for_me = object
  inherit
    [ < a: unit -> 'a -> string; b: unit -> 'b -> string >  as 'tpoT
    , 'a,(unit,'a,string,'tpoT) GT.a
    , 'b,(unit,'b,string,'tpoT) GT.a
    , ('a, 'b) glist ]
      show_meta_glist (fun () pa  -> pa.GT.fx ()) (fun () pb -> pb.GT.fx ()) for_me
end

let glist = {
  GT.gcata = glist_gcata;
  GT.plugins =
     (object (self)
        method show fa fb xs =
          glist_gcata (fa) ( fb) (new show_glist @@ self#show fa fb) () xs
      end)
}

let () =
  let rec show fa xs =
    glist.GT.plugins#show fa (fun () -> show fa)  xs
    (* glist_gcata (GT.lift fa) (GT.lift @@ show fa) (new show_glist) () xs *)
    in
  let soi = fun () -> string_of_int in
  printf "%s\n%!" (show soi (Nil));
  printf "%s\n%!" (show soi (Cons (2, Nil)));
  printf "%s\n%!" (show soi (Cons (2, Cons (2, Nil))));
()


(* *************************************************************** *)
type 'a list = ('a, 'a list) glist

let list_meta_gcata fa x = glist_meta_gcata fa (fun x -> x) x

let list_gcata fa transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa end  in
  list_meta_gcata
    (* fa *)
    (fun x  -> GT.make fa x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj
(* ????? *)

class virtual
  ['inh,'syn, 'tpoT
  ,'type_itself,'gt_a_for_self
  ,'gt_a_for_a
  ] list_meta_t = object
    inherit [ 'inh,'syn, 'tpoT,'type_itself,'gt_a_for_self, 'gt_a_for_a,'gt_a_for_self] glist_meta_t
end

(* class virtual
  ['tpoT,'a,'ia,'sa,'gt_a_for_a,'inh,'syn] list_t =
  object (this)
    inherit  ['tpoT, 'a list, 'gt_a_for_a, 'inh, 'syn]
      list_meta_t
end *)

class virtual ['tpoT,'a,'a_holder, 'self_holder] show_meta_list for_a for_me =

  (* let rec for_b = function
  | Nil -> "Nil ()"
  | Cons (p0,p1) -> "Cons (" ^ ((for_a p0) ^ ", " ^ (for_b p1) ^ ")")
  in *)

  object (this)
    inherit ['tpoT, 'a, 'a_holder, 'a list, 'self_holder, 'a_holder list] show_meta_glist
      for_a
      for_me for_me
end

class ['a, 'self_holder] show_list for_me = object(this)
  inherit [ < a: unit -> 'a -> string > as 'tpoT
          , 'a
          , (unit, 'a, string, 'tpoT) GT.a
          , 'self_holder
          ] show_meta_list (fun () pa -> pa.GT.fx ()) for_me

  (* method t_list transform_a x = list_gcata transform_a this x *)
end


let list = {
  GT.gcata = list_gcata;
  GT.plugins =
     (object(self)
        method show fa () = list_gcata (GT.lift fa) (new show_list (self#show fa)) ()
      end)
}


let () =
  let rec show fa () xs =
    list_gcata (GT.lift fa) (new show_list (show fa)) () xs
  in
  printf "%s\n%!" (show string_of_int () (Nil));
  printf "%s\n%!" (show (fun x -> x)  () (Cons ("FUCK", Nil)));
  printf "%s\n%!" (show string_of_int () (Cons (1, Cons (1, Nil))));
  ()

(* *************************************************************** *)
(* We continue shrinking the type but it seems that 'self_holder is still needed.
 * Or not? Maybe when type is monomorphic we don't need that self_holder and can
 * avoid it? It will make generated code a litlle bit shorter but improvement seems not
 * to be visible for the end-users.
 *)
type intlist = int list

let intlist_meta_gcata x = list_meta_gcata (fun x -> x) x

let intlist_gcata transformer initial_inh subj =
  let parameter_transforms_obj = object end  in
  intlist_meta_gcata parameter_transforms_obj transformer initial_inh subj

class virtual [ 'inh,'syn, 'tpoT,'type_itself, 'gt_a_for_self ] intlist_meta_t = object
  inherit ['tpoT,'type_itself,'gt_a_for_self, int, 'inh,'syn] list_meta_t
end

class virtual [ 'inh,'syn, 'tpoT,'gt_a_for_self] intlist_t = object
  inherit [ 'inh,'syn, 'tpoT, intlist, 'gt_a_for_self] intlist_meta_t
end

class virtual ['tpoT,'self_holder] show_meta_intlist for_me =
  let for_a () = GT.lift (GT.int.GT.plugins)#show () in
  object (this)
    inherit ['tpoT, int, int, 'self_holder] show_meta_list for_a for_me
end

class ['a, 'self_holder] show_intlist for_me = object(this)
  inherit [ <  > as 'tpoT
          , intlist   (* maybe 'self_holder here *)
          ] show_meta_intlist for_me
end


let () =
  let show xs =
    let rec helper () xs = intlist_gcata (new show_intlist helper) () xs in
    helper () xs
  in
  printf "%s\n%!" (show  Nil);
  printf "%s\n%!" (show  (Cons (6, Nil)));
  printf "%s\n%!" (show  (Cons (7, Cons (8, Nil))));
  ()

(* and now we will try to define logic list *)

type 'a logic = Value of 'a | Var of int
(* [@@deriving gt { show } ] *)

let rec logic_meta_gcata fa tpo trans initial_inh subj =
  let self = logic_meta_gcata fa tpo trans  in
  match subj with
  | Value p0 -> trans#c_Value initial_inh (GT.make self subj tpo) (fa p0)
  | Var p0 -> trans#c_Var initial_inh (GT.make self subj tpo) p0
let logic_gcata fa transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa end  in
  logic_meta_gcata (fun x  -> GT.make fa x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj

class type virtual
  ['inh,'syn,'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a] logic_meta_tt =
  object
    method  virtual c_Value :
      'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> 'gt_a_for_a -> 'syn
    method  virtual c_Var :
      'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> int -> 'syn
  end
class virtual
  ['inh,'syn,'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a] logic_meta_t =
  object (self : 'self)
    constraint 'self =
      ('inh,'syn,'tpoT,'type_itself,'gt_a_for_self,'gt_a_for_a)#logic_meta_tt
  end
class virtual ['inh,'syn,'tpoT,'a,'ia,'sa,'gt_a_for_a] logic_t =
  object (this)
    inherit  ['inh,'syn,'tpoT,'a logic,'a logic,'gt_a_for_a] logic_meta_t
  end

class ['tpoT,'a,'a_holder,'self_holder] show_meta_logic
  (for_a: (unit as 'inh) -> 'a_holder -> (string as 'syn))
  (for_me: 'inh -> 'self_holder -> 'syn) =
  object (this)
    inherit  ['inh,'syn,'tpoT, 'a,'inh,'syn,'a_holder] logic_t
    method c_Var fmt subj p0 =
      Format.asprintf "Var (%s)" ((GT.lift (GT.int.GT.plugins)#show ()) p0)
    method c_Value fmt subj (p0 : 'a_holder) =
      Format.asprintf "Value (%s)" (for_a () p0)
  end
class ['a] show_logic for_me =
  object
    inherit
      [ < a: (unit as 'inh) -> 'a -> (string as 'syn) >  as 'tpoT
      , 'a, ('inh,'a,'syn,'tpoT) GT.a
      ,'a logic]
      show_meta_logic (fun () pa -> pa.GT.fx ()) for_me
  end
let logic =
  { GT.gcata = logic_gcata
  ; GT.plugins = object (self)
        method show fa () subj =
          logic_gcata fa (new show_logic (self#show fa)) () subj
      end
  }

let () =
  let rec showF fa fmt xs = logic.GT.plugins#show fa fmt xs in
  Printf.printf  "%s\n%!" @@ showF (fun () -> Printf.sprintf "%d") () (Var 5);
  Printf.printf  "%s\n%!" @@ showF (fun () -> Printf.sprintf "%s") () (Value "asdf");
  ()

type 'a llist = ('a, 'a llist) glist logic

let rec llist_meta_gcata fa tpo trans initial_inh subj =
  let self = logic_meta_gcata fa tpo trans  in
  match subj with
  | Value p0 -> trans#c_Value initial_inh (GT.make self subj tpo) (fa p0)
  | Var p0 -> trans#c_Var initial_inh (GT.make self subj tpo) p0
let llist_gcata fa transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa end  in
  llist_meta_gcata (fun x  -> GT.make fa x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj

(* let (_: unit -> _ -> _ -> _ -> string) = fun () pa this for_me ->
  logic.GT.plugins#show
      (glist_gcata pa this (new show_meta_glist for_me) ()) () *)

let assF _ = assert false

class ['tpoT,'a,'a_holder,'self_holder] show_meta_llist
  (for_a: (unit as 'inh) -> 'a_holder -> (string as 'syn))
  (for_me: 'inh -> 'self_holder -> 'syn)
  (for_me_long: 'some_holder -> 'inh -> 'some_holder llist -> 'syn)
  =
  (* let (_:int) = glist.gcata for_a in
  let (_:int -> int) = fun x -> GT.lift (logic.GT.plugins#show x) in *)

  let self = Obj.magic (ref ())  in
  object (this)
  initializer self := (this :> (('x1,'x2,'x3,'x4) show_meta_llist))

  inherit [ < a: unit -> 'a -> string > as 'tpoT
          , 'a
          , (unit, 'a, string, 'tpoT) GT.a
          , 'self_holder
          ] show_meta_logic
          (fun () pb ->
            (* sholud map values of type
                   ('a, 'b logic) glist as 'b
            *)
            (* let rec show1 () xs = glist_gcata for_a
              (fun inh x ->
                  logic_gcata show1 (Obj.magic !self) inh x)
              (new show_meta_glist assF assF assF) () xs
            in
            logic.GT.plugins#show show1 () pa.GT.x *)

            (* let (_:int) = glist.GT.plugins#show in *)
            (* let rec show1 () xs = glist_gcata for_a
              (assF)
              (new show_meta_glist assF assF assF) () xs
            in
            (* glist.GT.plugins#show for_a for_me pa.GT.x *)
            glist.GT.plugins#show
              for_a
              (logic.GT.plugins#show show1)
              (* !!!(logic_gcata for_a !self) *)
              pb.GT.x *)

            (* let rec for_b () gs =
              glist.GT.plugins#show for_a for_logic_b gs
            and for_logic_b () l =
              logic.GT.plugins#show for_b () l
            in
            for_b () pb.GT.x *)
            (* There are probles siwth using this here because we can't use this in the inherit
              construction. Except maybe def's hack
            *)
          )
          for_me

(*
(GT.lift
   (logic.GT.plugins#show @@
     GT.lift
         (glist.GT.plugins#show
             (subj.GT.t#a ())
             (GT.transform xxx subj.GT.t#a this ())
         )
         ()
   )
   ()
   p0)
*)
end

class ['a] show_llist for_me =
  object
    inherit
      [ < a: (unit as 'inh) -> 'a -> (string as 'syn) >  as 'tpoT
      , 'a, ('inh,'a,'syn,'tpoT) GT.a
      ,'a logic]
      show_meta_llist assF assF assF
      (* show_meta_llist (fun () pa -> pa.GT.fx ()) for_me assF *)
  end

let llist =
  { GT.gcata = logic_gcata
  ; GT.plugins = object (self)
        method show fa subj =
          llist_gcata fa (new show_llist (fun () -> assF)) () subj
      end
  }

let () =
  let rec show fa xs = llist.GT.plugins#show fa xs in
  (* Printf.printf  "%s\n%!" @@ show (fun () -> Printf.sprintf "%d") () (Var 5); *)
  Printf.printf  "%s\n%!" @@ show
    (fun () -> Printf.sprintf "%s") (Value Nil);
  ()
