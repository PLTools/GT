open Printf
(*
type 'a gnat = O | S of 'a
(* [@@deriving gt {show}] *)

class type virtual ['tpoT,'type_itself,'gt_a_for_a,'inh,'syn] gnat_meta_tt =
  object
    method  virtual c_O : 'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> 'syn
    method  virtual c_S :
      'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> 'gt_a_for_a -> 'syn
  end
class type virtual ['a,'ia,'sa,'inh,'syn] gnat_tt =
  object
    inherit
      [ < a: 'ia -> 'a -> 'sa   >  as 'tpoT,'a gnat,('ia,'a,'sa,'tpoT) GT.a,
      'inh,'syn] gnat_meta_tt
    method  t_gnat : ('ia -> 'a -> 'sa) -> 'inh -> 'a gnat -> 'syn
  end
let rec gnat_meta_gcata fa tpo trans initial_inh subj =
  let self = gnat_meta_gcata fa tpo trans  in
  match subj with
  | O  -> trans#c_O initial_inh (GT.make self subj tpo)
  | S p0 -> trans#c_S initial_inh (GT.make self subj tpo) (fa p0)
let gnat_gcata fa transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa end  in
  gnat_meta_gcata (fun x  -> GT.make fa x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj

class virtual ['tpoT,'type_itself,'gt_a_for_a,'inh,'syn] gnat_meta_t =
  object (self : 'self)
    constraint 'self =
      ('tpoT,'type_itself,'gt_a_for_a,'inh,'syn)#gnat_meta_tt
  end
class virtual ['tpoT,'a,'ia,'sa,'gt_a_for_a,'inh,'syn] gnat_t =
  object (this) inherit  ['tpoT,'a gnat,'gt_a_for_a,'inh,'syn] gnat_meta_t
  end
class ['tpoT,'a,'a_holder] show_meta_gnat for_a =
  object (this)
    inherit  ['tpoT,'a,unit,string,'a_holder,unit,string] gnat_t
    method c_S inh subj (p0 : 'a_holder) = "S (" ^ ((for_a p0) ^ ")")
    method c_O inh subj = "O ()"
  end
class ['a] show_gnat =
  object
    inherit
      [ < a: unit -> 'a -> string   >  as 'tpoT
      , 'a, (unit,'a,string,'tpoT) GT.a]
      show_meta_gnat (fun pa  -> pa.GT.fx ())
  end
let gnat =
  {
    GT.gcata = gnat_gcata;
    GT.plugins =
      (object method show fa = gnat_gcata (GT.lift fa) (new show_gnat) () end)
  }

let rec show_nat n = gnat_meta_gcata (fun x -> x) (object end) (new show_meta_gnat show_nat) () n

type nat = nat gnat

let nat_meta_gcata x = gnat_meta_gcata (fun x -> x) x
let nat_gcata transformer initial_inh subj =
  let parameter_transforms_obj = object end  in
  nat_meta_gcata parameter_transforms_obj transformer initial_inh subj

class virtual [ 'heck
              , 'type_itself
              , 'inh, 'syn ] nat_meta_t = object
  inherit [ 'heck, 'type_itself, nat
          , 'inh, 'syn] gnat_meta_t
end
class virtual ['inh, 'syn] nat_t
  : [ <  > as 'heck, nat, 'inh, 'syn] nat_meta_t =
  object (this)
    inherit ['heck, nat, 'inh, 'syn] nat_meta_t
end

class ['heck] show_meta_nat =
  let rec for_a n = nat_gcata (new show_meta_gnat for_a) () n in
  object
    inherit [ 'heck, nat, nat ] show_meta_gnat for_a
end

class show_nat = object(this)
  inherit [ <  > as 'heck] show_meta_nat

  method t_nat x = nat_gcata this x
end

let () =
  let show n = nat_gcata (new show_nat) () n in
  printf "%s\n%s\n%s\n%!" (show O)  (show (S O)) (show (S(S O)));
  () *)

type ('a, 'b) glist = Nil | Cons of 'a * 'b
 (* [@@deriving gt {show}] *)

class type virtual
   ['tpoT,'type_itself,'gt_a_for_a,'gt_a_for_b,'inh,'syn] glist_meta_tt =
   object
     method  virtual c_Nil :
       'inh -> ('inh,'type_itself,'syn,'tpoT) GT.a -> 'syn
     method  virtual c_Cons :
       'inh ->
         ('inh,'type_itself,'syn,'tpoT) GT.a ->
           'gt_a_for_a -> 'gt_a_for_b -> 'syn
   end
class type virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] glist_tt =
  object
    inherit
      [ < a: 'ia -> 'a -> 'sa; b: 'ib -> 'b -> 'sb >  as 'tpoT
      , ('a, 'b) glist
      , ('ia,'a,'sa,'tpoT) GT.a
      , ('ib,'b,'sb,'tpoT) GT.a
      , 'inh,'syn ] glist_meta_tt
     method  t_glist :
       ('ia -> 'a -> 'sa) ->
         ('ib -> 'b -> 'sb) -> 'inh -> ('a,'b) glist -> 'syn
  end

let rec glist_meta_gcata fa fb tpo trans initial_inh subj =
   let self = glist_meta_gcata fa fb tpo trans in
   match subj with
   | Nil  -> trans#c_Nil initial_inh (GT.make self subj tpo)
   | Cons (p0,p1) ->
       trans#c_Cons initial_inh (GT.make self subj tpo) (fa p0) (fb p1)

let glist_gcata fa fb transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa method b = fb end  in
  glist_meta_gcata (fun x  -> GT.make fa x parameter_transforms_obj)
    (fun x  -> GT.make fb x parameter_transforms_obj)
    parameter_transforms_obj transformer initial_inh subj

class virtual ['tpoT,'type_itself,'gt_a_for_a,'gt_a_for_b,'inh,'syn] glist_meta_t =
  object (self : 'self)
    constraint 'self =
      ('tpoT,'type_itself,'gt_a_for_a,'gt_a_for_b,'inh,'syn) #glist_meta_tt
end
class virtual ['tpoT ,'a,'ia,'sa,'gt_a_for_a ,'b,'ib,'sb,'gt_a_for_b ,'inh,'syn] glist_t =
  object
    inherit ['tpoT, ('a,'b)glist ,'gt_a_for_a,'gt_a_for_b,'inh,'syn] glist_meta_t
end

class ['tpoT,'a,'a_holder,'b,'b_holder] show_meta_glist for_a for_b =
  object (this)
    inherit
      ['tpoT
      ,'a,unit,string,'a_holder
      ,'b,unit,string,'b_holder
      ,unit,string ] glist_t
     method c_Cons inh subj (p0 : 'a_holder) (p1 : 'b_holder) =
       "Cons (" ^ ((String.concat ", " [for_a p0; for_b p1]) ^ ")")
     method c_Nil inh subj = "Nil ()"
   end

class ['a,'b] show_glist = object
  inherit
    [ < a: unit -> 'a -> string; b: unit -> 'b -> string   >  as 'tpoT
    , 'a,(unit,'a,string,'tpoT) GT.a
    , 'b,(unit,'b,string,'tpoT) GT.a ]
      show_meta_glist (fun pa  -> pa.GT.fx ()) (fun pb -> pb.GT.fx ())
end


(* *************************************************************** *)
type 'a list = ('a, 'a list) glist

let list_meta_gcata fa = glist_meta_gcata fa (fun x -> x)
let list_gcata fa transformer initial_inh (subj: 'a list) =
  let parameter_transforms_obj = object method a = fa end  in
  list_meta_gcata fa parameter_transforms_obj transformer initial_inh subj

class virtual
  ['tpoT,'type_itself
  ,'gt_a_for_a
  ,'inh,'syn] list_meta_t = object
   (* (self : 'self) *)
    inherit ['tpoT,'type_itself,'gt_a_for_a,'type_itself,'inh,'syn] glist_meta_t
 end

class virtual
  ['tpoT,'a,'ia,'sa,'gt_a_for_a,'inh,'syn] list_t =
  object (this)
    inherit  ['tpoT, 'a list, 'gt_a_for_a, 'inh, 'syn]
      list_meta_t
end


let rec for_b : _ -> 'a list -> _ = fun for_a n ->
  (* list_gcata for_a (new show_meta_glist for_a (for_b for_a) ) () n *)
  (* let foo : ('a, 'b, 'c, 'd, 'e) show_meta_glist = (new show_meta_glist for_a (fun _ -> assert false) ) in *)
  list_gcata for_a (new show_meta_glist for_a (fun _ -> assert false) ) () n


class ['tpoT,'a,'a_holder] show_meta_list for_a =
  (* let rec for_b n = list_gcata (new show_meta_glist for_a for_b) () n in *)
  object
     inherit [ 'tpoT, 'a, 'a_holder, 'a list, 'a list ] show_meta_glist for_a
       (fun _ -> assert false)
      (* (for_b for_a) *)
end

class ['a] show_list = object(this)
  inherit [ < a: unit -> 'a -> string > as 'tpoT
          , 'a
          , (unit, 'a, string, 'tpoT) GT.a
          ] show_meta_list  (fun pa -> pa.GT.fx ())

  method t_list transform_a = list_gcata transform_a this
end

let glist = {
  GT.gcata = glist_gcata;
  GT.plugins =
     (object
        method show fa fb =
          glist_gcata (GT.lift fa) (GT.lift fb) (new show_glist) ()
      end)
}

let list = {
  GT.gcata = list_gcata;
  GT.plugins =
     (object
        method show fa = list_gcata (GT.lift fa) (new show_list fa) ()
      end)
}
