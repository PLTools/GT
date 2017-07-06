open Printf

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
      [ < a: 'ia -> 'a -> 'sa >  as 'tpoT
      , 'a gnat
      , ('ia,'a,'sa,'tpoT) GT.a
      , 'inh,'syn] gnat_meta_tt
    method  t_gnat : ('ia -> 'a -> 'sa) -> 'inh -> 'a gnat -> 'syn
  end

let rec gnat_meta_gcata fa tpo trans initial_inh subj =
  let self = gnat_meta_gcata fa tpo trans in
  match subj with
  | O  -> trans#c_O initial_inh (GT.make self subj tpo)
  | S p0 -> trans#c_S initial_inh (GT.make self subj tpo) (fa p0)

let gnat_gcata fa transformer initial_inh subj =
  let parameter_transforms_obj = object method a = fa end  in
  gnat_meta_gcata
    (fun x  -> GT.make fa x parameter_transforms_obj)
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
      [ < a: unit -> 'a -> string >  as 'tpoT
      , 'a, (unit,'a,string,'tpoT) GT.a]
      show_meta_gnat (fun pa  -> pa.GT.fx ())
  end
let gnat =
  {
    GT.gcata = gnat_gcata;
    GT.plugins =
      (object method show fa = gnat_gcata (GT.lift fa) (new show_gnat) () end)
  }

(* let rec show_nat n = gnat_meta_gcata (fun x -> x) (object end) (new show_meta_gnat show_nat) () n *)


(* *************************************************************** *)

type nat = nat gnat

let nat_meta_gcata x = gnat_meta_gcata (fun x -> x) x
let nat_gcata transformer initial_inh subj =
  let parameter_transforms_obj = object end  in
  nat_meta_gcata parameter_transforms_obj transformer initial_inh subj

(* class virtual [ 'tpoT
              , 'type_itself
              , 'inh, 'syn ] nat_meta_t = object
  inherit [ 'tpoT, 'type_itself, nat
          , 'inh, 'syn] gnat_meta_t
end
class virtual ['inh, 'syn] nat_t
  : [ <  > as 'heck, nat, 'inh, 'syn] nat_meta_t =
  object (this)
    inherit ['heck, nat, 'inh, 'syn] nat_meta_t
end *)

class ['heck] show_meta_nat =
  let rec for_a n = nat_gcata (new show_meta_gnat for_a) () n in
  object
    inherit [ 'heck, nat, nat ] show_meta_gnat for_a
end

class show_nat = object(this)
  inherit [ <  > as 'tpoT ] show_meta_nat

  method t_nat x = nat_gcata this x
end

let () =
  let show n = nat_gcata (new show_nat) () n in
  printf "%s\n%s\n%s\n%!" (show O)  (show (S O)) (show (S(S O)));
  ()
