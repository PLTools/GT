type ('a,'l) glist =
  | Nil
  | Cons of 'a * 'l
class type virtual ['a,'ia,'sa,'l,'il,'sl,'inh,'syn] glist_tt =
  object
    method  c_Nil :
      'inh ->
        ('inh,('a,'l) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl
                                   > )
          GT.a -> 'syn
    method  c_Cons :
      'inh ->
        ('inh,('a,'l) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl
                                   > )
          GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl   > ) GT.a
            ->
            ('il,'l,'sl,< a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl   > )
              GT.a -> 'syn
    method  t_glist :
      ('ia -> 'a -> 'sa) ->
        ('il -> 'l -> 'sl) -> 'inh -> ('a,'l) glist -> 'syn
  end

let glist :
  (('ia -> 'a -> 'sa) ->
     ('il -> 'l -> 'sl) ->
       ('a,'ia,'sa,'l,'il,'sl,'inh,'syn) #glist_tt ->
         'inh -> ('a,'l) glist -> 'syn,unit)
    GT.t
  =
  let rec llist_gcata fa fl trans inh subj =
    let rec self = llist_gcata fa fl trans

    and tpo = object method a = fa method l = fl end
     in
    match subj with
    | Nil  -> trans#c_Nil inh (GT.make self subj tpo)
    | Cons (p0,p1) ->
        trans#c_Cons inh (GT.make self subj tpo) (GT.make fa p0 tpo)
          (GT.make fl p1 tpo)
     in
  { GT.gcata = llist_gcata; GT.plugins = () }

class virtual ['a,'ia,'sa,'l,'il,'sl,'inh,'syn] glist_t =
  object (this)
    method virtual  c_Nil :
      'inh ->
        ('inh,('a,'l) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl
                                   > )
          GT.a -> 'syn
    method virtual  c_Cons :
      'inh ->
        ('inh,('a,'l) glist,'syn,<
                                   a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl
                                   > )
          GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl   > ) GT.a
            ->
            ('il,'l,'sl,< a: 'ia -> 'a -> 'sa  ;l: 'il -> 'l -> 'sl   > )
              GT.a -> 'syn
    method t_glist fa fl = GT.transform glist fa fl this
  end

class type ['a,'l] show_glist_env_tt = object  end
class ['a,'l] show_proto_glist env =
  object (this)
    inherit  ['a,unit,string,'l,unit,string,unit,string] glist_t
    method c_Cons inh subj p0 p1 =
      (("Cons (" ^ (p0.GT.fx ())) ^ (", " ^ (p1.GT.fx ()))) ^ ")"
    method c_Nil inh subj = "Nil (" ^ ")"
  end

class ['a,'l] show_llist_t = let self = Obj.magic (ref ())  in
  object (this)
    inherit  ['a,unit,string,'l,unit,string,unit,string] glist_t
    inherit  ((['a,'l] show_proto_glist) self)
    initializer self := (this :> ('a,'l) show_llist_t)
  end

let glist : (('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) ->
   ('a,'ia,'sa,'l,'il,'sl,'inh,'syn)#glist_tt ->
     'inh -> ('a,'l) glist -> 'syn,<
                       show: ('a -> string) ->
                               ('l -> string) ->
                                 ('a,'l) glist -> string  ;
                                     > )
    GT.t
  =
  {
    GT.gcata = (glist.GT.gcata);
    GT.plugins =
      (object
         method show a l =
           GT.transform glist (GT.lift a) (GT.lift l) (new show_llist_t) ()
       end)
  }

(* ***************************************************************** *)
type 'a logic =
  | Var of GT.int * 'a logic GT.list
  | Value of 'a
;;
class type virtual ['a,'ia,'sa,'inh,'syn] logic_tt = object
    method  c_Var :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          GT.int -> 'a logic GT.list -> 'syn
    method  c_Value :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa   > ) GT.a -> 'syn
    method  t_logic : ('ia -> 'a -> 'sa) -> 'inh -> 'a logic -> 'syn
  end
let logic :
  (('ia -> 'a -> 'sa) ->
     ('a,'ia,'sa,'inh,'syn)#logic_tt -> 'inh -> 'a logic -> 'syn,unit)
    GT.t
  =
  let rec logic_gcata fa trans inh subj =
    let rec self = logic_gcata fa trans

    and tpo = object method a = fa end
     in
    match subj with
    | Var (p0,p1) -> trans#c_Var inh (GT.make self subj tpo) p0 p1
    | Value p0 ->
        trans#c_Value inh (GT.make self subj tpo) (GT.make fa p0 tpo)
     in
  { GT.gcata = logic_gcata; GT.plugins = () }

class virtual ['a,'ia,'sa,'inh,'syn] logic_t =
  object (this)
    method virtual  c_Var :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          GT.int -> 'a logic GT.list -> 'syn
    method virtual  c_Value :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa   > ) GT.a -> 'syn
    method t_logic fa = GT.transform logic fa this
  end
class type ['a] show_logic_env_tt = object  end
class ['a] show_proto_logic env =
  object (this)
    inherit  ['a,unit,string,unit,string] logic_t
    method c_Value inh subj p0 = ("Value (" ^ (p0.GT.fx ())) ^ ")"
    method c_Var inh subj p0 p1 =
      (("Var (" ^ (GT.lift (GT.int.GT.plugins)#show () p0)) ^
         (", " ^
            (GT.lift
               ((GT.list.GT.plugins)#show
                  (GT.transform logic (subj.GT.t)#a this ())) () p1)))
        ^ ")"
  end

class ['a] show_logic_t = let self = Obj.magic (ref ())  in
  object (this)
    inherit  ['a,unit,string,unit,string] logic_t
    inherit  ((['a] show_proto_logic) self)
    initializer self := (this :> 'a show_logic_t)
  end

let logic :
  (('ia -> 'a -> 'sa) ->
     ('a,'ia,'sa,'inh,'syn)#logic_tt -> 'inh -> 'a logic -> 'syn,
     < show: ('a -> string) ->'a logic -> string  > )
    GT.t
=
{
  GT.gcata = (logic.GT.gcata);
  GT.plugins = (object
    method show a = GT.transform logic (GT.lift a) (new show_logic_t) ()
  end)
}

type 'a xxx = Foo of ('a, 'a xxx) glist logic

class type virtual ['a,'ia,'sa,'inh,'syn] xxx_tt =
  object
    method  c_Foo :
      'inh ->
        ('inh,'a xxx,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('a,'a xxx) glist logic -> 'syn
    method  t_xxx : ('ia -> 'a -> 'sa) -> 'inh -> 'a xxx -> 'syn
  end
let (xxx :
  (('ia -> 'a -> 'sa) ->
     ('a,'ia,'sa,'inh,'syn)#xxx_tt -> 'inh -> 'a xxx -> 'syn,unit)
    GT.t)
  =
  let rec xxx_gcata fa trans inh subj =
    let rec self = xxx_gcata fa trans

    and tpo = object method a = fa end
     in
    match subj with | Foo p0 -> trans#c_Foo inh (GT.make self subj tpo) p0
     in
  { GT.gcata = xxx_gcata; GT.plugins = () }

class virtual ['a,'ia,'sa,'inh,'syn] xxx_t =
  object (this)
    method virtual  c_Foo :
      'inh ->
        ('inh,'a xxx,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('a,'a xxx) glist logic -> 'syn
    method t_xxx fa = GT.transform xxx fa this
  end
class type ['a] show_xxx_env_tt = object  end
class ['a] show_proto_xxx env =
  object (this)
    inherit  ['a,unit,string,unit,string] xxx_t
    method c_Foo inh subj p0 =
      Printf.sprintf "Foo (%s)"
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

  end
class ['a] show_xxx_t = let self = Obj.magic (ref ())  in
  object (this)
    inherit  ['a,unit,string,unit,string] xxx_t
    inherit  ((['a] show_proto_xxx) self)
    initializer self := (this :> 'a show_xxx_t)
  end

let xxx :
  (('ia -> 'a -> 'sa) ->
     ('a,'ia,'sa,'inh,'syn)#xxx_tt -> 'inh -> 'a xxx -> 'syn,
     < show: ('a -> string) -> 'a xxx -> string > )
    GT.t
  =
  {
    GT.gcata = (xxx.GT.gcata);
    GT.plugins =
      (object
         method show a = GT.transform xxx (GT.lift a) (new show_xxx_t) ()
       end)
  }

let () =
  print_endline @@ xxx.GT.plugins#show (fun s -> s) (Foo (Var (5,[])));
  print_endline @@ xxx.GT.plugins#show (fun s -> s)
    (Foo (Value (Cons ("asdf", Foo (Value Nil)))));
  ()
