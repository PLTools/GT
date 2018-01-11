open GT
(* type t = int [@@deriving gt] *)

(* type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt ~gmap ~show ]
 *
 * type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ]
 *
 * type 'a logic = Var of int | Value of 'a [@@deriving gt ~gmap ~show ]
 *
 * type 'a llist = ('a logic, 'b) alist logic as 'b [@@deriving gt ~gmap ~show ] *)


type ('a,'b) alist =
  | Nil
  | Cons of 'a * 'b
class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] class_alist =
  object
    method virtual  c_Nil : 'inh -> 'syn
    method virtual  c_Cons : 'inh -> 'a -> 'b -> 'syn
  end
let gcata_alist tr inh t =
  match t with | Nil  -> tr#c_Nil inh | Cons (a,b) -> tr#c_Cons inh a b
class ['a,'b] show_alist (show_alist as _fself)  fa  fb =
  object
    inherit  ['a,unit,string,'b,unit,string,'inh,'syn] class_alist
    method c_Nil () = "Nil"
    method c_Cons () a b = Format.sprintf "Cons(%s, %s)" (fa () a) (fb () b)
  end
let rec show_alist fa fb t =
  GT.fix0
    (fun self  ->
       gcata_alist ((new show_alist) self (fun ()  -> fa) (fun ()  -> fb)))
    () t

type 'a list = ('a,'a list) alist
class virtual ['a,'ia,'sa,'inh,'syn] class_list =
  object
    inherit  ['a,'ia,'sa,'a list,'ia list,'sa list,'inh,'syn] class_alist
  end
let gcata_list = gcata_alist
class ['a] show_list (show_list as _fself)  fa =
  object
    inherit  ['a,unit,string,'inh,'syn] class_list
    inherit  ((['a,'ia,'sa,'a list,'ia list,'sa list,unit,string] show_alist)
      (fa ()) (_fself ()))
  end
let rec show_list fa t =
  GT.fix0 (fun self  -> gcata_list ((new show_list) self (fun ()  -> fa))) ()
    t

type 'a logic =
  | Var of int
  | Value of 'a
class virtual ['a,'ia,'sa,'inh,'syn] class_logic =
  object
    method virtual  c_Var : 'inh -> int -> 'syn
    method virtual  c_Value : 'inh -> 'a -> 'syn
  end
let gcata_logic tr inh t =
  match t with | Var a -> tr#c_Var inh a | Value a -> tr#c_Value inh a
class ['a] show_logic (show_logic as _fself)  fa =
  object
    inherit  ['a,unit,string,'inh,'syn] class_logic
    method c_Var () a = Format.sprintf "Var(%s)" (string_of_int a)
    method c_Value () a = Format.sprintf "Value(%s)" (fa () a)
  end
let rec show_logic fa t =
  GT.fix0 (fun self  -> gcata_logic ((new show_logic) self (fun ()  -> fa)))
    () t

type 'a llist = ('a logic,'b) alist logic as 'b
class virtual ['a,'ia,'sa,'inh,'syn] class_llist =
  object
    inherit
      [('a logic,'a llist) alist,('ia logic,'ia llist) alist,('sa logic,
                                                               'sa llist)
                                                               alist,
      'inh,'syn] class_logic
  end
let gcata_llist = gcata_logic
class ['a] show_llist (show_llist as _fself)  fa =
  object
    inherit  ['a,unit,string,'inh,'syn] class_llist
    inherit
      (([('a logic,'a llist) alist,('ia logic,'ia llist) alist,('sa logic,
                                                                 'sa llist)
                                                                 alist,
      unit,string] show_logic) (show_alist (show_logic (fa ())) (_fself ())))
  end
let rec show_llist fa t =
  GT.fix0 (fun self  -> gcata_llist ((new show_llist) self (fun ()  -> fa)))
    () t
