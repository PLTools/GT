open GT
(* type t = int [@@deriving gt] *)

type ('a,'b) alist = Nil | Cons of 'a * 'b [@@deriving gt ~gmap ~show ]

type 'a list = ('a, 'a list) alist [@@deriving gt ~gmap ~show ]

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
    inherit  ((['a,'a list] show_alist)
      (fun ()  -> show_alist (fa ()) (_fself ())) (fa ()) (_fself ()))
  end
let rec show_list fa t =
  GT.fix0 (fun self  -> gcata_list ((new show_list) self (fun ()  -> fa))) ()
    t

(* type 'a logic = Var of int | Value of 'a [@@deriving gt ~gmap ~show ] *)

(* type 'a llist = ('a logic, 'b) alist logic as 'b [@@deriving gt ~gmap ~show ] *)

