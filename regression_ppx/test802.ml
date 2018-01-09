open GT

type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
                     (* [@@deriving gt ~gmap ~show ] *)

let _ = fun (_ : ('a,'b) a)  -> ()
let _ = fun (_ : ('a,'b) b)  -> ()
class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] class_a =
  object
    method virtual  c_B : 'inh -> ('a,'b) b -> 'syn
    method virtual  c_D : 'inh -> 'a -> 'syn
  end
let gcata_a tr inh t =
  match t with | B a -> tr#c_B inh a | D a -> tr#c_D inh a
let _ = gcata_a
class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] class_b =
  object
    method virtual  c_A : 'inh -> (('a * 'a),'b) a -> 'syn
    method virtual  c_C : 'inh -> 'a -> 'b -> 'syn
    method virtual  c_E : 'inh -> 'syn
  end
let gcata_b tr inh t =
  match t with
  | A a -> tr#c_A inh a
  | C (a,b) -> tr#c_C inh a b
  | E  -> tr#c_E inh
let _ = gcata_b
class ['a,'b] show_a_stub show_b  (show_a as _fself)  fa  fb =
  object
    inherit  ['a,unit,string,'b,unit,string,'inh,'syn] class_a
    method c_B () a = Format.sprintf "B(%s)" (show_b fa fb a)
    method c_D () a = Format.sprintf "D(%s)" (fa a)
  end
class ['a,'b] show_b_stub show_a  (show_b as _fself)  fa  fb =
  object
    inherit  ['a,unit,string,'b,unit,string,'inh,'syn] class_b
    method c_A () a =
      Format.sprintf "A(%s)" (show_a (show_tuple2 fa fa) fb a)
    method c_C () a b = Format.sprintf "C(%s, %s)" (fa a) (fb b)
    method c_E () = "E"
  end
let (_:int) = new show_b_stub
let rec show_a fa fb t =
  fix0
    (fun self  ->
       gcata_a
         ((new show_a_stub) show_b self (fun ()  -> fa) (fun ()  -> fb))) t

and show_b fa fb t =
  fix0
    (fun self  ->
       gcata_b
         ((new show_b_stub) show_a self (fun ()  -> fa) (fun ()  -> fb))) t

let _ = show_a

and _ = show_b

class virtual ['a,'b] show_a fself  fa  fb =
  object inherit  ((['a,'b] show_a_stub) fself fa fb) end
class virtual ['a,'b] show_b fself  fa  fb =
  object inherit  ((['a,'b] show_b_stub) fself fa fb) end

(* type ('a,'b) demo = 'a * 'b *)
(* type nonrec ('a,'b) demo = ('a,'b) demo list *)
