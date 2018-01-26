open GT

type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
(* [@@deriving gt ~show ~gmap] *)


class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn] class_pv =
  object
    method virtual  c_A : 'inh -> 'a -> 'syn
    method virtual  c_B : 'inh -> 'b -> 'syn
  end
let gcata_pv tr inh t =
  match t with | `A a -> tr#c_A inh a | `B a -> tr#c_B inh a
class ['a,'b] show_pv _fself  fa  fb =
  object
    inherit  ['a,unit,string,'b,unit,string,'inh,string] class_pv
    method c_A () a = Format.sprintf "`A(%a)" fa a
    method c_B () a = Format.sprintf "`B(%a)" fb a
  end
let rec show_pv fa fb () t =
  GT.fix0 (fun self  -> gcata_pv ((new show_pv) self fa fb)) () t
class ['a,'a_2,'b,'b_2] gmap_pv _fself  fa  fb =
  object
    inherit  ['a,unit,'a_2,'b,unit,'b_2,'inh,('a_2,'b_2) pv] class_pv
    method c_A () a = `A (fa () a)
    method c_B () a = `B (fb () a)
  end
let rec gmap_pv fa fb () t =
  GT.fix0 (fun self  -> gcata_pv ((new gmap_pv) self fa fb)) () t

let () =
  let sh (x: (int,string)pv) = show_pv (fun () -> string_of_int) (fun () x -> x) () x in
  Printf.printf "%s\t%s\n%!" (sh @@ `A 5) (sh @@ `B "xxx")

type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
[@@deriving gt ~show ~gmap]


let () =
  let sh x = show_pv_ext (fun () -> string_of_int) (fun () x -> x) () x in
  Printf.printf "%s\t%s\t%s\n%!" (sh @@ `A 5) (sh @@ `B "xxx") (sh @@ `C 32)
