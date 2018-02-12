open GT

type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
[@@deriving gt ~show ~gmap]

type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
(* [@@deriving gt ~show ~gmap] *)

type nonrec ('self,'a,'b) pv_ext_open =
  'self constraint 'self = [> `C of 'a  | ('a,'b) pv]
class virtual ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'polyvar_extra] class_pv_ext =
  object
    method virtual  c_C : 'inh -> 'a -> 'syn
    inherit  ['a,'ia,'sa,'b,'ib,'sb,'inh,'syn,'polyvar_extra] class_pv
  end
let gcata_pv_ext tr inh t =
  match t with | `C a -> tr#c_C inh a | #pv as subj -> gcata_pv tr inh subj
class ['a,'b,'polyvar_extra] show_pv_ext _fself  fa  fb =
  object
    inherit
      ['a,unit,string,'b,unit,string,'inh,string,('polyvar_extra,'a,'b)
                                                   pv_ext_open]
      class_pv_ext
    method c_C () a = Format.sprintf "`C(%a)" fa a
    inherit  ((['a,'b,'polyvar_extra] show_pv) _fself fa fb)
  end
let rec show_pv_ext fa fb () t =
  GT.fix0 (fun self  -> gcata_pv_ext ((new show_pv_ext) self fa fb)) () t
class ['a,'a_2,'b,'b_2,'polyvar_extra] gmap_pv_ext _fself  fa  fb =
  object
    inherit
      ['a,unit,'a_2,'b,unit,'b_2,'inh
      , ('polyvar_extra,'a_2,'b_2) pv_ext_open as 'ans
      , 'polyvar_extra ]
      class_pv_ext
    method c_C () a = `C (fa () a)
    inherit  ((['a,'a_2,'b,'b_2,'polyvar_extra] gmap_pv) _fself fa fb)
  end
let rec gmap_pv_ext fa fb () t =
  GT.fix0 (fun self  -> gcata_pv_ext ((new gmap_pv_ext) self fa fb)) () t


let () =
  let sh x = show_pv_ext (fun () -> string_of_int) (fun () x -> x) () x in
  Printf.printf "%s\t%s\t%s\n%!" (sh @@ `A 5) (sh @@ `B "xxx") (sh @@ `C 32)
