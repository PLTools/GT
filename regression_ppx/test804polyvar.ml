open GT

type 'a maybe = Just of 'a | Nothing [@@deriving gt ~show ~gmap]

type 'a pv = [ `A of 'a ] [@@deriving gt ~show ~gmap]


let () =
  let sh x = show_pv (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")

type 'a wtf = [ `C of 'a | 'a pv ] maybe
(* [@@deriving gt ~show ~gmap] *)

class virtual ['a,'ia,'sa,'inh,'syn] class_wtf =
  object
    inherit 
      [[ `C of 'a  | 'a pv],[ `C of 'a  | 'a pv],[ `C of 'a  | 'a pv],
      'inh,'syn] class_maybe
  end
let gcata_wtf = gcata_maybe 
class ['a] show_wtf (show_wtf as _fself)  fa =
  object
    inherit  ['a,unit,string,'inh,'syn] class_wtf
    inherit  (([[ `C of 'a  | 'a pv]] show_maybe)
      (show_maybe
         (fun ()  ->
            fun foo  ->
              match foo with
              | `C a -> Printf.sprintf "`C (%s)" fa a
              | #pv as subj -> 1))
      (fun ()  ->
         fun foo  ->
           match foo with
           | `C a -> Printf.sprintf "`C (%s)" fa a
           | #pv as subj -> 1))
  end
let rec show_wtf fa () t =
  GT.fix0 (fun self  -> gcata_wtf ((new show_wtf) self fa)) () t 

let () =
  let sh x = show_wtf (fun () x -> x) () x in
  Printf.printf "%s\t%s\n%!" (Just(`A "a")) (Just (`C "ccc"))
