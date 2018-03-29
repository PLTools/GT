open GT

type 'a maybe = Just of 'a | Nothing
[@@deriving gt ~show ~gmap]

type 'a pv = [ `A of 'a ]
[@@deriving gt ~show ~gmap]

let () =
  let sh x = show_pv (fun () x -> x) () x in
  Printf.printf "%s\n%!" (sh @@ `A "aaa")

     type +'a wtf = [ `C of 'a  | 'a pv] maybe (* [@@deriving  gt ~show ~gmap] *)
     class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_wtf =
       object
         inherit
           [[ `C of 'a  | 'a pv],[ `C of 'a  | 'a pv],[ `C of 'a  | 'a pv],
           'inh,'syn,'extra] class_maybe
       end
     let gcata_wtf = gcata_maybe
     class ['a,'extra] show_wtf _fself  fa =
       object
         inherit  ['a,unit,string,'inh,string,'extra] class_wtf
         inherit  (([[ `C of 'a  | 'a pv],'extra] show_maybe) _fself
           (fun inh  ->
              fun foo  ->
                match foo with
                | `C a -> Format.sprintf "`C(%s)" (fa (inh : unit) a)
                | #pv as subj -> show_pv fa inh subj))
       end
     let rec show_wtf fa the_init t =
       GT.fix0 (fun self  -> gcata_wtf ((new show_wtf) self fa)) the_init t
     class ['a,'a_2,'extra] gmap_wtf _fself  fa =
       object
         inherit  ['a,unit,'a_2,'inh,'a_2 wtf,'extra] class_wtf
         inherit  (([[ `C of 'a  | 'a pv],[ `C of 'a  | 'a pv],'extra]
           gmap_maybe) _fself
           (fun inh  ->
              fun foo  ->
                match foo with
                | `C a -> `C (fa inh a)
                | #pv as subj -> gmap_pv fa inh subj))
       end
     let rec gmap_wtf fa the_init t =
       GT.fix0 (fun self  -> gcata_wtf ((new gmap_wtf) self fa)) the_init t
     module type MT_wtf  =
       sig
         val gcata :
           ('a,_,'sa,'inh,'syn,_)#class_wtf -> 'inh -> 'a wtf -> 'syn
         val show : (unit -> 'a -> string) -> unit -> 'a wtf -> string
         val gmap : (unit -> 'a -> 'a_2) -> unit -> 'a wtf -> 'a_2 wtf
       end
     let wtf : (module MT_wtf) = (module
       struct
         let gcata = gcata_wtf
         let show = show_wtf
         let gmap = gmap_wtf
       end)

(* include (struct
 *   type 'a wtf = [ `C of 'a | 'a pv ] maybe
 *   [@@deriving gt ~show ~gmap]
 * end : sig
 *   type 'a wtf = [ `C of 'a | 'a pv ] maybe
 *   [@@deriving gt ~show ~gmap]
 *
 * end) *)

let () =
  let sh x = show_wtf (fun () x -> x) () x in
  Printf.printf "%s\t%s\n%!" (sh @@ Just(`A "a")) (sh @@ Just (`C "ccc"))
