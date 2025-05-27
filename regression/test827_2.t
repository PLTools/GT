  $ ../ppx/pp_gt.exe -pretty test827mut.ml
  class virtual ['ia,'a,'sa,'inh,'syn] list_t =
    object
      method virtual  c_Nil : 'inh -> 'syn
      method virtual  c_Cons : 'inh -> 'a -> 'a list -> 'syn
    end
  let gcata_list tr inh s =
    match s with | [] -> tr#c_Nil inh | x::xs -> tr#c_Cons inh x xs
  class ['a,'sa,'syn] gmap_list_t fa  fself =
    object
      constraint 'syn = 'sa list
      inherit  [unit,'a,'sa,unit,'syn] list_t
      method c_Nil _ = []
      method c_Cons _ x xs = (fa () x) :: (fself () xs)
    end
  class virtual ['ia,'a,'sa,'inh,'syn] option_t =
    object
      method virtual  c_None : 'inh -> 'syn
      method virtual  c_Some : 'inh -> 'a -> 'syn
    end
  let gcata_option tr inh subj =
    match subj with | None -> tr#c_None inh | Some x -> tr#c_Some inh x
  class ['a,'sa,'syn] gmap_option_t fa  _fself =
    object
      constraint 'syn = 'sa option
      inherit  [unit,'a,'sa,unit,'syn] option_t
      method c_None () = None
      method c_Some () x = Some (fa () x)
    end
  type 'a bbb =
    | AAA of 'a * 'a bbb GT.list 
  and 'a aaa =
    | BBB of 'a * 'a bbb GT.option [@@deriving gt ~options:{ gmap }]
  include
    struct
      class virtual ['ia,'a,'sa,'inh,'extra,'syn] aaa_t =
        object
          method virtual  c_BBB :
            'inh -> 'extra -> 'a -> 'a bbb GT.option -> 'syn
        end
      class virtual ['ia,'a,'sa,'inh,'extra,'syn] bbb_t =
        object
          method virtual  c_AAA :
            'inh -> 'extra -> 'a -> 'a bbb GT.list -> 'syn
        end
      let gcata_aaa (tr : (_,'typ0__003_,_,_,'typ0__003_ aaa,_)#aaa_t) inh subj
        =
        match subj with
        | BBB (_x__001_, _x__002_) -> tr#c_BBB inh subj _x__001_ _x__002_
      let gcata_bbb (tr : (_,'typ0__006_,_,_,'typ0__006_ bbb,_)#bbb_t) inh subj
        =
        match subj with
        | AAA (_x__004_, _x__005_) -> tr#c_AAA inh subj _x__004_ _x__005_
      let fix_aaa_bbb aaa0 bbb0 =
        let rec traitaaa fa inh subj =
          gcata_aaa (aaa0 (traitaaa, traitbbb) fa) inh subj
        and traitbbb fa inh subj =
          gcata_bbb (bbb0 (traitaaa, traitbbb) fa) inh subj in
        (traitaaa, traitbbb)
      class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t_stub ((_fself_aaa,
                                                            gmap_bbb) as
                                                             _mutuals_pack)
         fa =
        object
          inherit  [unit,'a,'a_2,unit,'extra_aaa,'a_2 aaa] aaa_t
          constraint 'extra_aaa = 'a aaa
          constraint 'syn_aaa = 'a_2 aaa
          method c_BBB () _ _x__009_ _x__010_ =
            BBB
              ((fa () _x__009_),
                ((fun () -> fun subj -> GT.gmap GT.option (gmap_bbb fa ()) subj)
                   () _x__010_))
        end
      class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub ((_, _fself_bbb) as
                                                             _mutuals_pack)
         fa =
        object
          inherit  [unit,'a,'a_2,unit,'extra_bbb,'a_2 bbb] bbb_t
          constraint 'extra_bbb = 'a bbb
          constraint 'syn_bbb = 'a_2 bbb
          method c_AAA () _ _x__011_ _x__012_ =
            AAA
              ((fa () _x__011_),
                ((fun () -> fun subj -> GT.gmap GT.list (_fself_bbb fa ()) subj)
                   () _x__012_))
        end
      let gmap_aaa_0 eta = (new gmap_aaa_t_stub) eta
      let gmap_bbb_0 eta = (new gmap_bbb_t_stub) eta
      let gmap_aaa eta__007_ =
        let (f, _) = fix_aaa_bbb gmap_aaa_0 gmap_bbb_0 in f eta__007_
      let gmap_bbb eta__008_ =
        let (_, f) = fix_aaa_bbb gmap_aaa_0 gmap_bbb_0 in f eta__008_
      class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t _  fa =
        object
          inherit  ((['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t_stub)
            (gmap_aaa, gmap_bbb) fa)
        end
      class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t _  fa =
        object
          inherit  ((['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub)
            (gmap_aaa, gmap_bbb) fa)
        end
      let aaa =
        {
          GT.gcata = gcata_aaa;
          GT.fix = fix_aaa_bbb;
          GT.plugins =
            (object method gmap fa subj = gmap_aaa (GT.lift fa) () subj end)
        }
      let gmap_aaa fa subj = gmap_aaa (GT.lift fa) () subj
      let bbb =
        {
          GT.gcata = gcata_bbb;
          GT.fix = fix_aaa_bbb;
          GT.plugins =
            (object method gmap fa subj = gmap_bbb (GT.lift fa) () subj end)
        }
      let gmap_bbb fa subj = gmap_bbb (GT.lift fa) () subj
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let __ (type a) (type b) =
    (fun eta -> GT.gmap bbb eta : (a -> b) -> a bbb -> b bbb)

