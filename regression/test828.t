  $ ../ppx/pp_gt.exe -pretty test828mut.ml
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
  include
    (struct
       type 'a aaa = 'a bbb GT.option
       and 'a bbb = 'a bbb GT.list[@@deriving gt ~options:{ gmap }]
       include
         struct
           class virtual ['ia,'a,'sa,'inh,'extra,'syn] aaa_t =
             object
               inherit  ['ia bbb,'a bbb,'sa bbb,'inh,'extra,'syn] GT.option_t
             end
           class virtual ['ia,'a,'sa,'inh,'extra,'syn] bbb_t =
             object
               inherit  ['ia bbb,'a bbb,'sa bbb,'inh,'extra,'syn] GT.list_t
             end
           let gcata_aaa = GT.gcata_option
           let gcata_bbb = GT.gcata_list
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
               inherit  ((['a bbb,'a_2 bbb,'extra_aaa,'syn_aaa]
                 GT.gmap_option_t) (gmap_bbb fa) (_fself_aaa fa))
             end
           class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub ((_, _fself_bbb)
                                                                  as
                                                                  _mutuals_pack)
              fa =
             object
               inherit  [unit,'a,'a_2,unit,'extra_bbb,'a_2 bbb] bbb_t
               constraint 'extra_bbb = 'a bbb
               constraint 'syn_bbb = 'a_2 bbb
               inherit  ((['a bbb,'a_2 bbb,'extra_bbb,'syn_bbb] GT.gmap_list_t)
                 (_fself_bbb fa) (_fself_bbb fa))
             end
           let gmap_aaa_0 = new gmap_aaa_t_stub
           let gmap_bbb_0 = new gmap_bbb_t_stub
           let gmap_aaa eta__001_ =
             let (f, _) = fix_aaa_bbb gmap_aaa_0 gmap_bbb_0 in f eta__001_
           let gmap_bbb eta__002_ =
             let (_, f) = fix_aaa_bbb gmap_aaa_0 gmap_bbb_0 in f eta__002_
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
                 (object method gmap fa subj = gmap_aaa (GT.lift fa) () subj
                  end)
             }
           let gmap_aaa fa subj = gmap_aaa (GT.lift fa) () subj
           let bbb =
             {
               GT.gcata = gcata_bbb;
               GT.fix = fix_aaa_bbb;
               GT.plugins =
                 (object method gmap fa subj = gmap_bbb (GT.lift fa) () subj
                  end)
             }
           let gmap_bbb fa subj = gmap_bbb (GT.lift fa) () subj
         end[@@ocaml.doc "@inline"][@@merlin.hide ]
       let __ (type a) (type b) =
         (fun eta -> GT.gmap bbb eta : (a -> b) -> a bbb -> b bbb)
     end :
      sig
        type 'a aaa = 'a bbb GT.option
        and 'a bbb = 'a bbb GT.list[@@deriving gt ~options:{ gmap }]
        include
          sig
            class virtual ['ia,'a,'sa,'inh,'extra,'syn] aaa_t :
              object
                inherit ['ia bbb,'a bbb,'sa bbb,'inh,'extra,'syn] GT.option_t
              end
            val gcata_aaa :
              (_,'a,'sa,'inh,'a aaa,'syn)#aaa_t -> 'inh -> 'a aaa -> 'syn
            class virtual ['ia,'a,'sa,'inh,'extra,'syn] bbb_t :
              object
                inherit ['ia bbb,'a bbb,'sa bbb,'inh,'extra,'syn] GT.list_t
              end
            val gcata_bbb :
              (_,'a,'sa,'inh,'a bbb,'syn)#bbb_t -> 'inh -> 'a bbb -> 'syn
            val fix_aaa_bbb :
              (('alias_for_aaa * 'alias_for_bbb) ->
                 ('a1_i -> 'a1 -> 'a1_s) ->
                   ('a1_i,'a1,'a1_s,'inh2,'a1 aaa,'syn3)#aaa_t)
                ->
                (('alias_for_aaa * 'alias_for_bbb) ->
                   ('a4_i -> 'a4 -> 'a4_s) ->
                     ('a4_i,'a4,'a4_s,'inh5,'a4 bbb,'syn6)#bbb_t)
                  ->
                  (((('a1_i -> 'a1 -> 'a1_s) -> 'inh2 -> 'a1 aaa -> 'syn3) as
                      'alias_for_aaa)
                    *
                    ((('a4_i -> 'a4 -> 'a4_s) -> 'inh5 -> 'a4 bbb -> 'syn6) as
                       'alias_for_bbb))
            class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t_stub :
              (((unit -> 'a -> 'a_2) -> unit -> 'a aaa -> 'a_2 aaa) *
                ((unit -> 'a -> 'a_2) -> unit -> 'a bbb -> 'a_2 bbb)) ->
                (unit -> 'a -> 'a_2) ->
                  object
                    constraint 'extra_aaa = 'a aaa
                    constraint 'syn_aaa = 'a_2 aaa
                    inherit ['a bbb,'a_2 bbb,'extra_aaa,'syn_aaa]
                      GT.gmap_option_t
                  end
            class ['a,'a_2,'extra_aaa,'syn_aaa] gmap_aaa_t :
              (unit -> 'a -> 'a_2) ->
                (unit -> 'a aaa -> 'a_2 aaa) ->
                  object
                    constraint 'extra_aaa = 'a aaa
                    constraint 'syn_aaa = 'a_2 aaa
                    inherit ['a bbb,'a_2 bbb,'extra_aaa,'syn_aaa]
                      GT.gmap_option_t
                  end
            class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t_stub :
              (((unit -> 'a -> 'a_2) -> unit -> 'a aaa -> 'a_2 aaa) *
                ((unit -> 'a -> 'a_2) -> unit -> 'a bbb -> 'a_2 bbb)) ->
                (unit -> 'a -> 'a_2) ->
                  object
                    constraint 'extra_bbb = 'a bbb
                    constraint 'syn_bbb = 'a_2 bbb
                    inherit ['a bbb,'a_2 bbb,'extra_bbb,'syn_bbb]
                      GT.gmap_list_t
                  end
            class ['a,'a_2,'extra_bbb,'syn_bbb] gmap_bbb_t :
              (unit -> 'a -> 'a_2) ->
                (unit -> 'a bbb -> 'a_2 bbb) ->
                  object
                    constraint 'extra_bbb = 'a bbb
                    constraint 'syn_bbb = 'a_2 bbb
                    inherit ['a bbb,'a_2 bbb,'extra_bbb,'syn_bbb]
                      GT.gmap_list_t
                  end
            val gmap_aaa : ('a -> 'a_2) -> 'a aaa -> 'a_2 aaa
            val gmap_bbb : ('a -> 'a_2) -> 'a bbb -> 'a_2 bbb
            val aaa :
              ((_,'a,'sa,'inh,'a aaa,'syn)#aaa_t -> 'inh -> 'a aaa -> 'syn,
                < gmap: ('a -> 'a_2) -> 'a aaa -> 'a_2 aaa   > ,
                (('alias_for_aaa * 'alias_for_bbb) ->
                   ('a1_i -> 'a1 -> 'a1_s) ->
                     ('a1_i,'a1,'a1_s,'inh2,'a1 aaa,'syn3)#aaa_t)
                  ->
                  (('alias_for_aaa * 'alias_for_bbb) ->
                     ('a4_i -> 'a4 -> 'a4_s) ->
                       ('a4_i,'a4,'a4_s,'inh5,'a4 bbb,'syn6)#bbb_t)
                    ->
                    (((('a1_i -> 'a1 -> 'a1_s) -> 'inh2 -> 'a1 aaa -> 'syn3) as
                        'alias_for_aaa)
                      *
                      ((('a4_i -> 'a4 -> 'a4_s) -> 'inh5 -> 'a4 bbb -> 'syn6)
                         as 'alias_for_bbb)))
                GT.t
            val bbb :
              ((_,'a,'sa,'inh,'a bbb,'syn)#bbb_t -> 'inh -> 'a bbb -> 'syn,
                < gmap: ('a -> 'a_2) -> 'a bbb -> 'a_2 bbb   > ,
                (('alias_for_aaa * 'alias_for_bbb) ->
                   ('a1_i -> 'a1 -> 'a1_s) ->
                     ('a1_i,'a1,'a1_s,'inh2,'a1 aaa,'syn3)#aaa_t)
                  ->
                  (('alias_for_aaa * 'alias_for_bbb) ->
                     ('a4_i -> 'a4 -> 'a4_s) ->
                       ('a4_i,'a4,'a4_s,'inh5,'a4 bbb,'syn6)#bbb_t)
                    ->
                    (((('a1_i -> 'a1 -> 'a1_s) -> 'inh2 -> 'a1 aaa -> 'syn3) as
                        'alias_for_aaa)
                      *
                      ((('a4_i -> 'a4 -> 'a4_s) -> 'inh5 -> 'a4 bbb -> 'syn6)
                         as 'alias_for_bbb)))
                GT.t
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
      end)
  $ ./test828mut.exe
