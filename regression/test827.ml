let () = print_endline "test827"

type 'a aaa = 'a bbb GT.list
and 'b bbb = 'b aaa GT.option

(* [@@deriving gt ~options:{ show }] *)

class virtual ['ia, 'a, 'sa, 'inh, 'extra, 'syn] aaa_t =
  object
    inherit ['ia bbb, 'a bbb, 'sa bbb, 'inh, 'extra, 'syn] GT.list_t
  end

class virtual ['ib, 'b, 'sb, 'inh, 'extra, 'syn] bbb_t =
  object
    inherit ['ib aaa, 'b aaa, 'sb aaa, 'inh, 'extra, 'syn] GT.option_t
  end

let gcata_aaa = GT.gcata_list
let gcata_bbb = GT.gcata_option

class ['a, 'extra_aaa] show_aaa_t_stub (_fself_aaa, show_bbb)
  (fa : unit -> _ -> string) =
  object
    inherit [unit, 'a, string, unit, 'extra_aaa, string] aaa_t
    constraint 'extra_aaa = 'a aaa

    inherit
      ['a bbb, 'extra_aaa] GT.show_list_t
        (show_bbb fa)
        (_fself_aaa : unit -> _ aaa -> string)
  end

class ['b, 'extra_bbb] show_bbb_t_stub (show_aaa, _fself_bbb)
  (fb : unit -> _ -> string) =
  object
    inherit [unit, 'b, string, unit, 'extra_bbb, string] bbb_t
    constraint 'extra_bbb = 'b bbb
    inherit ['b aaa, 'extra_bbb] GT.show_option_t (show_aaa fb) _fself_bbb
  end

let show_aaa_0 : (unit -> _ -> string) * _ -> _ = fun eta -> new show_aaa_t_stub eta

let fix_aaa aaa0 bbb0 =
  let rec traitaaa fa inh (subj : _) =
    gcata_aaa (aaa0 (traitaaa, traitbbb) fa) inh subj
  and traitbbb fb inh subj =
    gcata_bbb (bbb0 (traitaaa, traitbbb) fb) inh subj
  in
  (traitaaa, traitbbb)

let show_bbb_0 = fun eta -> new show_bbb_t_stub eta

(* let show_aaa eta__001_ =
     let f, _ = fix_aaa show_aaa_0 show_bbb_0 in
     f eta__001_

   let show_bbb eta__002_ =
     let _, f = fix_aaa show_aaa_0 show_bbb_0 in
     f eta__002_
*)
