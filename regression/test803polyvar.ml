let id x = x

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  [@@deriving gt ~options:{ show; gmap }]

  (* class ['a,'a_2,'b,'b_2,'extra_pv] gmap_pv_t :
   *     Fix_gmap.fn ->
   *       (unit -> 'a -> 'a_2) ->
   *         (unit -> 'b -> 'b_2) ->
   *           (unit -> ('a, 'b) pv -> ('a_2, 'b_2) pv) ->
   *             object
   *               constraint 'extra_pv = [> ('a_2, 'b_2) pv ]
   *               method  c_A : unit -> ('a, 'b) pv -> 'a -> 'extra_pv
   *               method  c_B : unit -> ('a, 'b) pv -> 'b -> 'extra_pv
   *             end *)

end = struct
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  [@@deriving gt ~options:{show; gmap }]

    (* class ['a,'a_2,'b,'b_2,'extra_pv] gmap_pv_t
     *     ({ Fix_gmap.call = call } as               _mutuals_pack)
     *    fa fb fself_pv =
     *   object
     *     inherit  [ unit,'a,'a_2
     *              , unit,'b,'b_2
     *              , unit,'extra_pv, [> ('a_2, 'b_2) pv ] as 'extra_pv ]
     *         pv_t
     *     method c_A inh___003_ _ _x__004_ =
     *       match `A (fa inh___003_ _x__004_) with #pv as x -> x
     *     method c_B inh___005_ _ _x__006_ =
     *       match `B (fb inh___005_ _x__006_) with #pv as x -> x
     *   end *)

end

let _ =
  let open PV in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (GT.show pv GT.id          GT.id (`A "1"))
      (GT.show pv (GT.show GT.int)  GT.id @@
       GT.gmap pv int_of_string  GT.id (`A "1"))

module PVExt : sig
  (* type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a ]
   * [@@deriving gt ~options:{show; gmap}] *)
end = struct
  type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a ]
  (* [@@deriving gt ~options:{show; gmap}] *)
    class virtual ['ia,'a,'sa,'ib,'b,'sb,'inh,'extra,'syn] pv_ext_t =
      object
        inherit  ['ia,'a,'sa,'ib,'b,'sb,'inh,'extra,'syn] PV.pv_t
        method virtual  c_C : 'inh -> ('a, 'b) pv_ext -> 'a -> 'syn
      end
    let gcata_pv_ext tr inh subj =
      match subj with
      | #PV.pv as subj -> PV.gcata_pv tr inh subj
      | `C ___011_ -> tr#c_C inh subj ___011_
    module type IndexResult  =
      sig
        type 'a result
        type _ i =
          | Pv_ext: ('a result -> 'b result -> ('a, 'b) pv_ext result) i
      end
    module Index(S:sig type 'a result end) =
      struct
        type 'a result = 'a S.result
        type _ i =
          | Pv_ext: ('a result -> 'b result -> ('a, 'b) pv_ext result) i
      end
    module type IndexResult2  =
      sig
        type ('a, 'b) result
        type _ i =
          | Pv_ext:
          (('a, 'a2) result ->
             ('b, 'b2) result -> (('a, 'b) pv_ext, ('a2, 'b2) pv_ext) result)
          i
      end
    module Index2(S:sig type ('a, 'b) result end) =
      struct
        type ('a, 'b) result = ('a, 'b) S.result
        type _ i =
          | Pv_ext:
          (('a, 'a2) result ->
             ('b, 'b2) result -> (('a, 'b) pv_ext, ('a2, 'b2) pv_ext) result)
          i
      end
    module Igmap =
      (Index2)(struct type ('a, 'b) result = unit -> 'a -> 'b end)
    module Fix_gmap = (GT.FixV)(Igmap)
    class ['a,'a_2,'b,'b_2,'extra_pv_ext] gmap_pv_ext_t ({
                                                           Fix_gmap.call =
                                                             call
                                                           } as _mutuals_pack)
       fa  fb  fself_pv_ext =
      object
        inherit
          [unit,'a,'a_2,unit,'b,'b_2,unit,'extra_pv_ext,('a_2, 'b_2) pv_ext]
            pv_ext_t
        constraint 'extra_pv_ext = [> ('a_2, 'b_2) pv_ext ]
        inherit  (['a,'a_2,'b,'b_2,'extra_pv_ext] PV.gmap_pv_t PV.gmap_fix
          fa fb
          (fun () -> assert false
             (* fun subj ->
              *   match subj with | #PV.pv as subj -> fself_pv_ext () subj *)))
        method c_C inh___012_ _ _x__013_ = `C (fa inh___012_ _x__013_)
      end
    let gmap_pv_ext_0 call fa fb inh0 subj =
      GT.transform_gc gcata_pv_ext ((new gmap_pv_ext_t) call fa fb) inh0 subj
    let gmap_fix =
      Fix_gmap.fixv
        (fun f ->
           {
             call = fun (type a) ->
               fun (sym : a Igmap.i) ->
                 (match sym with | Igmap.Pv_ext -> gmap_pv_ext_0 f :
                 a)
           })
    module Ishow = (Index)(struct type 'a result = unit -> 'a -> string end)
    module Fix_show = (GT.FixV)(Ishow)
    class ['a,'b,'extra_pv_ext] show_pv_ext_t ({ Fix_show.call = call } as
                                                 _mutuals_pack)
       fa  fb  fself_pv_ext =
      object
        inherit  [unit,'a,string,unit,'b,string,unit,'extra_pv_ext,string]
          pv_ext_t
        inherit  ((['a,'b,'extra_pv_ext] PV.show_pv_t) PV.show_fix fa fb
          (fun () ->
             fun subj ->
               match subj with | #PV.pv as subj -> fself_pv_ext () subj))
        method c_C inh___014_ _ _x__015_ =
          Printf.sprintf "`C (%s)" (fa () _x__015_)
      end
    let show_pv_ext_0 call fa fb inh0 subj =
      GT.transform_gc gcata_pv_ext ((new show_pv_ext_t) call fa fb) inh0 subj
    let show_fix =
      Fix_show.fixv
        (fun f ->
           {
             call = fun (type a) ->
               fun (sym : a Ishow.i) ->
                 (match sym with | Ishow.Pv_ext -> show_pv_ext_0 f :
                 a)
           })
    let pv_ext =
      {
        GT.gcata = gcata_pv_ext;
        GT.plugins =
          (object
             method gmap fa fb subj =
               gmap_fix.call Igmap.Pv_ext (GT.lift fa) (GT.lift fb) () subj
             method show fa fb subj =
               show_fix.call Ishow.Pv_ext (GT.lift fa) (GT.lift fb) () subj
           end)
      }

end

(* let _ =
 *   let open PV in
 *   let open PVExt in
 *   Printf.printf "****************************\n%!";
 *   Printf.printf "Original pv: %s\n" @@
 *       GT.show pv  GT.id   GT.id (`A "1");
 *   Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
 *   GT.show pv_ext (GT.show GT.int) GT.id @@
 *     GT.gmap pv int_of_string GT.id (`A "1") ;
 *   Printf.printf "Original pv_ext: %s\n" @@
 *     GT.show pv_ext  GT.id   GT.id (`C "1");
 *   Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
 *     GT.show pv_ext (GT.show GT.int)   GT.id @@
 *     GT.gmap pv_ext int_of_string  GT.id (`C "1");
 *
 * module PVExt2 : sig
 *   type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
 *   [@@deriving gt ~options:{show; gmap}]
 * end = struct
 *   type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
 *   [@@deriving gt ~options:{show; gmap}]
 * end
 *
 * let () =
 *   let open PVExt in
 *   let open PVExt2 in
 *
 *   Printf.printf "****************************\n%!";
 *   Printf.printf "Original pv_ext: %s\n" @@
 *       GT.show pv_ext2  GT.id   GT.id (`C "1");
 *   Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
 *     GT.show pv_ext2  (GT.show GT.int) GT.id
 *       (GT.gmap pv_ext (int_of_string)  GT.id (`C "1") );
 *   Printf.printf "Original pv_ext2: %s\n" @@
 *     GT.show pv_ext2 GT.id  GT.id (`D "1");
 *   Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
 *     GT.show pv_ext2 (GT.show GT.int) GT.id @@
 *     GT.gmap pv_ext2 int_of_string GT.id (`D "1");
 *
 * module PVExt3 : sig
 *   type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c]
 *   [@@deriving gt ~options:{show; gmap}]
 * end = struct
 *   type ('a, 'b, 'c) pv_ext3 = [ ('a, 'b) PVExt2.pv_ext2 | `E of 'c]
 *   [@@deriving gt ~options:{show; gmap}]
 * end
 *
 * let () =
 *   let open PVExt2 in
 *   let open PVExt3 in
 *
 *   Printf.printf "****************************\n%!";
 *   Printf.printf "Original pv_ext2: %s\n" @@
 *       GT.show pv_ext2 GT.id GT.id  (`D "1");
 *   Printf.printf "Mapped pv_ext2 and showed as a pv_ext3: %s\n" @@
 *     GT.show pv_ext3 (GT.show GT.int) GT.id GT.id  @@
 *     GT.gmap pv_ext2 int_of_string GT.id       (`D "1");
 *   Printf.printf "Original pv_ext3: %s\n" @@
 *     GT.show pv_ext3   GT.id GT.id  GT.id (`E "1");
 *
 *   Printf.printf "Mapped PV_ext3 and showed as a pv_ext3: %s\n" @@
 *   GT.show pv_ext3 GT.id GT.id (GT.show GT.float) @@
 *   GT.gmap pv_ext3 GT.id GT.id float_of_string
 *     (`E "1.0"); *)
