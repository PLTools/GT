let id x = x

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  [@@deriving gt ~options:{ show; gmap }]

    (* class ['a,'a_2,'b,'b_2,'extra_pv] gmap_pv_t :
     *   Fix_gmap.fn ->
     *     (unit -> 'a -> 'a_2) ->
     *       (unit -> 'b -> 'b_2) ->
     *         (unit -> ('a, 'b) pv -> 'extra_pv (\* !!!! *\)) ->
     *           object
     *             constraint 'extra_pv = [> ('a_2, 'b_2) pv]
     *             method  c_A : unit -> ('a, 'b) pv -> 'a -> 'extra_pv
     *             method  c_B : unit -> ('a, 'b) pv -> 'b -> 'extra_pv
     *           end *)

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
  type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a ]
  [@@deriving gt ~options:{show; gmap}]
end = struct
  type ('a, 'b) pv_ext = [ ('a, 'b) PV.pv | `C of 'a ]
  [@@deriving gt ~options:{show; gmap}]

    (* class ['a,'a_2,'b,'b_2,'extra_pv_ext] gmap_pv_ext_t ({
     *                                                        Fix_gmap.call =
     *                                                          call
     *                                                        } as _mutuals_pack)
     *     fa  fb
     *     (fself_pv_ext: unit -> ('a,'b) pv_ext -> [> ('a2,'b2) pv_ext ]) =
     *   object
     *     inherit  [unit,'a,'a_2,unit,'b,'b_2
     *              ,unit, [> ('a_2, 'b_2) pv_ext], 'extra_pv_ext]
     *       pv_ext_t
     *     (\* constraint 'extra_pv_ext = [> ('a_2, 'b_2) pv_ext] *\)
     *     inherit  ((['a,'a_2,'b,'b_2, 'extra_pv_ext ] PV.gmap_pv_t) PV.gmap_fix
     *       fa fb
     *       (fun () ->
     *          fun subj ->
     *            match subj with | #PV.pv as subj -> fself_pv_ext () subj
     *       ))
     *     method c_C inh___008_ _ _x__009_ =
     *       match `C (fa inh___008_ _x__009_) with | #pv_ext as wtf -> wtf
     *   end *)

end

let _ =
  let open PV in
  let open PVExt in
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv: %s\n" @@
      GT.show pv  GT.id   GT.id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
  GT.show pv_ext (GT.show GT.int) GT.id @@
    ((GT.gmap pv int_of_string GT.id (`A "1")) :> (_,_) pv_ext);
  Printf.printf "Original pv_ext: %s\n" @@
    GT.show pv_ext id id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    GT.show pv_ext (GT.show GT.int)   GT.id @@
    ((GT.gmap pv_ext int_of_string  GT.id (`C "1")) :> (_,_) pv_ext);

module PVExt2 : sig
  type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
  [@@deriving gt ~options:{show; gmap}]
  end = struct
  type ('a, 'b) pv_ext2 = [ ('a, 'b) PVExt.pv_ext | `D of 'a]
  [@@deriving gt ~options:{show; gmap}]
end

let () =
  let open PVExt in
  let open PVExt2 in

  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext: %s\n" @@
      GT.show pv_ext2  GT.id   GT.id (`C "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
    GT.show pv_ext2  (GT.show GT.int) GT.id
      ((GT.gmap pv_ext (int_of_string)  GT.id (`C "1")) :> (_,_) pv_ext2);
  Printf.printf "Original pv_ext2: %s\n" @@
    GT.show pv_ext2 GT.id  GT.id (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
    GT.show pv_ext2 (GT.show GT.int) GT.id @@
    GT.gmap pv_ext2 int_of_string GT.id (`D "1");

(* module PVExt3 : sig
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
