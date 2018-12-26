module GT = struct
  include Show_typed_api
end

module T : sig
  (* @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   * @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;; *)
  end = struct
  (* @type t2 = GT.int * GT.string with (\* show,gmap,foldl,eq,compare,eval,stateful,html, *\)show_typed;; *)

  let () = ();;

  (* @type 'a t3 = GT.int * 'a * GT.string with (\* show,gmap,foldl,eq,compare,eval,stateful,html, *\)show_typed;; *)
    type 'a t3 = GT.int * 'a * GT.string
    class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] t3_t =
      object
        inherit
          [GT.int, GT.int, GT.int, 'ia, 'a, 'sa, GT.string, GT.string,
          GT.string, 'inh, 'self, 'syn]
            GT.tuple3_t
      end
    let gcata_t3 = GT.gcata_tuple3
    class ['a, 'self_t3] show_typed_t3_t typ_a fa fself =
      object
        inherit [unit, 'a, string, unit, 'self_t3, string] t3_t
        inherit
          [GT.int, 'a, GT.string, 'self_t3] GT.show_typed_tuple3_t "GT.int"
            (fun () subj -> GT.show_typed GT.int subj)
            typ_a
            fa
            "GT.string"
            (fun () subj -> GT.show_typed GT.string subj)
            fself
      end
    let rec show_typed_t3 typ_a fa inh0 subj =
      GT.transform_gc gcata_t3 ((new show_typed_t3_t) typ_a fa) inh0 subj
    let t3 =
      {GT.gcata = gcata_t3;
       GT.plugins = object (_) method show_typed a = show_typed_t3 a () end}
    let show_typed_t3 a = show_typed_t3 a ()


  let () = ();;
  (* @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
   *
   * let () = ();;
   *
   * @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,html,show_typed;;
   *
   * let () = ();;
   *
   * @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * let () = ();;
   * (\* TODO: implement stateful for records *\)
   * @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;;
   * let () = () *)
end
