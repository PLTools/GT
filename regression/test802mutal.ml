open Printf

module PV: sig
  type a = [`A of b | `C of GT.int   ]
  and  b = [`B of a | `D of GT.string]
  [@@deriving gt ~options:{show;gmap}]
end = struct
  type a = [`A of b | `C of GT.int   ]
  and  b = [`B of a | `D of GT.string]
  [@@deriving gt ~options:{show;gmap}]
end

module Show2 = struct
  open PV
  class ['self] show_b_t_stub2 (for_a,for_b) fself = object
    inherit ['self] show_b_t_stub (for_a,for_b) fself
    method c_C () (_ :b) a  = Printf.sprintf "new C (%s)" (for_a () a)
    method c_D () _ s  = Printf.sprintf "new D %s" s
  end

  let showa0 a b  = Printf.printf "new!\n"; new show_a_t_stub  a b
  let showb0 a b  = Printf.printf "new!\n"; new show_b_t_stub2 a b

  let show_a () s =
    (fst @@ fix_a showa0 showb0) () s

  let show_b () s =
    (snd @@ fix_a showa0 showb0) () s

  let _ = Printf.printf "%s\n" (show_a () (`A (`B (`A (`D "4")))))
end

type c = [ PV.b | `E of GT.int ]
[@@deriving gt ~options:{show}]

module ShowC = struct
  open PV

  class ['self] show_c_stub2 make_clas (fself: _ -> c -> _) =
    let show_a2,show_b2 =
      Show2.(fix_a
               showa0
               (fun _ _ -> ((make_clas fself ()) :> 'self show_b_t_stub) ))
    in
    object
      inherit [unit, _, string] c_t
      inherit [ 'self] show_b_t_stub (show_a2,show_b2) (fself :> unit -> b -> _)
      method! c_B () _ a  = sprintf "new `B (%s)" (show_a2 () a)
      method! c_D () _ s  = sprintf "new `D %s" s
      method  c_E () _ s  = sprintf "new `E %d" s
    end

  let rec showc0 fself () = Printf.printf "new c0!\n"; new show_c_stub2 showc0 fself

  let show_c () (s: c) =
    let rec trait () s = gcata_c (showc0 trait ()) () (s :> c)
    in
    trait () s

  let _ =
    Printf.printf "%s\n" (show_c () (`B (`A (`D "4"))));
    Printf.printf "%s\n" (show_c () (`E 18) )

end
