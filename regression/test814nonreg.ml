open Printf

(* module FixV2(Sym: sig type ('a,'b) i end) =
 * struct
 *   type fn = { call: 'a 'b . ('a,'b) Sym.i -> 'a }
 *
 *   let fixv f = let rec g = { call = fun x -> (f g).call x } in g
 * end *)

(* type 'a s = SS of 'a
 * and t = int s
 * and u = float s
 * [@@deriving gt ~options:{show}] *)

(* module X = struct
 *   type ('a,'b) x = X1 of 'a | X2 of ('a,'b) x | X3 of ('b,'a) x
 *   [@@deriving gt ~options:{show}]
 *
 *   (\* let (_:int) = show_x *\)
 *
 *   let () =
 *     let p x = printf "%s\n%!" @@ show_x (GT.show GT.int) (GT.show GT.float) x in
 *     p (X1 1);
 *     p (X3 (X3 (X1 1)));
 * end *)

module Y = struct
  type 'a s = SS of 'a
  and t = GT.int s
  and u = GT.float s
  [@@deriving gt ~options:{show; }]


  let () =
    let () = printf "%s\n%!" @@ GT.(show s) (sprintf "%S") (SS "asdf") in
    let () = printf "%s\n%!" @@ GT.(show s) (sprintf "%b") (SS true) in
    let () = printf "%s\n%!" @@ GT.(show t) (SS 42) in
    let () = printf "%s\n%!" @@ GT.(show u) (SS 3.1415) in
    ()

  (* class ['a,'extra_s] show_s2_t _ fa _ = object
   *   inherit  [unit,'a,string,unit,'extra_s,string] s_t
   *   method c_SS () _ x = Printf.sprintf "ZE_STRING \"%a\"" fa x
   * end
   * let show_s2 call fa () subj =
   *   GT.transform_gc gcata_s (new show_s2_t call fa) () subj
   *
   *
   * let show = fixv (fun f ->
   *     { call = fun (type a) (sym : a I.i) : a ->
   *           match sym with
   *           | I.S -> show_s2 f
   *           | I.U -> show_u_0  f
   *           | I.T -> show_t_0  f
   *     })
   *
   * let show_s fa = show.call I.S (GT.lift fa) ()
   *
   * let () =
   *   printf "After overriding a method\n%!";
   *   let () = printf "%s\n%!" @@ show_s (sprintf "%s") (SS "ZZZZZZZ") in
   *   () *)
end
