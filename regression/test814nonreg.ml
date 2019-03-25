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
  (* [@@deriving gt ~options:{show; }] *)

    class virtual ['ia,'a,'sa,'inh,'extra,'syn] s_t =
      object method virtual  c_SS : 'inh -> 'a s -> 'a -> 'syn end
    class virtual ['inh,'extra,'syn] u_t =
      object inherit  [GT.float,GT.float,GT.float,'inh,'extra,'syn] s_t end
    class virtual ['inh,'extra,'syn] t_t =
      object inherit  [GT.int,GT.int,GT.int,'inh,'extra,'syn] s_t end
    let gcata_s tr inh subj =
      match subj with | SS _x__001_ -> tr#c_SS inh subj _x__001_
    let gcata_u = gcata_s
    let gcata_t = gcata_s
    module type IndexResult_s  =
      sig
        type 'a result
        type _ i =
          | S: ('a result -> 'a s result) i
          | T: t result i
          | U: u result i
      end
    module Index_s(S:sig type 'a result end) =
      struct
        type 'a result = 'a S.result
        type _ i =
          | S: ('a result -> 'a s result) i
          | T: t result i
          | U: u result i
      end
    module type IndexResult2_s  =
      sig
        type ('a, 'b) result
        type _ i =
          | S: (('a, 'a2) result -> ('a s, 'a2 s) result) i
          | T: (t, t) result i
          | U: (u, u) result i
      end
    module Index2_s(S:sig type ('a, 'b) result end) =
      struct
        type ('a, 'b) result = ('a, 'b) S.result
        type _ i =
          | S: (('a, 'a2) result -> ('a s, 'a2 s) result) i
          | T: (t, t) result i
          | U: (u, u) result i
      end
    module type IndexResult_fold_s  =
      sig
        type ('a, 'syn) result
        type _ i =
          | S: (('a, 'syn) result -> ('a s, 'syn) result) i
          | T: (t, 'syn) result i
          | U: (u, 'syn) result i
      end
    module Index_fold_s(S:sig type ('a, 'syn) result end) =
      struct
        type ('a, 'syn) result = ('a, 'syn) S.result
        type _ i =
          | S: (('a, 'syn) result -> ('a s, 'syn) result) i
          | T: (t, 'syn) result i
          | U: (u, 'syn) result i
      end
    module type IndexResult_stateful_s  =
      sig
        type ('env, 'a, 'b) result
        type _ i =
          | S: (('env, 'a, 'a2) result -> ('env, 'a s, 'a2 s) result) i
          | T: ('env, t, t) result i
          | U: ('env, u, u) result i
      end
    module Index_stateful_s(S:sig type ('env, 'a, 'b) result end) =
      struct
        type ('env, 'a, 'b) result = ('env, 'a, 'b) S.result
        type _ i =
          | S: (('env, 'a, 'a2) result -> ('env, 'a s, 'a2 s) result) i
          | T: ('env, t, t) result i
          | U: ('env, u, u) result i
      end
    module Ishow_s =
      (Index_s)(struct type 'a result = unit -> 'a -> string end)
    module Fix_show_s = (GT.FixV)(Ishow_s)
    class ['a,'extra_s] show_s_t ({ Fix_show_s.call = call } as _mutuals_pack)
       fa  fself_s =
      object
        inherit  [unit,'a,string,unit,'extra_s,string] s_t
        method c_SS inh___002_ _ _x__003_ =
          Printf.sprintf "SS (%s)" (fa () _x__003_)
      end
    class ['extra_u] show_u_t ({ Fix_show_s.call = call } as _mutuals_pack)
      fself_u =
      object
        inherit  [unit,'extra_u,string] u_t
        inherit  (([GT.float,'extra_u] show_s_t) _mutuals_pack
          (fun () -> fun subj -> GT.show GT.float subj) fself_u)
      end
    class ['extra_t] show_t_t ({ Fix_show_s.call = call } as _mutuals_pack)
      fself_t =
      object
        inherit  [unit,'extra_t,string] t_t
        inherit  (([GT.int,'extra_t] show_s_t) _mutuals_pack
          (fun () -> fun subj -> GT.show GT.int subj) fself_t)
      end
    let show_s_0 call fa inh0 subj =
      GT.transform_gc gcata_s ((new show_s_t) call fa) inh0 subj
    let show_u_0 call inh0 subj =
      GT.transform_gc gcata_u ((new show_u_t) call) inh0 subj
    let show_t_0 call inh0 subj =
      GT.transform_gc gcata_t ((new show_t_t) call) inh0 subj
    let show_s_fix =
      Fix_show_s.fixv
        (fun f ->
           {
             call = fun (type a) ->
               fun (sym : a Ishow_s.i) ->
                 (match sym with
                  | Ishow_s.S -> show_s_0 f
                  | Ishow_s.U -> show_u_0 f
                  | Ishow_s.T -> show_t_0 f : a)
           })
    let s =
      {
        GT.gcata = gcata_s;
        GT.plugins =
          (object
             method show fa subj =
               show_s_fix.call Ishow_s.S (GT.lift fa) () subj
           end)
      }
    let u =
      {
        GT.gcata = gcata_u;
        GT.plugins =
          (object method show subj = show_u_fix.call Ishow_s.U () subj end)
      }
    let t =
      {
        GT.gcata = gcata_t;
        GT.plugins =
          (object method show subj = show_t_fix.call Ishow_s.T () subj end)
      }

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
