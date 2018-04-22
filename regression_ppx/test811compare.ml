(* module T1 = struct
 * type ('a,'b) t = A of 'a | B of 'b * int
 * [@@deriving gt ~show ~compare]
 *
 * let () =
 *   let cmp1 x y = compare_t (GT.compare GT.int) (GT.compare GT.string) x y in
 *   assert (GT.EQ =  cmp1 (A 5) (A 5) );
 *   assert (GT.EQ =  cmp1 (B ("",5)) (B ("",5))  );
 *   assert (GT.EQ <> cmp1 (A 5) (B ("",5)) );
 *   assert (GT.LT =  cmp1 (A 5) (B ("",5)) );
 *   assert (GT.GT =  cmp1 (B ("",5))  (A 5));
 *   ()
 * end
 *
 * (\* testing polymorphic variants *\)
 * module T2 = struct
 * type 'b t2 = [ `A | `B of 'b * int ]
 * [@@deriving gt ~show ~compare]
 *
 * let () =
 *   let cmp1 x y = compare_t2 (GT.compare GT.string) x y in
 *   assert (GT.EQ =  cmp1 `A           `A );
 *   assert (GT.EQ =  cmp1 (`B ("",5)) (`B ("",5)) );
 *   assert (GT.EQ <> cmp1 `A          (`B ("",5)) );
 *   (\* I'm not sure why the answer is not LT here *\)
 *   assert (GT.EQ <>  cmp1 `A          (`B ("",5)) );
 *   assert (GT.EQ <>  cmp1 (`B ("",5))  `A );
 *   ()
 * end *)

module T3 = struct
  type 'a t = { q: int; w: string; e: 'a GT.list }
  (* [@@deriving gt ~show ~compare] *)

    class virtual ['a,'ia,'sa,'inh,'syn,'extra] class_t =
      object method virtual  do_t : 'inh -> 'a t -> 'syn end
    let gcata_t tr inh t = tr#do_t inh t
    class ['a,'extra] show_t fself  fa =
      object
        inherit  ['a,unit,string,unit,string,'extra] class_t
        constraint 'inh = unit
        method do_t () { q; w; e } =
          Format.sprintf "{  q=%s; w=%s; e=%s; }"
            ((fun subj  -> (let open GT in int.GT.plugins)#show subj) q)
            ((fun subj  -> (let open GT in string.GT.plugins)#show subj) w)
            ((fun subj  -> ((let open GT in GT.list.GT.plugins)#show fa) subj)
               e)
      end
    let rec show_t fa subj =
      GT.fix0 (fun self  -> (gcata_t ((new show_t) self fa)) ()) subj
    class ['a] compare_t fself  fa =
      object
        inherit  ['a,'a,GT.comparison,'a t,GT.comparison,'extra] class_t
        method do_t inh { q; w; e } =
          (((fun inh  ->
               fun subj  -> (let open GT in int.GT.plugins)#compare inh subj)
              inh.q q)
             &&
             ((fun inh  ->
                 fun subj  ->
                   (let open GT in string.GT.plugins)#compare inh subj)
                inh.w w))
            &&
            ((fun inh  ->
                fun subj  ->
                  ((let open GT in GT.list.GT.plugins)#compare fa) inh subj)
               inh.e e)
      end
    let rec compare_t fa the_init subj =
      GT.fix0 (fun self  -> gcata_t ((new compare_t) self fa)) the_init subj
    let t =
      {
        GT.gcata = gcata_t;
        GT.plugins =
          (object method show = show_t method compare = compare_t end)
      }
    let ()  =
    let cmp1 x y = compare_t (GT.compare GT.string) x y in
    let a = { q=5; w="asd"; e= [""] } in
    assert (true  = cmp1 a a)
end
