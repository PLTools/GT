module T : sig
  (* @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful;;
   * @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;; *)
end = struct
  (* @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful;; *)

  type 'a t3 = GT.int * 'a * GT.string
   (* with show,gmap,foldl,eq,compare,eval,stateful;; *)
    class virtual ['ia,'a,'sa,'inh,'self,'syn] t3_t =
      object
        inherit
          [GT.int,GT.int,GT.int,'ia,'a,'sa,GT.string,GT.string,GT.string,
          'inh,'self,'syn] GT.tuple3_t
      end
    let gcata_t3 = GT.gcata_tuple3

    class ['a,'a_2,'env,'self] stateful_t3_t fself  fa =
      object
        inherit  ['env,'a,('env * 'a_2),'env,'self,('env * 'a_2 t3)] t3_t
        inherit  [GT.int,GT.int,'a,'a_2,GT.string,GT.string,'env,'self] GT.stateful_tuple3_t
            fself
            (* (fun inh -> fun subj -> (GT.int.GT.plugins)#stateful inh subj) *)
            (fun _ -> assert false)
            fa
            (fun inh -> fun subj -> (GT.string.GT.plugins)#stateful inh subj)
      end
    let rec stateful_t3 fa the_init subj =
      GT.fix0 (fun self -> gcata_t3 ((new stateful_t3_t) self fa)) the_init
        subj
    class ['a,'a_2,'self,'env] eval_t3_t fself  fa =
      object
        inherit  ['env,'a,'a_2,'env,'self,'a_2 t3] t3_t
        inherit  (([GT.int,GT.int,'a,'a_2,GT.string,GT.string,'env,'self]
          GT.eval_tuple3_t) fself
          (fun inh -> fun subj -> (GT.int.GT.plugins)#eval inh subj) fa
          (fun inh -> fun subj -> (GT.string.GT.plugins)#eval inh subj))
      end
    let rec eval_t3 fa the_init subj =
      GT.fix0 (fun self -> gcata_t3 ((new eval_t3_t) self fa)) the_init subj
    class ['a,'self] compare_t3_t fself  fa =
      object
        inherit  ['a,'a,GT.comparison,'a t3,'self,GT.comparison] t3_t
        inherit  (([GT.int,'a,GT.string,'self] GT.compare_tuple3_t) fself
          (fun inh -> fun subj -> (GT.int.GT.plugins)#compare inh subj) fa
          (fun inh -> fun subj -> (GT.string.GT.plugins)#compare inh subj))
      end
    let rec compare_t3 fa the_init subj =
      GT.fix0 (fun self -> gcata_t3 ((new compare_t3_t) self fa)) the_init
        subj
    class ['a,'self] eq_t3_t fself  fa =
      object
        inherit  ['a,'a,bool,'a t3,'self,bool] t3_t
        inherit  (([GT.int,'a,GT.string,'self] GT.eq_tuple3_t) fself
          (fun inh -> fun subj -> (GT.int.GT.plugins)#eq inh subj) fa
          (fun inh -> fun subj -> (GT.string.GT.plugins)#eq inh subj))
      end
    let rec eq_t3 fa the_init subj =
      GT.fix0 (fun self -> gcata_t3 ((new eq_t3_t) self fa)) the_init subj
    class ['a,'syn,'self] foldl_t3_t fself  fa =
      object
        inherit  ['syn,'a,'syn,'syn,'self,'syn] t3_t
        inherit  (([GT.int,'a,GT.string,'syn,'self] GT.foldl_tuple3_t) fself
          (fun inh -> fun subj -> (GT.int.GT.plugins)#foldl inh subj) fa
          (fun inh -> fun subj -> (GT.string.GT.plugins)#foldl inh subj))
      end
    let rec foldl_t3 fa the_init subj =
      GT.fix0 (fun self -> gcata_t3 ((new foldl_t3_t) self fa)) the_init subj
    class ['a,'a_2,'self] gmap_t3_t fself  fa =
      object
        inherit  [unit,'a,'a_2,unit,'self,'a_2 t3] t3_t
        inherit  (([GT.int,GT.int,'a,'a_2,GT.string,GT.string,'self]
          GT.gmap_tuple3_t) fself (fun subj -> (GT.int.GT.plugins)#gmap subj)
          fa (fun subj -> (GT.string.GT.plugins)#gmap subj))
      end
    let rec gmap_t3 fa subj =
      GT.fix0 (fun self -> gcata_t3 ((new gmap_t3_t) self fa) ()) subj
    class ['a,'self] show_t3_t fself  fa =
      object
        inherit  [unit,'a,string,unit,'self,string] t3_t
        inherit  (([GT.int,'a,GT.string,'self] GT.show_tuple3_t) fself
          (fun subj -> (GT.int.GT.plugins)#show subj) fa
          (fun subj -> (GT.string.GT.plugins)#show subj))
      end
    let rec show_t3 fa subj =
      GT.fix0 (fun self -> gcata_t3 ((new show_t3_t) self fa) ()) subj
    let t3 =
      {
        GT.gcata = gcata_t3;
        GT.plugins =
          (object
             method stateful = stateful_t3
             method eval = eval_t3
             method compare = compare_t3
             method eq = eq_t3
             method foldl = foldl_t3
             method gmap = gmap_t3
             method show = show_t3
           end)
      }

  (* @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful;;
   *
   * @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful;;
   * @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;; *)
end


