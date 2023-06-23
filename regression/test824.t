  $ ../ppx/pp_gt.exe test824mut.ml
  module MyIdent =
    struct
      type nonrec t = GT.string[@@deriving gt ~options:{ fmt }]
      include
        struct
          let _ = fun (_ : t) -> ()
          class virtual ['inh,'extra,'syn] t_t =
            object inherit  ['inh,'extra,'syn] GT.string_t end
          let gcata_t = GT.gcata_string
          let _ = gcata_t
          class ['extra_t] fmt_t_t _fself_t =
            object
              inherit  [Format.formatter,'extra_t,unit] t_t
              constraint 'extra_t = t
              inherit  ((['extra_t] GT.fmt_string_t) _fself_t)
            end
          let fmt_t inh___001_ subj___002_ =
            GT.fmt GT.string inh___001_ subj___002_
          let _ = fmt_t
          let t =
            {
              GT.gcata = gcata_t;
              GT.fix = (fun eta -> GT.transform_gc gcata_t eta);
              GT.plugins = (object method fmt = fmt_t end)
            }
          let _ = t
        end[@@ocaml.doc "@inline"][@@merlin.hide ]
    end
  type api = (MyIdent.t * term) GT.list
  and term =
    | LI of heap GT.option * MyIdent.t 
    | CInt of GT.int 
    | BinOp of term * term 
    | Unit 
    | Call of term * term 
    | Union of (pf * term) GT.list 
    | Lambda of
    {
    lam_argname: MyIdent.t GT.option ;
    lam_api: api ;
    lam_eff: heap ;
    lam_body: term ;
    lam_is_rec: GT.bool } 
  and t =
    | HAssoc of (MyIdent.t * term) GT.list 
    | HMerge of (pf * t) GT.list 
    | HCmps of heap * heap 
    | HCall of term * term 
    | HEmpty 
  and pf =
    | LogicBinOp of pf * pf 
    | Not of pf 
    | EQident of MyIdent.t * MyIdent.t 
    | PFTrue 
    | PFFalse 
    | Term of term 
  and heap = t[@@deriving gt ~options:{ fmt }]
  include
    struct
      let _ = fun (_ : api) -> ()
      let _ = fun (_ : term) -> ()
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : pf) -> ()
      let _ = fun (_ : heap) -> ()
      class virtual ['inh,'extra,'syn] api_t =
        object
          inherit 
            [(MyIdent.t * term),(MyIdent.t * term),(MyIdent.t * term),'inh,
            'extra,'syn] GT.list_t
        end
      class virtual ['inh,'extra,'syn] pf_t =
        object
          method virtual  c_LogicBinOp : 'inh -> 'extra -> pf -> pf -> 'syn
          method virtual  c_Not : 'inh -> 'extra -> pf -> 'syn
          method virtual  c_EQident :
            'inh -> 'extra -> MyIdent.t -> MyIdent.t -> 'syn
          method virtual  c_PFTrue : 'inh -> 'extra -> 'syn
          method virtual  c_PFFalse : 'inh -> 'extra -> 'syn
          method virtual  c_Term : 'inh -> 'extra -> term -> 'syn
        end
      class virtual ['inh,'extra,'syn] t_t =
        object
          method virtual  c_HAssoc :
            'inh -> 'extra -> (MyIdent.t * term) GT.list -> 'syn
          method virtual  c_HMerge : 'inh -> 'extra -> (pf * t) GT.list -> 'syn
          method virtual  c_HCmps : 'inh -> 'extra -> heap -> heap -> 'syn
          method virtual  c_HCall : 'inh -> 'extra -> term -> term -> 'syn
          method virtual  c_HEmpty : 'inh -> 'extra -> 'syn
        end
      class virtual ['inh,'extra,'syn] term_t =
        object
          method virtual  c_LI :
            'inh -> 'extra -> heap GT.option -> MyIdent.t -> 'syn
          method virtual  c_CInt : 'inh -> 'extra -> GT.int -> 'syn
          method virtual  c_BinOp : 'inh -> 'extra -> term -> term -> 'syn
          method virtual  c_Unit : 'inh -> 'extra -> 'syn
          method virtual  c_Call : 'inh -> 'extra -> term -> term -> 'syn
          method virtual  c_Union :
            'inh -> 'extra -> (pf * term) GT.list -> 'syn
          method virtual  c_Lambda :
            'inh ->
              'extra ->
                MyIdent.t GT.option -> api -> heap -> term -> GT.bool -> 'syn
        end
      class virtual ['inh,'extra,'syn] heap_t =
        object inherit  ['inh,'extra,'syn] t_t end
      let gcata_api = GT.gcata_list
      let _ = gcata_api
      let gcata_pf (tr : (_,pf,_)#pf_t) inh subj =
        match subj with
        | LogicBinOp (_x__003_, _x__004_) ->
            tr#c_LogicBinOp inh subj _x__003_ _x__004_
        | Not _x__005_ -> tr#c_Not inh subj _x__005_
        | EQident (_x__006_, _x__007_) ->
            tr#c_EQident inh subj _x__006_ _x__007_
        | PFTrue -> tr#c_PFTrue inh subj
        | PFFalse -> tr#c_PFFalse inh subj
        | Term _x__008_ -> tr#c_Term inh subj _x__008_
      let _ = gcata_pf
      let gcata_t (tr : (_,t,_)#t_t) inh subj =
        match subj with
        | HAssoc _x__009_ -> tr#c_HAssoc inh subj _x__009_
        | HMerge _x__010_ -> tr#c_HMerge inh subj _x__010_
        | HCmps (_x__011_, _x__012_) -> tr#c_HCmps inh subj _x__011_ _x__012_
        | HCall (_x__013_, _x__014_) -> tr#c_HCall inh subj _x__013_ _x__014_
        | HEmpty -> tr#c_HEmpty inh subj
      let _ = gcata_t
      let gcata_term (tr : (_,term,_)#term_t) inh subj =
        match subj with
        | LI (_x__015_, _x__016_) -> tr#c_LI inh subj _x__015_ _x__016_
        | CInt _x__017_ -> tr#c_CInt inh subj _x__017_
        | BinOp (_x__018_, _x__019_) -> tr#c_BinOp inh subj _x__018_ _x__019_
        | Unit -> tr#c_Unit inh subj
        | Call (_x__020_, _x__021_) -> tr#c_Call inh subj _x__020_ _x__021_
        | Union _x__022_ -> tr#c_Union inh subj _x__022_
        | Lambda
            { lam_argname = _x__023_; lam_api = _x__024_; lam_eff = _x__025_;
              lam_body = _x__026_; lam_is_rec = _x__027_ }
            ->
            tr#c_Lambda inh subj _x__023_ _x__024_ _x__025_ _x__026_ _x__027_
      let _ = gcata_term
      let gcata_heap = gcata_t
      let _ = gcata_heap
      let fix_api_heap_pf_t_term api0 pf0 t0 term0 heap0 =
        let rec traitapi inh subj =
          gcata_api (api0 (traitapi, traitpf, traitt, traitterm, traitheap))
            inh subj
        and traitpf inh subj =
          gcata_pf (pf0 (traitapi, traitpf, traitt, traitterm, traitheap)) inh
            subj
        and traitt inh subj =
          gcata_t (t0 (traitapi, traitpf, traitt, traitterm, traitheap)) inh
            subj
        and traitterm inh subj =
          gcata_term (term0 (traitapi, traitpf, traitt, traitterm, traitheap))
            inh subj
        and traitheap inh subj =
          gcata_heap (heap0 (traitapi, traitpf, traitt, traitterm, traitheap))
            inh subj in
        (traitapi, traitpf, traitt, traitterm, traitheap)
      let _ = fix_api_heap_pf_t_term
      class ['extra_api] fmt_api_t_stub ((_fself_api, _, _, fmt_term, _) as
                                           _mutuals_pack)
        =
        object
          inherit  [Format.formatter,'extra_api,unit] api_t
          constraint 'extra_api = api
          inherit  (([(MyIdent.t * term),'extra_api] GT.fmt_list_t)
            (fun _x__033_ ->
               fun (_x__034_, _x__035_) ->
                 Format.fprintf _x__033_ "@[(@,%a,@,@ %a@,)@]"
                   (fun inh___036_ ->
                      fun subj___037_ ->
                        GT.fmt MyIdent.t inh___036_ subj___037_) _x__034_
                   fmt_term _x__035_)
            _fself_api)
        end
      class ['extra_pf] fmt_pf_t_stub ((_, _fself_pf, _, fmt_term, _) as
                                         _mutuals_pack)
        =
        object
          inherit  [Format.formatter,'extra_pf,unit] pf_t
          constraint 'extra_pf = pf
          method c_LogicBinOp inh___038_ _ _x__039_ _x__040_ =
            Format.fprintf inh___038_ "LogicBinOp @[(@,%a,@,@ %a@,)@]"
              _fself_pf _x__039_ _fself_pf _x__040_
          method c_Not inh___041_ _ _x__042_ =
            Format.fprintf inh___041_ "Not @[(@,%a@,)@]" _fself_pf _x__042_
          method c_EQident inh___043_ _ _x__044_ _x__045_ =
            Format.fprintf inh___043_ "EQident @[(@,%a,@,@ %a@,)@]"
              (fun inh___046_ ->
                 fun subj___047_ -> GT.fmt MyIdent.t inh___046_ subj___047_)
              _x__044_
              (fun inh___048_ ->
                 fun subj___049_ -> GT.fmt MyIdent.t inh___048_ subj___049_)
              _x__045_
          method c_PFTrue inh___050_ _ = Format.fprintf inh___050_ "PFTrue"
          method c_PFFalse inh___051_ _ = Format.fprintf inh___051_ "PFFalse"
          method c_Term inh___052_ _ _x__053_ =
            Format.fprintf inh___052_ "Term @[(@,%a@,)@]" fmt_term _x__053_
        end
      class ['extra_t] fmt_t_t_stub ((_, fmt_pf, _fself_t, fmt_term, fmt_heap)
                                       as _mutuals_pack)
        =
        object
          inherit  [Format.formatter,'extra_t,unit] t_t
          constraint 'extra_t = t
          method c_HAssoc inh___054_ _ _x__055_ =
            Format.fprintf inh___054_ "HAssoc @[(@,%a@,)@]"
              (fun inh___056_ ->
                 fun subj___057_ ->
                   GT.fmt GT.list
                     (fun _x__058_ ->
                        fun (_x__059_, _x__060_) ->
                          Format.fprintf _x__058_ "@[(@,%a,@,@ %a@,)@]"
                            (fun inh___061_ ->
                               fun subj___062_ ->
                                 GT.fmt MyIdent.t inh___061_ subj___062_)
                            _x__059_ fmt_term _x__060_) inh___056_ subj___057_)
              _x__055_
          method c_HMerge inh___063_ _ _x__064_ =
            Format.fprintf inh___063_ "HMerge @[(@,%a@,)@]"
              (fun inh___065_ ->
                 fun subj___066_ ->
                   GT.fmt GT.list
                     (fun _x__067_ ->
                        fun (_x__068_, _x__069_) ->
                          Format.fprintf _x__067_ "@[(@,%a,@,@ %a@,)@]" fmt_pf
                            _x__068_ _fself_t _x__069_) inh___065_ subj___066_)
              _x__064_
          method c_HCmps inh___070_ _ _x__071_ _x__072_ =
            Format.fprintf inh___070_ "HCmps @[(@,%a,@,@ %a@,)@]" fmt_heap
              _x__071_ fmt_heap _x__072_
          method c_HCall inh___073_ _ _x__074_ _x__075_ =
            Format.fprintf inh___073_ "HCall @[(@,%a,@,@ %a@,)@]" fmt_term
              _x__074_ fmt_term _x__075_
          method c_HEmpty inh___076_ _ = Format.fprintf inh___076_ "HEmpty"
        end
      class ['extra_term] fmt_term_t_stub ((fmt_api, fmt_pf, _, _fself_term,
                                            fmt_heap) as _mutuals_pack)
        =
        object
          inherit  [Format.formatter,'extra_term,unit] term_t
          constraint 'extra_term = term
          method c_LI inh___077_ _ _x__078_ _x__079_ =
            Format.fprintf inh___077_ "LI @[(@,%a,@,@ %a@,)@]"
              (fun inh___080_ ->
                 fun subj___081_ ->
                   GT.fmt GT.option fmt_heap inh___080_ subj___081_) _x__078_
              (fun inh___082_ ->
                 fun subj___083_ -> GT.fmt MyIdent.t inh___082_ subj___083_)
              _x__079_
          method c_CInt inh___084_ _ _x__085_ =
            Format.fprintf inh___084_ "CInt @[(@,%a@,)@]"
              (fun inh___086_ ->
                 fun subj___087_ -> GT.fmt GT.int inh___086_ subj___087_)
              _x__085_
          method c_BinOp inh___088_ _ _x__089_ _x__090_ =
            Format.fprintf inh___088_ "BinOp @[(@,%a,@,@ %a@,)@]" _fself_term
              _x__089_ _fself_term _x__090_
          method c_Unit inh___091_ _ = Format.fprintf inh___091_ "Unit"
          method c_Call inh___092_ _ _x__093_ _x__094_ =
            Format.fprintf inh___092_ "Call @[(@,%a,@,@ %a@,)@]" _fself_term
              _x__093_ _fself_term _x__094_
          method c_Union inh___095_ _ _x__096_ =
            Format.fprintf inh___095_ "Union @[(@,%a@,)@]"
              (fun inh___097_ ->
                 fun subj___098_ ->
                   GT.fmt GT.list
                     (fun _x__099_ ->
                        fun (_x__100_, _x__101_) ->
                          Format.fprintf _x__099_ "@[(@,%a,@,@ %a@,)@]" fmt_pf
                            _x__100_ _fself_term _x__101_) inh___097_
                     subj___098_) _x__096_
          method c_Lambda inh___102_ _ _x__103_ _x__104_ _x__105_ _x__106_
            _x__107_ =
            Format.fprintf inh___102_
              "Lambda {@[<hov>@,@ @,@[lam_argname@,=@,%a;@]@,@ @,@[lam_api@,=@,%a;@]@,@ @,@[lam_eff@,=@,%a;@]@,@ @,@[lam_body@,=@,%a;@]@,@ @,@[lam_is_rec@,=@,%a;@]@]@ }@,"
              (fun inh___108_ ->
                 fun subj___109_ ->
                   GT.fmt GT.option
                     (fun inh___110_ ->
                        fun subj___111_ ->
                          GT.fmt MyIdent.t inh___110_ subj___111_) inh___108_
                     subj___109_) _x__103_ fmt_api _x__104_ fmt_heap _x__105_
              _fself_term _x__106_
              (fun inh___112_ ->
                 fun subj___113_ -> GT.fmt GT.bool inh___112_ subj___113_)
              _x__107_
        end
      class ['extra_heap] fmt_heap_t_stub ((_, _, fmt_t, _, _fself_heap) as
                                             _mutuals_pack)
        = let _ = fmt_t in let _ = _fself_heap in
        object
          inherit  [Format.formatter,'extra_heap,unit] heap_t
          constraint 'extra_heap = heap
          inherit  ((['extra_heap] fmt_t_t_stub) _mutuals_pack)
        end
      let fmt_api_0 = new fmt_api_t_stub
      let _ = fmt_api_0
      let fmt_pf_0 = new fmt_pf_t_stub
      let _ = fmt_pf_0
      let fmt_t_0 = new fmt_t_t_stub
      let _ = fmt_t_0
      let fmt_term_0 = new fmt_term_t_stub
      let _ = fmt_term_0
      let fmt_heap_0 = new fmt_heap_t_stub
      let _ = fmt_heap_0
      let fmt_api eta__028_ =
        let (f, _, _, _, _) =
          fix_api_heap_pf_t_term fmt_api_0 fmt_pf_0 fmt_t_0 fmt_term_0
            fmt_heap_0 in
        f eta__028_
      let _ = fmt_api
      let fmt_pf eta__029_ =
        let (_, f, _, _, _) =
          fix_api_heap_pf_t_term fmt_api_0 fmt_pf_0 fmt_t_0 fmt_term_0
            fmt_heap_0 in
        f eta__029_
      let _ = fmt_pf
      let fmt_t eta__030_ =
        let (_, _, f, _, _) =
          fix_api_heap_pf_t_term fmt_api_0 fmt_pf_0 fmt_t_0 fmt_term_0
            fmt_heap_0 in
        f eta__030_
      let _ = fmt_t
      let fmt_term eta__031_ =
        let (_, _, _, f, _) =
          fix_api_heap_pf_t_term fmt_api_0 fmt_pf_0 fmt_t_0 fmt_term_0
            fmt_heap_0 in
        f eta__031_
      let _ = fmt_term
      let fmt_heap eta__032_ =
        let (_, _, _, _, f) =
          fix_api_heap_pf_t_term fmt_api_0 fmt_pf_0 fmt_t_0 fmt_term_0
            fmt_heap_0 in
        f eta__032_
      let _ = fmt_heap
      class ['extra_api] fmt_api_t _ =
        object
          inherit  ((['extra_api] fmt_api_t_stub)
            (fmt_api, fmt_pf, fmt_t, fmt_term, fmt_heap))
        end
      class ['extra_pf] fmt_pf_t _ =
        object
          inherit  ((['extra_pf] fmt_pf_t_stub)
            (fmt_api, fmt_pf, fmt_t, fmt_term, fmt_heap))
        end
      class ['extra_t] fmt_t_t _ =
        object
          inherit  ((['extra_t] fmt_t_t_stub)
            (fmt_api, fmt_pf, fmt_t, fmt_term, fmt_heap))
        end
      class ['extra_term] fmt_term_t _ =
        object
          inherit  ((['extra_term] fmt_term_t_stub)
            (fmt_api, fmt_pf, fmt_t, fmt_term, fmt_heap))
        end
      class ['extra_heap] fmt_heap_t _ =
        object
          inherit  ((['extra_heap] fmt_heap_t_stub)
            (fmt_api, fmt_pf, fmt_t, fmt_term, fmt_heap))
        end
      let api =
        {
          GT.gcata = gcata_api;
          GT.fix = fix_api_heap_pf_t_term;
          GT.plugins = (object method fmt = fmt_api end)
        }
      let _ = api
      let pf =
        {
          GT.gcata = gcata_pf;
          GT.fix = fix_api_heap_pf_t_term;
          GT.plugins = (object method fmt = fmt_pf end)
        }
      let _ = pf
      let t =
        {
          GT.gcata = gcata_t;
          GT.fix = fix_api_heap_pf_t_term;
          GT.plugins = (object method fmt = fmt_t end)
        }
      let _ = t
      let term =
        {
          GT.gcata = gcata_term;
          GT.fix = fix_api_heap_pf_t_term;
          GT.plugins = (object method fmt = fmt_term end)
        }
      let _ = term
      let heap =
        {
          GT.gcata = gcata_heap;
          GT.fix = fix_api_heap_pf_t_term;
          GT.plugins = (object method fmt = fmt_heap end)
        }
      let _ = heap
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let api1 =
    [("gl",
       (Lambda
          {
            lam_api = [];
            lam_argname = None;
            lam_eff = HEmpty;
            lam_body = (CInt 5);
            lam_is_rec = true
          }));
    ("gl",
      (Lambda
         {
           lam_api = [];
           lam_argname = None;
           lam_eff = HEmpty;
           lam_body = (CInt 5);
           lam_is_rec = true
         }));
    ("gl",
      (Lambda
         {
           lam_api = [];
           lam_argname = None;
           lam_eff = HEmpty;
           lam_body = (CInt 5);
           lam_is_rec = true
         }));
    ("gl",
      (Lambda
         {
           lam_api = [];
           lam_argname = None;
           lam_eff = HEmpty;
           lam_body = (CInt 5);
           lam_is_rec = true
         }))]
  let () = (GT.fmt api) Format.std_formatter api1
  $ ./test824mut.exe
  [ ("gl",
    Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty; lam_body=CInt (5); 
            lam_is_rec=true;
    }); ("gl",
        Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty; 
                lam_body=CInt (5); lam_is_rec=true;
        }); ("gl",
            Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty; 
                    lam_body=CInt (5); lam_is_rec=true;
            }); ("gl",
                Lambda { lam_argname=None; lam_api=[]; lam_eff=HEmpty; 
                        lam_body=CInt (5); lam_is_rec=true;
                })]
