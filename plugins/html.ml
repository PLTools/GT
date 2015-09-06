#load "q_MLast.cmo";;

open Pa_gt.Plugin
open Printf
open List

let _ =
  register "html" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = <:ctyp< unit >>; 
          syn_t       = <:ctyp< HTMLView.viewer >>;
          proper_args = d.type_args; 
          fixed_inh   = Some <:expr< () >>;
          sname       = (fun _ -> <:ctyp< HTMLView.viewer>>);
          iname       = (fun _ -> <:ctyp< unit >>)
        }, 
	let wrap_id l = map (fun (x, y) -> x, y, (fun x -> x)) l in
        let (@@) x y = E.app [<:expr< View.concat >>; x; y] in
        let rec body env tag args = 
          E.app [<:expr< HTMLView.b>>; 
                 E.app [<:expr< HTMLView.string >>; tag]
	  ]              
          @@          
	  (E.app [
             <:expr< HTMLView.ul>>;
             fold_left 
	       (fun expr arg ->
		 let append ?(a=E.str "") e = 
                   let attr = <:expr< ~{$P.id "attrs"$ = $a$} >> in
                   expr @@ E.app [<:expr< HTMLView.li >>; attr; e] 
                 in
		 match arg with                     
		 | arg, Variable _, wrapper -> 
		     expr @@ (wrapper <:expr< $E.lid arg$.GT.fx () >>)
		 | arg, Self _, wrapper -> 
		     append ~a:(<:expr< $E.id env.this$#attribute $E.gt_x (E.id arg)$ >>) (wrapper <:expr< $E.lid arg$.GT.fx () >>)
		 | arg, Tuple (_, elems), wrapper ->
		     let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in			
		     append (
		       <:expr<
                         let $P.tuple (map P.id args)$ = $E.lid arg$ in
	                 $wrapper (body env (E.str "tuple") (wrap_id (combine args elems)))$
		       >>
		    )
		 | arg, typ, wrapper -> 
		     (match env.trait "html" typ with
		     | Some e -> append (wrapper <:expr< $e$ () $E.lid arg$>>)
		     | None   -> expr
		     )
	       )
	     (<:expr< View.empty >>)
             args]
	  )
	in
        object
	  inherit generator
          method custom = 
            let atyp = 
              if d.is_polyvar 
              then <:ctyp< ! $list:["a"]$ . $T.arrow [T.var "a"; T.id "string"]$ >>
              else T.arrow [
                     if List.length d.type_args = 0 
                     then T.id d.name 
                     else T.app ((T.id d.name)::(map T.var d.type_args)); <:ctyp< string >>] 
            in
            let abod = <:expr< fun _ -> "" >> in
            let ntyp = <:ctyp< string -> string >> in
            let nbod = <:expr< fun s -> s>> in
            [<:class_str_item< method $lid:"attribute"$ : $atyp$ = $abod$ >>,
             <:class_sig_item< method $lid:"attribute"$ : $atyp$ >>;

             <:class_str_item< method $lid:"cname"$ : $ntyp$ = $nbod$ >>,
             <:class_sig_item< method $lid:"cname"$ : $ntyp$ >>
            ]
	  method record env fields = 
            body env (E.str "struct") (map (fun (a, (f, _, t)) -> a, t, 
                                        (fun x -> <:expr< View.concat (HTMLView.string $E.str f$) (HTMLView.li $x$) >>))
                                        fields
                                      )
	  method tuple env elems = body env (E.str "tuple") (wrap_id elems)
	  method constructor env name args = 
            let name = (if d.is_polyvar then "`" else "") ^ name in
            let tag  = E.app [<:expr< $E.id env.this$ # $"cname"$ >>; E.str name] in
	    body env tag (wrap_id args)
	end
     )
    )
    
