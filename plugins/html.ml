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
          sname       = (fun _ -> <:ctyp< HTMLView.viewer>>);
          iname       = (fun _ -> <:ctyp< unit >>)
        }, 
	let wrap_id l = map (fun (x, y) -> x, y, (fun x -> x)) l in
        let (@@) x y = E.app [<:expr< View.concat >>; x; y] in
        let rec body env tag args = 
          E.app [<:expr< HTMLView.b>>; 
                 E.app [<:expr< HTMLView.string >>; E.str tag]
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
		     append (wrapper <:expr< $E.lid arg$.GT.fx () >>)
		 | arg, Self _, wrapper -> 
		     append ~a:(<:expr< this#attribute $E.gt_x (E.id arg)$ >>) (wrapper <:expr< $E.lid arg$.GT.fx () >>)
		 | arg, Tuple (_, elems), wrapper ->
		     let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in			
		     append (
		       <:expr<
                         let $P.tuple (map P.id args)$ = $E.lid arg$ in
	                 $wrapper (body env "tuple" (wrap_id (combine args elems)))$
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
            let atyp = T.arrow [if List.length d.type_args = 0 then T.id d.name else T.app ((T.id d.name)::(map T.id d.type_args)); <:ctyp< string >>] in
            let expr = <:expr< fun _ -> "" >> in
            [<:class_str_item< method $lid:"attribute"$ : $atyp$ = $expr$ >>,
             <:class_sig_item< method $lid:"attribute"$ : $atyp$ >>
            ]
	  method record env fields = 
            body env "struct" (map (fun (a, (f, _, t)) -> a, t, 
                                 (fun x -> <:expr< View.concat (HTMLView.string $E.str f$) (HTMLView.li $x$) >>))
                                 fields
                              )
	  method tuple env elems = body env "tuple" (wrap_id elems)
	  method constructor env name args = 
	    body env ((if d.is_polyvar then "`" else "") ^ name) (wrap_id args)
	end
     )
    )
    
