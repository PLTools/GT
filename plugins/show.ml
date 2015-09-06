#load "q_MLast.cmo";;

open Pa_gt.Plugin
open Printf
open List

let _ =
  register "show" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in
       H.(
        {
          inh_t       = T.id "unit"; 
          syn_t       = T.id "string";
          proper_args = d.type_args; 
          fixed_inh   = Some <:expr< () >>;
          sname       = (fun _ -> T.id "string");
          iname       = (fun _ -> T.id "unit")
        }, 
	let wrap_id l = map (fun (x, y) -> x, y, (fun x -> x)) l in
        let (@@) x y = E.app [E.lid "^"; x; y] in
        let rec body env start stop delim args = 
          (snd 
	     (fold_left 
		(fun (first, expr as acc) arg ->
                  let append e = false, expr @@ (if first then e else (E.str ", ") @@ e) in
                  match arg with                     
		  | arg, (Variable _ | Self _), wrapper -> 
		      append (wrapper <:expr< $E.lid arg$.GT.fx () >>)
		  | arg, Tuple (_, elems), wrapper ->
		      let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in			
		      append (
                        <:expr<
                           let $P.tuple (map P.id args)$ = $E.lid arg$ in
			   $wrapper (body env (E.str "(") (E.str ")") (E.str ", ") (wrap_id (combine args elems)))$
                        >>
		      )
                  | arg, typ, wrapper -> 
		      (match env.trait "show" typ with
		       | Some e -> append (wrapper <:expr< $e$ () $E.lid arg$>>)
		       | None   -> acc
		      )
		)         
		(true, start)
                args 
	     )
          ) @@ stop
	in
        object
	  inherit generator
	  method record env fields = 
            body env (E.str "{") (E.str "}") (E.str "; ") (map (fun (a, (f, _, t)) -> a, t, (fun x -> (E.str (f ^ "=")) @@ x)) fields)
	  method tuple env elems = 
	    body env (E.str "(") (E.str ")") (E.str ", ") (wrap_id elems)
	  method constructor env name args = 
	    body env (E.str ((if d.is_polyvar then "`" else "") ^ name ^ " (")) (E.str ")") (E.str ", ") (wrap_id args)
	end
     )
    )
    
