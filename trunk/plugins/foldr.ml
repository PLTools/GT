#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf

let _ =
  register "foldr" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(
        let gen   = name_generator (d.name::d.type_args) in
	let syn   = gen#generate "syn" in
        {
          inh_t       = T.var syn; 
          syn_t       = T.var syn;
          proper_args = d.type_args @ [syn];
          sname       = (fun _ -> T.var syn);
          iname       = (fun _ -> T.var syn)
        }, 
	let rec body env args =
	  fold_right
            (fun (arg, typ) inh ->
	      let arg = E.id arg in
	      match typ with
	      | Variable _ | Self _ -> <:expr< $arg$.GT.fx $inh$ >>
	      | Tuple (_, elems) -> 
		  let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in					
                  <:expr<
                      let $P.tuple (map P.id args)$ = $arg$ in
                      $body env (combine args elems)$
                  >>
	      | _ ->
		  match env.trait "foldr" typ with
		  | None   -> inh
		  | Some e -> <:expr< $e$ $inh$ $arg$ >>
	    )
	    args
            (E.id env.inh)
	in
        object
	  inherit generator
	  method record      env fields    = body env (map (fun (n, (_, _, t)) -> n, t) fields)
	  method tuple       env elems     = body env elems
	  method constructor env name args = body env args
	end
       )
    )
