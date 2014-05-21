#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf

let _ =
  register "foldl" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(
        let gen   = name_generator (d.name::d.type_args) in
	let syn   = gen#generate "syn" in
        {
          inh_t       = `Mono (T.var syn); 
          syn_t       = T.var syn;
          proper_args = d.type_args @ [syn];
          arg_img     = (fun _ -> T.var syn)
        }, 
	let rec body env args =
	  fold_left
            (fun inh (arg, typ) ->
	      let arg = E.id arg in
	      match typ with
	      | Variable _ | Self _ -> E.app [E.gt_fx arg; inh]
	      | Tuple (_, elems) -> 
		  let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in					
		  E.let_nrec 
		    [P.tuple (map P.id args), arg]
		    (body env (combine args elems))
	      | _ ->
		  match env.trait "foldl" typ with
		  | None   -> inh
		  | Some e -> E.app [e; inh; arg]
	    )
            (E.id env.inh)
	    args
	in
        object
	  inherit generator
	  method record      env fields    = body env (map (fun (n, (_, _, t)) -> n, t) fields)
	  method tuple       env elems     = body env elems
	  method constructor env name args = body env args
	end
       )
    )
