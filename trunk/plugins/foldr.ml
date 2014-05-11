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
          inh_t       = `Mono (T.var syn); 
          syn_t       = T.var syn;
          proper_args = d.type_args @ [syn];
          arg_img     = (fun _ -> T.var syn)
        }, 
        object
	  inherit generator
	  method record env fields = invalid_arg "not supported"
	  method tuple env elems = invalid_arg "not supported"
	  method constructor env name args =
	    fold_right
              (fun (arg, typ) inh ->
		let arg = E.id arg in
		match typ with
		| Variable _ | Self _ -> E.app [E.gt_fx arg; inh]
		| _ ->
		    match env.trait "foldr" typ with
		    | None   -> inh
		    | Some e -> E.app [e; inh; arg]
	      )
	      args
              (E.id env.inh)
	end
       )
    )
