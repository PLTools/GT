#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf

let _ =
  register "map" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(
        let gen   = name_generator (d.name::d.type_args) in
	let imgs  = map (fun a -> gen#generate (targ a)) d.type_args in
        let targs = combine d.type_args imgs in
        {
          inh_t       = `Mono (T.id "unit"); 
          syn_t       = T.app (T.id d.name::map T.var imgs);
          proper_args = flatten (map (fun (x, y) -> [x; y]) targs); 
          arg_img     = (fun a -> 
	                   try T.var (assoc a targs) 
	                   with Not_found -> 
			     raise (Generic_extension (sprintf "arg_img not found (%s)" a))
			)
        }, 
        object
	  inherit generator
	  method record env fields = invalid_arg "not supported"
	  method tuple env elems = invalid_arg "not supported"
	  method constructor env name args =
	    E.app (((if d.is_polyvar then E.variant else E.id) name)::
                   map 
	             (function 
		      | arg, (Variable _ | Self _) -> E.app [E.gt_fx (E.lid arg); E.unit]
		      | arg, typ ->
		         (match env.trait "map" typ with
		          | None   -> E.id arg
			  | Some e -> E.app [e; E.unit; E.id arg]
			 )
		     ) 
	             args
		  )
        end
       )
    )
