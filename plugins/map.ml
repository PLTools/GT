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
	let rec map_arg env = function 
	| arg, (Variable _ | Self _) -> E.app [E.gt_fx (E.lid arg); E.unit]
	| arg, Tuple (_, elems) -> 
	    let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in
	    E.let_nrec 
	      [P.tuple (map P.id args), E.id arg]
	      (E.tuple (map (map_arg env) (combine args elems)))
	| arg, typ ->
	    (match env.trait "map" typ with
	     | None   -> E.id arg
	     | Some e -> E.app [e; E.unit; E.id arg]
	    )
	in
        object
	  inherit generator
	  method record env fields =
	    let values = map (map_arg env) (map (fun (n, (_, _, t)) -> n, t) fields) in
	    E.record (combine (map (fun (_, (n, _, _)) -> P.id n) fields) values)

	  method tuple env elems = 
	    E.tuple (map (map_arg env) elems)

	  method constructor env name args =
	    E.app (((if d.is_polyvar then E.variant else E.id) name)::
                   map (map_arg env) args
		  )
        end
       )
    )
