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
	let imgs  = map (fun a -> gen#generate (sarg a)) d.type_args in
        let self  = gen#generate "self" in
        let targs = combine d.type_args imgs in
        {
          inh_t       = <:ctyp< unit >>; 
          syn_t       = if d.is_polyvar
                        then T.app (T.id (type_open_t d.name)::T.var self::map T.var imgs)
                        else T.app (T.id d.name::map T.var imgs);
          fixed_inh   = Some <:expr< () >>;
          proper_args = (flatten (map (fun (x, y) -> [x; y]) targs) @ if d.is_polyvar then [self] else []);
          iname       = (fun _ -> T.id "unit"); 
          sname       = (fun a -> 
	                   try T.var (assoc a targs) 
	                   with Not_found -> 
			     raise (Generic_extension (sprintf "arg_img not found (%s)" a))
			)
        }, 
	let rec map_arg env = function 
	| arg, (Variable _ | Self _) -> <:expr< $E.lid arg$.GT.fx () >> 
	| arg, Tuple (_, elems) -> 
	    let args = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in
	    <:expr<
               let $P.tuple (map P.id args)$ = $E.id arg$ in
               $E.tuple (map (map_arg env) (combine args elems))$
            >>
	| arg, typ ->
	    (match env.trait "map" typ with
	     | None   -> E.id arg
	     | Some e -> <:expr< $e$ () $E.id arg$ >>
	    )
	in
        object
	  inherit generator
	  method record env fields =
	    let values = map (map_arg env) (map (fun (n, (_, _, t)) -> n, t) fields) in
	    E.record (combine (map (fun (_, (n, _, _)) -> P.id n) fields) values)

	  method tuple env elems = E.tuple (map (map_arg env) elems)

	  method constructor env name args =
	    E.app (((if d.is_polyvar then E.variant else E.id) name)::
                   map (map_arg env) args
		  )
        end
       )
    )
