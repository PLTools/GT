#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf
open MLast
open Ploc 

exception Found of int

let _ =
  register "eq" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       H.(
        let proper_args, inh_t =
	  if d.is_polyvar 
	  then
            let ng   = name_generator d.type_args in
            let self = ng#generate "self" in
	    let args = self :: d.type_args in
	    args,
	    `Poly (T.app (T.id (type_open_t d.name) :: map T.var args), fun x -> T.var x)
	  else
	    d.type_args,
	    `Poly (T.app (T.id d.name :: map T.var d.type_args), fun x -> T.var x)
	in        
        {
          inh_t       = inh_t;
          syn_t       = T.id "bool";
          proper_args = proper_args;
          arg_img     = (fun _ -> T.id "bool")
        }, 
	let rec many env arg args =
	  fold_left
	    (fun acc (b, typ) ->
	      let test = 
		match typ with
		| Instance (_, args, qname) ->
		    (match env.trait "eq" typ with
		     | None   -> <:expr< True >>
		     | Some e -> 
			 let rec name = function
			 | [n]  -> <:expr< $e$ (` $type_tag$ $E.id (arg b)$) $E.id b$ >> 
			 | _::t -> name t
			 in
			 name qname
		    )
		| Tuple (_, elems) ->
		    let args_a = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in
		    let args_b = mapi (fun i _ -> env.new_name (sprintf "e%d" i)) elems in			   
		    let elems  = combine args_a elems in
		    let args   = combine args_a args_b in
		    let arg' a = assoc a args in
                    <:expr<
                      let $P.tuple (map P.id args_a)$ = $E.lid b$       in
                      let $P.tuple (map P.id args_b)$ = $E.lid (arg b)$ in
                      $many env arg' elems$
                    >>

		| Variable (_, a) -> <:expr< $E.id b$.GT.fx (` $d.arg_tag a$ $E.id (arg b)$) >> 
		| Self     _      -> <:expr< $E.id b$.GT.fx (` $type_tag$ $E.id (arg b)$) >>
		| Arbitrary _     -> <:expr< true >>
	      in
	      <:expr< $acc$ && $test$ >>
	    )
	    <:expr< True >>
	  args
	in
        object
	  inherit generator
	  method record env fields = 
	    let args   = map (fun a -> a, env.new_name a) (map fst fields) in
	    let arg  a = assoc a args in
	    let branch = many env arg (map (fun (a, (_, _, t)) -> a, t) fields) in
            <:expr< match $E.id env.inh$ with 
                    | ` $type_tag$ $P.record (map (fun (a, (f, _, _)) -> P.id f, P.id (arg a)) fields)$ -> $branch$ 
                    | _ -> False
                    end
            >>

	  method tuple env elems  = 
	    let args   = map (fun a -> a, env.new_name a) (map fst elems) in
	    let arg  a = assoc a args in
	    let branch = many env arg elems in
            <:expr< match $E.id env.inh$ with
                    | ` $type_tag$ $P.tuple (map (fun (_, a) -> P.id a) args)$ -> $branch$
                    | _ -> False
                    end
            >>

	  method constructor env name cargs =
	    let args   = map (fun a -> a, env.new_name a) (map fst cargs) in
	    let arg  a = assoc a args in
	    let branch = many env arg cargs in	    
	    <:expr< match $E.id env.inh$ with
	            | ` $type_tag$ $P.app (((if d.is_polyvar then P.variant else P.uid) name)::(map (fun (_, a) -> P.id a) args))$ -> $branch$
                    | _ -> False
                    end
            >>
	end
       )
    )
