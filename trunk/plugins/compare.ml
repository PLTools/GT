#load "q_MLast.cmo";;

open Pa_gt.Plugin
open List
open Printf
open MLast
open Ploc 
open Pcaml
exception Found of int
let _ =
  register "compare" 
    (fun loc d -> 
       let module H = Helper (struct let loc = loc end) in       
       let ng   = name_generator d.type_args in
       let self = ng#generate "self" in
       H.(                
        {
          inh_t       = if d.is_polyvar 
                        then T.app (T.id (type_open_t d.name) :: map T.var (self :: d.type_args)) 
                        else T.app (T.id d.name :: map T.var d.type_args);
          syn_t       = <:ctyp< GT.comparison >>;
          proper_args = if d.is_polyvar then self :: d.type_args else d.type_args;
          sname       = (fun _ -> <:ctyp< GT.comparison >>);
          iname       = (fun a -> T.var a)
        }, 
	let rec many env arg args =
	  fold_left
	    (fun acc (b, typ) ->
	       let body = 
		 match typ with
		 | Instance (_, _, qname) ->
		     (match env.trait "compare" typ with
		      | None   -> <:expr< GT.EQ >>
		      | Some e -> 
			  let rec name = function
			    | [n]  -> <:expr< $e$ ($E.id (arg b)$) $E.id b$ >> 
			    | _::t -> name t
			  in
			  name qname
		     )
                 | Variable  (_, a) -> <:expr< $E.id b$.GT.fx ($E.id (arg b)$) >> 
		 | Self      _      -> <:expr< $E.id b$.GT.fx ($E.id (arg b)$) >>
		 | Arbitrary _      -> <:expr< GT.EQ >>
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
	       in			
	       <:expr< GT.chain_compare $acc$ (fun _ -> $body$) >>
	    )
	    <:expr< GT.EQ >>
	    args
	in
        object
	  inherit generator
	  method record env fields = 
	    let args   = map (fun a -> a, env.new_name a) (map fst fields) in
	    let arg  a = assoc a args in
	    let branch = many env arg (map (fun (a, (_, _, t)) -> a, t) fields) in
            <:expr< match $E.id env.inh$ with 
                    | $P.record (map (fun (a, (f, _, _)) -> P.id f, P.id (arg a)) fields)$ -> $branch$
                    end
            >>

	  method tuple env elems  = 
	    let args   = map (fun a -> a, env.new_name a) (map fst elems) in
	    let arg  a = assoc a args in
	    let branch = many env arg elems in
            <:expr< match $E.id env.inh$ with
                    | $P.tuple (map (fun (_, a) -> P.id a) args)$ -> $branch$
                    end
            >>

	  method constructor env name cargs =
	    let other  = env.new_name "other" in
	    let args   = map (fun a -> a, env.new_name a) (map fst cargs) in
	    let arg  a = assoc a args in
	    let branch = many env arg cargs in
            <:expr< match $E.id env.inh$ with
                    | $P.app (((if d.is_polyvar then P.variant else P.uid) name)::(map (fun (_, a) -> P.id a) args))$ -> $branch$
                    | $P.id other$ -> GT.$E.id (if d.is_polyvar then "compare_poly" else "compare_vari")$ $E.id other$ $E.id env.subj$.GT.x
                    end
            >>
	end
       )
    )
