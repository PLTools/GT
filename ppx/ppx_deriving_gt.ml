(*
 * Generic Transformers PPX syntax extension.
 * Copyright (C) 2016-2017
 *   Dmitrii Kosarev aka Kakadu
 * St.Petersburg State University, JetBrains Research
 *
 *)

open Ppxlib

open Expander
module E = Expander.Make(PpxHelpers)

let gt_param name =
  let open Deriving.Args in
  arg name __

let r =
  let open Deriving.Args in
  map1 ~f:(List.map (fun ({txt},e) -> (txt, e)) ) @@
  pexp_record __ none

let str_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)
       let generator_f si =
         H.str_type_decl_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) ->
                    (name,Expander.Use [])
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )

let str_type_ext : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)
       let generator_f si =
         H.str_type_ext_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) ->
                    (name,Expander.Use [])
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )


let sig_type_decl : (_, _) Deriving.Generator.t =
  Deriving.Generator.make Deriving.Args.(empty +> arg "options" r)
    (fun ~loc ~path info options ->
       let module H = Expander.Make(PpxHelpers) in
       (* Expander.notify "with annotations %s" (String.concat "," info); *)
       let generator_f si =
         H.sig_type_decl_many_plugins ~loc si
           (match options with
            | None -> []
            | Some xs ->
              List.map (function
                  | (Lident name, e) ->
                    (name,Expander.Use [])
                  | _ -> failwith "only lowercase identifiers are allowed"
                ) xs
           )

       in
       generator_f [] info
    )

let () =
  (* Sys.command "notify-send 'Registering deriver' wtf" |> ignore; *)
  Deriving.add
    ~str_type_decl
    ~sig_type_decl
    ~str_type_ext
    (* ~sig_type_ext *)
    "gt"
  |> Deriving.ignore
