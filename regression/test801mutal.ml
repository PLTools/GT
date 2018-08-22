open Printf

(* type 'l a = A of b     | C | E of 'l a | D of 'l
 * and     b = I of GT.int a | J | K of b
 * [@@deriving gt ~options:{show;gmap;fmt}]
 *
 *
 * let _ =
 *   printf "Testing Show\n";
 *   printf "%s\n" @@ show_a (GT.show GT.int) (E C);
 *   printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
 *   printf "%s\n" @@ show_b                  (I (A J));
 *   printf "%s\n" @@ show_b                  (K J);
 *
 *   printf "Testing Gmap\n";
 *   printf "%s\n" @@ show_a (GT.show GT.int) @@ gmap_a ((+)1) (D 6);
 *
 *   ()
 *
 * let b =
 *   {
 *     GT.gcata = gcata_b;
 *     GT.plugins = (object method gmap = gmap_b method show = show_b end)
 *   }
 * let a =
 *   {
 *     GT.gcata = gcata_a;
 *     GT.plugins = (object method gmap = gmap_a method show = show_a end)
 *   }
 *
 * (\* ************************************ *\)
 * type c = b GT.list [@@deriving gt ~options:{show;gmap}]
 * type 'a d = 'a GT.list a GT.list  [@@deriving gt ~options:{show;gmap}] *)


type class_type_declaration = class_type class_info
and class_type = C
and 'a class_info = 'a
(* [@@deriving gt ~options:{fmt}] *)

class virtual ['ia,'a,'sa,'inh,'self,'syn] class_info_t =
  object inherit  ['ia,'a,'sa,'inh,'self,'syn] GT.free_t end
class virtual ['inh,'self,'syn] class_type_t =
  object method virtual  c_C : 'inh -> 'syn end
class virtual ['inh,'self,'syn] class_type_declaration_t =
  object
    inherit  [class_type,class_type,class_type,'inh,'self,'syn] class_info_t
  end
let gcata_class_info = GT.gcata_free
let gcata_class_type tr inh subj = match subj with | C -> tr#c_C inh
let gcata_class_type_declaration = gcata_class_info
class ['a,'self_class_info] fmt_class_info_t_stub ofmt_class_type
  ofmt_class_type_declaration  fself  fa =
  object
    inherit
      [Format.formatter,'a,unit,Format.formatter,'self_class_info,unit]
      class_info_t
    inherit  ((['a,'self_class_info] GT.fmt_free_t) fself fa)
  end

class ['self_class_type] fmt_class_type_t_stub ofmt_class_info
  ofmt_class_type_declaration  fself =
  object
    inherit  [Format.formatter,'self_class_type,unit] class_type_t
    method c_C inh___001_ = Format.fprintf inh___001_ "C"
  end

class ['self_class_type_declaration] fmt_class_type_declaration_t_stub ofmt_class_info
   ofmt_class_type  fself =
  object
    inherit  [Format.formatter,'self_class_type_declaration,unit]
      class_type_declaration_t
    inherit  [class_type,'self_class_type_declaration] fmt_class_info_t_stub
        ofmt_class_type (fun () -> assert false) fself
      (gcata_class_type (ofmt_class_type ()))
  end
type nonrec fmt_t_class_info_1 =
  {
  fmt_class_info_trf:
    'a .
      (Format.formatter -> 'a -> unit) ->
        Format.formatter -> 'a class_info -> unit
    }
type nonrec fmt_t_class_info_2 =
  {
  fmt_oclass_info_func:
    'self_class_info 'a .
      unit ->
        (Format.formatter -> 'a -> unit) ->
          ('a, 'self_class_info) fmt_class_info_t_stub
    }
type nonrec fmt_t_class_info_3 =
  {
  fmt_class_info_func:
    'self_class_info 'a 'self_class_type_declaration 'self_class_type .
      (unit -> 'self_class_type fmt_class_type_t_stub) ->
        (unit ->
           'self_class_type_declaration fmt_class_type_declaration_t_stub)
          ->
          (Format.formatter -> 'a class_info -> unit) ->
            (Format.formatter -> 'a -> unit) ->
              ('a, 'self_class_info) fmt_class_info_t_stub
    }
type nonrec fmt_t_class_type_1 =
  {
  fmt_class_type_trf: Format.formatter -> class_type -> unit }
type nonrec fmt_t_class_type_2 =
  {
  fmt_oclass_type_func:
    'self_class_type . unit -> 'self_class_type fmt_class_type_t_stub }
type nonrec fmt_t_class_type_3 =
  {
  fmt_class_type_func:
    'self_class_type 'self_class_type_declaration 'self_class_info 'a .
      (unit ->
         (Format.formatter -> 'a -> unit) ->
           ('a, 'self_class_info) fmt_class_info_t_stub)
        ->
        (unit ->
           'self_class_type_declaration fmt_class_type_declaration_t_stub)
          ->
          (Format.formatter -> class_type -> unit) ->
            'self_class_type fmt_class_type_t_stub
    }
type nonrec fmt_t_class_type_declaration_1 =
  {
  fmt_class_type_declaration_trf:
    Format.formatter -> class_type_declaration -> unit }
type nonrec fmt_t_class_type_declaration_2 =
  {
  fmt_oclass_type_declaration_func:
    'self_class_type_declaration .
      unit -> 'self_class_type_declaration fmt_class_type_declaration_t_stub
    }
type nonrec fmt_t_class_type_declaration_3 =
  {
  fmt_class_type_declaration_func:
    'self_class_type_declaration 'self_class_type 'self_class_info .
      (unit ->
         (Format.formatter -> class_type -> unit) ->
           (class_type, 'self_class_info) fmt_class_info_t_stub)
        ->
        (unit -> 'self_class_type fmt_class_type_t_stub) ->
          (Format.formatter -> class_type_declaration -> unit) ->
            'self_class_type_declaration fmt_class_type_declaration_t_stub
    }
let fmt_fix_class_info_class_type_class_type_declaration
  (class_info0, class_type0, class_type_declaration0) =
  let rec fmt_class_info =
    {
      fmt_class_info_trf =
        (fun fa ->
           fun inh ->
             fun subj ->
               gcata_class_info (oclass_info.fmt_oclass_info_func () fa) inh
                 subj)
    }
  and oclass_info =
    {
      fmt_oclass_info_func =
        (fun () ->
           fun fa ->
             class_info0.fmt_class_info_func oclass_type.fmt_oclass_type_func
               oclass_type_declaration.fmt_oclass_type_declaration_func
               (fmt_class_info.fmt_class_info_trf fa) fa)
    }
  and fmt_class_type =
    {
      fmt_class_type_trf =
        (fun inh ->
           fun subj ->
             gcata_class_type (oclass_type.fmt_oclass_type_func ()) inh subj)
    }
  and oclass_type =
    {
      fmt_oclass_type_func =
        (fun () ->
           class_type0.fmt_class_type_func oclass_info.fmt_oclass_info_func
             oclass_type_declaration.fmt_oclass_type_declaration_func
             fmt_class_type.fmt_class_type_trf)
    }
  and fmt_class_type_declaration =
    {
      fmt_class_type_declaration_trf =
        (fun inh ->
           fun subj ->
             gcata_class_type_declaration
               (oclass_type_declaration.fmt_oclass_type_declaration_func ())
               inh subj)
    }
  and oclass_type_declaration =
    {
      fmt_oclass_type_declaration_func =
        (fun () ->
           class_type_declaration0.fmt_class_type_declaration_func
             oclass_info.fmt_oclass_info_func
             oclass_type.fmt_oclass_type_func
             fmt_class_type_declaration.fmt_class_type_declaration_trf)
    } in
  (fmt_class_info, oclass_info, fmt_class_type, oclass_type,
    fmt_class_type_declaration, oclass_type_declaration)
let (fix_result_class_info, fmt_o_class_info, fix_result_class_type,
     fmt_o_class_type, fix_result_class_type_declaration,
     fmt_o_class_type_declaration)
  =
  fmt_fix_class_info_class_type_class_type_declaration
    ({ fmt_class_info_func = (new fmt_class_info_t_stub) },
      { fmt_class_type_func = (new fmt_class_type_t_stub) },
      {
        fmt_class_type_declaration_func =
          (new fmt_class_type_declaration_t_stub)
      })
let fmt_class_info fa subj = fix_result_class_info.fmt_class_info_trf fa subj
let fmt_class_type subj = fix_result_class_type.fmt_class_type_trf subj
let fmt_class_type_declaration subj =
  fix_result_class_type_declaration.fmt_class_type_declaration_trf subj
class ['a,'self_class_info] fmt_class_info_t fself  fa =
  object
    inherit  ((['a,'self_class_info] fmt_class_info_t_stub)
      fmt_o_class_type.fmt_oclass_type_func
      fmt_o_class_type_declaration.fmt_oclass_type_declaration_func fself fa)
  end
class ['self_class_type] fmt_class_type_t fself =
  object
    inherit  ((['self_class_type] fmt_class_type_t_stub)
      fmt_o_class_info.fmt_oclass_info_func
      fmt_o_class_type_declaration.fmt_oclass_type_declaration_func fself)
  end
class ['self_class_type_declaration] fmt_class_type_declaration_t fself =
  object
    inherit  ((['self_class_type_declaration]
      fmt_class_type_declaration_t_stub)
      fmt_o_class_info.fmt_oclass_info_func
      fmt_o_class_type.fmt_oclass_type_func fself)
  end
