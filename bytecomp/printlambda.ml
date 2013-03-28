(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printlambda.ml 12179 2012-02-21 17:41:02Z xleroy $ *)

open Format
open Asttypes
open Primitive
open Types
open Lambda

let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32     -> "int32"
  | Pint64     -> "int64"

let boxed_integer_mark = function
  | Pnativeint -> "Nativeint"
  | Pint32     -> "Int32"
  | Pint64     -> "Int64"

let bigarray_kind = function
  | Pbigarray_unknown    -> "generic"
  | Pbigarray_float32    -> "float32"
  | Pbigarray_float64    -> "float64"
  | Pbigarray_sint8      -> "sint8"
  | Pbigarray_uint8      -> "uint8"
  | Pbigarray_sint16     -> "sint16"
  | Pbigarray_uint16     -> "uint16"
  | Pbigarray_int32      -> "int32"
  | Pbigarray_int64      -> "int64"
  | Pbigarray_caml_int   -> "camlint"
  | Pbigarray_native_int -> "nativeint"
  | Pbigarray_complex32  -> "complex32"
  | Pbigarray_complex64  -> "complex64"

let bigarray_layout = function
  | Pbigarray_unknown_layout -> "unknown"
  | Pbigarray_c_layout       -> "C"
  | Pbigarray_fortran_layout -> "Fortran"

let record_rep = function
  | Record_regular -> "regular"
  | Record_float   -> "float"

let array_kind = function
  | Pgenarray   -> "gen"
  | Pintarray   -> "int"
  | Paddrarray  -> "addr"
  | Pfloatarray -> "float"

let base_const ppf = function
  | Const_int n       -> fprintf ppf "%i" n
  | Const_char c      -> fprintf ppf "%C" c
  | Const_string s    -> fprintf ppf "%S" s
  | Const_float f     -> fprintf ppf "%s" f
  | Const_int32 n     -> fprintf ppf "%lil" n
  | Const_int64 n     -> fprintf ppf "%LiL" n
  | Const_nativeint n -> fprintf ppf "%nin" n

let rec struct_const ppf = function
  | Const_base c         -> base_const ppf c
  | Const_immstring s    -> fprintf ppf "#%S" s
  | Const_pointer n      -> fprintf ppf "%ia" n
  | Const_block(tag, []) -> fprintf ppf "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl in
      fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let print_boxed_integer name ppf bi =
  fprintf ppf "%s.%s" (boxed_integer_mark bi) name;;

let print_bigarray ppf name unsafe n kind layout =
  fprintf ppf "Bigarray.%s[%d,%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    n (bigarray_kind kind) (bigarray_layout layout)

let primitive ppf = function
  | Pidentity -> fprintf ppf "id"
  | Pignore -> fprintf ppf "ignore"
  | Prevapply _ -> fprintf ppf "revapply"
  | Pdirapply _ -> fprintf ppf "dirapply"
  | Pgetglobal id -> fprintf ppf "global[%a]" Ident.print id
  | Psetglobal id -> fprintf ppf "setglobal[%a]" Ident.print id
  | Pmakeblock(tag, Immutable) -> fprintf ppf "makeblock[%i]" tag
  | Pmakeblock(tag, Mutable) -> fprintf ppf "makemutable[%i]" tag
  | Pfield n -> fprintf ppf "field[%i]" n
  | Psetfield(n, ptr) ->
      let instr = if ptr then "setfield_ptr" else "setfield_imm" in
      fprintf ppf "%s[%i]" instr n
  | Pfloatfield n -> fprintf ppf "floatfield[%i]" n
  | Psetfloatfield n -> fprintf ppf "setfloatfield[%i]" n
  | Pduprecord (rep, size) -> fprintf ppf "duprecord[%s,%i]" (record_rep rep) size
  | Plazyforce -> fprintf ppf "force"
  | Pccall p -> 
      let flags = [
        (true, p.prim_name);
        (p.prim_native_name <> "", p.prim_native_name);
        (true, string_of_int p.prim_arity);
        (not p.prim_alloc, "noalloc");
        (p.prim_native_float, "float");
      ] in
      let flags = List.filter fst flags in
      let flags = List.map snd flags in
      fprintf ppf "ccall[%s]" (String.concat "," flags)
  | Praise -> fprintf ppf "raise"
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint -> fprintf ppf "/"
  | Pmodint -> fprintf ppf "mod"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(Ceq) -> fprintf ppf "=="
  | Pintcomp(Cneq) -> fprintf ppf "!="
  | Pintcomp(Clt) -> fprintf ppf "<"
  | Pintcomp(Cle) -> fprintf ppf "<="
  | Pintcomp(Cgt) -> fprintf ppf ">"
  | Pintcomp(Cge) -> fprintf ppf ">="
  | Poffsetint n -> fprintf ppf "+[%i]" n
  | Poffsetref n -> fprintf ppf "+:=[%i]" n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pfloatcomp(Ceq) -> fprintf ppf "==."
  | Pfloatcomp(Cneq) -> fprintf ppf "!=."
  | Pfloatcomp(Clt) -> fprintf ppf "<."
  | Pfloatcomp(Cle) -> fprintf ppf "<=."
  | Pfloatcomp(Cgt) -> fprintf ppf ">."
  | Pfloatcomp(Cge) -> fprintf ppf ">=."
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringsetu -> fprintf ppf "string.unsafe_set"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pstringsets -> fprintf ppf "string.set"
  | Parraylength k -> fprintf ppf "array.length[%s]" (array_kind k)
  | Pmakearray k -> fprintf ppf "makearray[%s]" (array_kind k)
  | Parrayrefu k -> fprintf ppf "array.unsafe_get[%s]" (array_kind k)
  | Parraysetu k -> fprintf ppf "array.unsafe_set[%s]" (array_kind k)
  | Parrayrefs k -> fprintf ppf "array.get[%s]" (array_kind k)
  | Parraysets k -> fprintf ppf "array.set[%s]" (array_kind k)
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbittest -> fprintf ppf "testbit"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint bi -> print_boxed_integer "div" ppf bi
  | Pmodbint bi -> print_boxed_integer "mod" ppf bi
  | Pandbint bi -> print_boxed_integer "and" ppf bi
  | Porbint bi -> print_boxed_integer "or" ppf bi
  | Pxorbint bi -> print_boxed_integer "xor" ppf bi
  | Plslbint bi -> print_boxed_integer "lsl" ppf bi
  | Plsrbint bi -> print_boxed_integer "lsr" ppf bi
  | Pasrbint bi -> print_boxed_integer "asr" ppf bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | Pbigarrayref(unsafe, n, kind, layout) ->
      print_bigarray ppf "get" unsafe n kind layout
  | Pbigarrayset(unsafe, n, kind, layout) ->
      print_bigarray ppf "set" unsafe n kind layout

let print_list ?first ?(delim="") ?last printer ppf = function
  | [] -> ()
  | hd :: tl ->
      (match first with Some s -> pp_print_string ppf s | None -> ());
      printer ppf hd;
      List.iter (fprintf ppf "%s@%a" delim printer) tl;
      (match last with Some s -> pp_print_string ppf s | None -> ())

let rec lam_ty ppf = function
  | Lt_top -> fprintf ppf "top"
  | Lt_bot -> fprintf ppf "bot"
  | Lt_arrow (t1,t2) -> fprintf ppf "(%a -> %a)" lam_ty t1 lam_ty t2
  | Lt_var id -> Ident.print ppf id
  | Lt_mu (id, t) -> fprintf ppf "(mu %a %a)" Ident.print id lam_ty t
  | Lt_value { Lambda. const = `Some [] ; blocks = [] }  -> 
      fprintf ppf "(val)"
  | Lt_value { Lambda. const ; blocks }  -> 
      let print_const ppf = function
        | `Any -> pp_print_string ppf " int"
        | `Some l -> fprintf ppf " int[%s]"
            (String.concat "," (List.map string_of_int l))
      in
      let print_block ppf (i,tys) =
        fprintf ppf "(tag[%d]%a)" i (print_list ~first:" " ~delim:" " lam_ty) tys
      in
      fprintf ppf "(val%a%a)"
        print_const const 
        (print_list print_block) blocks
;; 
let rec lam ppf = function
  | Lvar id ->
      Ident.print ppf id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply(lfun, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Lfunction(kind, params, body) ->
      let pr_params ppf params =
        match kind with
        | Curried ->
            List.iter (fun (id,ty) -> fprintf ppf "@ (%a : %a)" Ident.print id lam_ty ty) params
        | Tupled ->
            fprintf ppf " (";
            let first = ref true in
            List.iter
              (fun (id,ty) ->
                if !first then first := false else fprintf ppf ",@ ";
                fprintf ppf "(%a : %a)" Ident.print id lam_ty ty)
              params;
            fprintf ppf ")" in
      fprintf ppf "@[<2>(function%a@ %a)@]" pr_params params lam body

  | Llet(k, id, arg, body) ->
      let rec letbody = function
        | Llet(k', id, arg, body) when k = k' ->
            fprintf ppf "@ @[<2>%a@ %a@]" Ident.print id lam arg;
            letbody body
        | expr -> expr in
      let kind = match k with
        | Strict    -> ""
        | Alias     -> "[alias]"
        | StrictOpt -> "[opt]"
        | Variable  -> "[var]"
      in
      fprintf ppf "@[<2>(let%s@ @[<hv 1>(@[<2>%a@ %a@]" kind Ident.print id lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case[int,%i]:@ %a@]" n lam l)
         sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case[tag,%i]:@ %a@]" n lam l)
          sw.sw_blocks ;
        begin match sw.sw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in

      fprintf ppf
       "@[<1>(%s[%d,%d] %a@ @[<v 0>%a@])@]"
       (match sw.sw_failaction with None -> "switch*" | _ -> "switch")
       sw.sw_numconsts sw.sw_numblocks
       lam larg switch sw
  | Lstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit[%d]%a)@]" i lams ls;
  | Lstaticcatch(lbody, (i, vars), lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with[%d]%a@ %a)@]"
        lam lbody i
        (fun ppf -> List.iter (fprintf ppf " %a" Ident.print)) vars
        lam lhandler
  | Ltrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Lifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Self then "self" else if k = Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Levent(expr, ev) ->
      let kind =
       match ev.lev_kind with
       | Lev_before -> "before"
       | Lev_after _  -> "after"
       | Lev_function -> "funct-body" in
      fprintf ppf "@[<2>(%s %s(%i)%s:%i-%i@ %a)@]" kind
              ev.lev_loc.Location.loc_start.Lexing.pos_fname
              ev.lev_loc.Location.loc_start.Lexing.pos_lnum
              (if ev.lev_loc.Location.loc_ghost then "<ghost>" else "")
              ev.lev_loc.Location.loc_start.Lexing.pos_cnum
              ev.lev_loc.Location.loc_end.Lexing.pos_cnum
              lam expr
  | Lifused(id, expr) ->
      fprintf ppf "@[<2>(ifused@ %a@ %a)@]" Ident.print id lam expr

and sequence ppf = function
  | Lsequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

let structured_constant = struct_const

let lambda = lam
