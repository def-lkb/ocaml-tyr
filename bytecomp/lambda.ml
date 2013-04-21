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

(* $Id: lambda.ml 12070 2012-01-23 14:49:39Z lefessan $ *)

open Misc
open Path
open Asttypes

(* ********************** *)
(** {0 Type definitions} **)
(* ********************** *)

(** {1 Basic definitions} *)

type raw_tag = int

type structured_constant =
  | Const_base        of constant
  | Const_pointer     of raw_tag
  | Const_block       of raw_tag * structured_constant list
  | Const_float_array of string list
  | Const_immstring   of string

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list

type comparison =
  | Ceq | Cneq | Clt | Cgt | Cle | Cge

type array_kind =
  | Pgenarray | Paddrarray | Pintarray | Pfloatarray

type boxed_integer =
  | Pnativeint | Pint32 | Pint64

type bigarray_kind =
  | Pbigarray_unknown
  | Pbigarray_float32   | Pbigarray_float64
  | Pbigarray_sint8     | Pbigarray_uint8
  | Pbigarray_sint16    | Pbigarray_uint16
  | Pbigarray_int32     | Pbigarray_int64
  | Pbigarray_caml_int  | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

type bigarray_layout =
  | Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

(** {1 Primitives} *)

type primitive =
  | Pidentity
  | Pignore
  | Prevapply of Location.t
  | Pdirapply of Location.t
  (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock     of int * mutable_flag
  | Pfield         of int
  | Psetfield      of int * bool
  | Pfloatfield    of int
  | Psetfloatfield of int
  | Pduprecord     of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint  | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray   of array_kind
  | Parraylength of array_kind
  | Parrayrefu   of array_kind
  | Parraysetu   of array_kind
  | Parrayrefs   of array_kind
  | Parraysets   of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint   of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint   of boxed_integer
  | Paddbint   of boxed_integer
  | Psubbint   of boxed_integer
  | Pmulbint   of boxed_integer
  | Pdivbint   of boxed_integer
  | Pmodbint   of boxed_integer
  | Pandbint   of boxed_integer
  | Porbint    of boxed_integer
  | Pxorbint   of boxed_integer
  | Plslbint   of boxed_integer
  | Plsrbint   of boxed_integer
  | Pasrbint   of boxed_integer
  | Pbintcomp  of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout

(** {1 Expressions of lambda language} *)

type lambda =
  | Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list * Location.t
  | Lfunction of function_kind * binding list * lambda
  | Llet of let_kind * Ident.t * lambda * lambda
  | Lletrec of (binding * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of Ident.t * lambda_switch * ty
  | Lstaticraise of raw_tag * lambda list
  | Lstaticcatch of lambda * raw_tag * binding list * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda
  | Ltypeabs of Ident.t list * lambda
  | Ltypeapp of lambda * ty list
  | Lascribe of lambda * ty

and binding = Ident.t * ty

and lambda_switch = {
  sw_numconsts  : int;
  sw_consts     : (raw_tag * lambda) list;
  sw_numblocks  : int;
  sw_blocks     : (raw_tag * lambda) list;
  sw_failaction : lambda option;
}

and lambda_event = {
  lev_loc  : Location.t;
  lev_kind : lambda_event_kind;
  lev_repr : int ref option;
  lev_env  : Env.summary;
}

and lambda_event_kind =
  | Lev_before
  | Lev_after of Types.type_expr
  | Lev_function


(** {1 Types of lambda language} *)

and ty = 
  | Ty_top
  | Ty_var    of Ident.t
  | Ty_link   of ty_link ref
  | Ty_mu     of Ident.t * ty
  | Ty_forall of Ident.t list * ty
  | Ty_exists of Ident.t list * ty

  | Ty_arrow  of ty * ty
  | Ty_const  of ty_const
  | Ty_array  of ty

  (* Structured values *)
  | Ty_tagged  of tag_set

  (* Exceptions *)
  | Ty_exn
  | Ty_witness of ty option

and ty_const =
  | Ty_const_int
  | Ty_const_char
  | Ty_const_string
  | Ty_const_float
  | Ty_const_int32
  | Ty_const_int64
  | Ty_const_nativeint

and ty_link =
  | Ln_unbound of Ident.t
  | Ln_bound of ty
  | Ln_forward of ty
  | Ln_marked of ty

and tag_set =
  | Tag_const of raw_tag * refinement * tag_set
  | Tag_block of raw_tag * refinement * ty list * tag_set
  | Tag_open
  | Tag_close 

and refinement = Ident.t list * binding list

(* ********************************* *)
(** {0 General purpose definitions} **)
(* ********************************* *)

(** {1 Handling of type errors} *)
type error =
  | Not_subtype  of ty * ty
  | Cant_apply   of ty * ty
  | Unbound_var  of Ident.t
  | Unbound_tvar of Ident.t
  | Tvar_capture of Ident.t
  | Fail of string

exception Error of error
let error err = raise (Error err)

(** {1 Manipulating tagsets} *)

let rec tag_const raw ?(refinement=([],[])) = function
  | Tag_const (n, b, t) when n < raw ->
    Tag_const (n, b, tag_const raw ~refinement t)
  | Tag_const (n, _, _) when n = raw ->
    failwith "Conflicting tag"
  | t -> Tag_const (raw, refinement, t)

let rec tag_block raw ?(refinement=([],[])) tys = function
  | Tag_const (n, b, t) ->
    Tag_const (n, b, tag_block raw ~refinement tys t)
  | Tag_block (n, b, p, t) when n < raw ->
    Tag_block (n, b, p, tag_block raw ~refinement tys t)
  | Tag_block (n, _, _, _) when n = raw ->
    failwith "Conflicting tag"
  | t -> Tag_block (raw, refinement, tys, t)

let rec assert_wellformed_tagset = function
  | Tag_close | Tag_open
  | Tag_const (_, _, (Tag_close | Tag_open))
  | Tag_block (_, _, _, (Tag_close | Tag_open)) -> ()
  | Tag_const (c0, _, (Tag_const (c1, _, _) as ts))
  | Tag_block (c0, _, _, (Tag_block (c1, _, _, _) as ts)) when c0 < c1 ->
    assert_wellformed_tagset ts
  | Tag_const (_, _, (Tag_block (_, _, _, _) as ts)) ->
    assert_wellformed_tagset ts
  | _ -> failwith "Malformed tagset"

let rec tagset_const_shift n = function
  | Tag_const (c, r, ts) -> Tag_const (c+n, r, tagset_const_shift n ts)
  | ts -> ts

type tagset_const_op = [`Eq|`Ne|`Lt|`Gt|`Le|`Ge]
let tagset_const_op_neg = function
  | `Eq -> `Ne | `Ne -> `Eq
  | `Lt -> `Ge | `Ge -> `Lt
  | `Gt -> `Le | `Le -> `Gt
let tagset_const_op_eval = function
  | `Eq -> (=) | `Ne -> (<>)
  | `Lt -> (<) | `Ge -> (>=)
  | `Gt -> (>) | `Le -> (<=)

let tagset_const_eval (op : tagset_const_op) rhs tag =
  let id t = t and close _ = Tag_close and rel = tagset_const_op_eval op in
  match tag with
  | Tag_close | Tag_open ->
    begin match op with
      | `Eq -> close, id
      | `Ne -> id, close
      | _   -> id, id
    end
  | Tag_block (t,r,fs,_) -> id, (fun ts -> Tag_block (t,r,fs,ts)) 
  | Tag_const (lhs,r,_) when rel lhs rhs ->
    (fun ts -> Tag_const (lhs,r,ts)), id
  | Tag_const (lhs,r,_) ->
    id, (fun ts -> Tag_const (lhs,r,ts))

let rec tagset_apply op rhs ts =
  let true', false' = match ts with
    | Tag_close | Tag_open -> ts, ts
    | Tag_const (_,_,ts) | Tag_block (_,_,_,ts) -> tagset_apply op rhs ts
  in
  let true'f, false'f = tagset_const_eval op rhs ts in
  true'f true', false'f false'

let rec tagset_match_const c = function
  | Tag_const (c',r,_) when c = c' -> Tag_const (c',r,Tag_close)
  | Tag_const (_,_,ts) | Tag_block (_,_,_,ts) -> tagset_match_const c ts
  | Tag_open | Tag_close as t -> t

let rec tagset_match_block c = function
  | Tag_block (c',r,tys,_) when c = c' -> Tag_block (c',r,tys,Tag_close)
  | Tag_const (_,_,ts) | Tag_block (_,_,_,ts) -> tagset_match_const c ts
  | Tag_open | Tag_close as t -> t

(** {1 Lambda types constructors} *)

let ty_unit = Ty_tagged (tag_const 0 Tag_close)

let ty_bool = Ty_tagged (tag_const 0 (tag_const 1 Tag_close))

let ty_bot = (* forall A. A *)
  let tA = Ident.create "A" in
  Ty_forall ([tA], Ty_var tA)

let ty_const_int = Ty_const Ty_const_int
let ty_const_float = Ty_const Ty_const_float
let ty_TODO = Ty_top

let rec ty_repr = function
    (* Path compression *)
  | Ty_link ({contents = Ln_bound
                  (Ty_link {contents = Ln_bound _} as ty)} as v) ->
    let ty' = ty_repr ty in
    v := Ln_bound (ty');
    ty'
  | Ty_link { contents = Ln_bound ty } -> ty
  | t -> t

(** {1 Lambda expressions constructors} *)

let switch_alias ?(name="alias") larg switch ty =
  match larg with
  | Lvar x -> Lswitch (x, switch, ty)
  | _ ->
    let alias = Ident.create name in
    Llet (Strict, alias, larg,
      Lswitch (alias, switch, ty))

let const_unit = Const_pointer 0

let lambda_unit = Lconst const_unit

let name_lambda arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in Llet(Strict, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
      [] -> fn (List.rev names)
    | (Lvar id as arg) :: rem ->
      name_list (arg :: names) rem
    | arg :: rem ->
      let id = Ident.create "let" in
      Llet(Strict, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args

let proj_binding ((id, ty), a) = id, a
let proj_bindings l = List.map proj_binding l

(** {1 Iterate through lambda} *)

let iter f = function
    Lvar _
  | Lconst _ -> ()
  | Lapply(fn, args, _) ->
    f fn; List.iter f args
  | Lfunction(kind, params, body) ->
    f body
  | Llet(str, id, arg, body) ->
    f arg; f body
  | Lletrec(decl, body) ->
    f body;
    List.iter (fun (bind, exp) -> f exp) decl
  | Lprim(p, args) ->
    List.iter f args
  | Lswitch(id, sw, t) ->
    f (Lvar id);
    List.iter (fun (key, case) -> f case) sw.sw_consts;
    List.iter (fun (key, case) -> f case) sw.sw_blocks;
    begin match sw.sw_failaction with
      | None -> ()
      | Some l -> f l
    end
  | Lstaticraise (_,args) ->
    List.iter f args
  | Lstaticcatch(e1, tag, vars, e2) ->
    f e1; f e2
  | Ltrywith(e1, exn, e2) ->
    f e1; f e2
  | Lifthenelse(e1, e2, e3) ->
    f e1; f e2; f e3
  | Lsequence(e1, e2) ->
    f e1; f e2
  | Lwhile(e1, e2) ->
    f e1; f e2
  | Lfor(v, e1, e2, dir, e3) ->
    f e1; f e2; f e3
  | Lassign(id, e) ->
    f e
  | Lsend (k, met, obj, args, _) ->
    List.iter f (met::obj::args)
  | Levent (lam, evt) ->
    f lam
  | Lifused (v, e) ->
    f e
  | Ltypeabs (_, l)
  | Ltypeapp (l, _)
  | Lascribe (l, _)  -> f l

(** {1 Extract free identifiers / variables} *)

module IdentSet =
  Set.Make (struct
    type t = Ident.t
    let compare = compare
  end)

let free_ids get l =
  let fv = ref IdentSet.empty in
  let rec free l =
    iter free l;
    fv := List.fold_right IdentSet.add (get l) !fv;
    match l with
    | Lfunction(kind, params, body) ->
      List.iter (fun (id,ty) -> fv := IdentSet.remove id !fv) params
    | Llet(str, id, arg, body) ->
      fv := IdentSet.remove id !fv
    | Lletrec(decl, body) ->
      List.iter (fun ((id, t), exp) -> fv := IdentSet.remove id !fv) decl
    | Lstaticcatch(e1, tag, bindings, e2) ->
      List.iter (fun (id,ty) -> fv := IdentSet.remove id !fv) bindings
    | Ltrywith(e1, exn, e2) ->
      fv := IdentSet.remove exn !fv
    | Lfor(v, e1, e2, dir, e3) ->
      fv := IdentSet.remove v !fv
    | Lassign(id, e) ->
      fv := IdentSet.add id !fv
    | Ltypeapp _ | Ltypeabs _ | Lascribe _
    | Lvar _ | Lconst _ | Lapply _
    | Lprim _ | Lswitch _ | Lstaticraise _
    | Lifthenelse _ | Lsequence _ | Lwhile _
    | Lsend _ | Levent _ | Lifused _ -> ()
  in free l; !fv

let free_variables l =
  free_ids (function Lvar id -> [id] | _ -> []) l

let free_methods l =
  free_ids (function Lsend(Self, Lvar meth, obj, _, _) -> [meth] | _ -> []) l

(** {1 For static failures} *)

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0,[])

let rec is_guarded = function
  | Lifthenelse(cond, body, Lstaticraise (0,[])) -> true
  | Llet(str, id, lam, body) -> is_guarded body
  | Levent(lam, ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0,[])) ->
    Lifthenelse (cond, body, patch)
  | Llet(str, id, lam, body) ->
    Llet (str, id, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
    Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(** {1 Translate an access path} *)

let rec transl_path = function
    Pident id ->
    if Ident.global id then Lprim(Pgetglobal id, []) else Lvar id
  | Pdot(p, s, pos) ->
    Lprim(Pfield pos, [transl_path p])
  | Papply(p1, p2) ->
    fatal_error "Lambda.transl_path"

(** {1 Compile a sequence of expressions} *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
    let lam = fn x in Lsequence(lam, make_sequence fn rem)

(** {1 To let-bind expressions to variables *)

let bind str var exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, var, exp, body)

(** {1 Working with comparison *)

let commute_comparison = function
  | Ceq -> Ceq | Cneq -> Cneq
  | Clt -> Cgt | Cle -> Cge
  | Cgt -> Clt | Cge -> Cle

let negate_comparison = function
  | Ceq -> Cneq | Cneq -> Ceq
  | Clt -> Cge  | Cle -> Cgt
  | Cgt -> Cle  | Cge -> Clt

(* ************************************************* *)
(** {0 Syntactic equality on types and expressions} **)
(* ************************************************* *)

let samepair f1 f2 (a1,a2) (b1,b2) =
  f1 a1 b1 && f2 a2 b2

let rec same l1 l2 =
  match (l1, l2) with
  | Lvar v1, Lvar v2 ->
    Ident.same v1 v2
  | Lconst c1, Lconst c2 ->
    c1 = c2
  | Lapply(a1, bl1, _), Lapply(a2, bl2, _) ->
    same a1 a2 && samelist same bl1 bl2
  | Lfunction(k1, idl1, a1), Lfunction(k2, idl2, a2) ->
    k1 = k2 && samelist (fun (i1,t1) (i2,t2) -> Ident.same i1 i2) idl1 idl2 && same a1 a2
  | Llet(k1, id1, a1, b1), Llet(k2, id2, a2, b2) ->
    k1 = k2 && Ident.same id1 id2 && same a1 a2 && same b1 b2
  | Lletrec (bl1, a1), Lletrec (bl2, a2) ->
    samelist (samepair samebinding same) bl1 bl2 && same a1 a2
  | Lprim(p1, al1), Lprim(p2, al2) ->
    p1 = p2 && samelist same al1 al2
  | Lswitch(a1, s1, t1), Lswitch(a2, s2, t2) ->
    Ident.same a1 a2 && sameswitch s1 s2 && sametype t1 t2
  | Lstaticraise(n1, al1), Lstaticraise(n2, al2) ->
    n1 = n2 && samelist same al1 al2
  | Lstaticcatch(a1, n1, b1, l1), Lstaticcatch(a2, n2, b2, l2) ->
    same a1 a2 && n1 = n2 && samelist samebinding b1 b2 && same l1 l2
  | Ltrywith(a1, id1, b1), Ltrywith(a2, id2, b2) ->
    same a1 a2 && Ident.same id1 id2 && same b1 b2
  | Lifthenelse(a1, b1, c1), Lifthenelse(a2, b2, c2) ->
    same a1 a2 && same b1 b2 && same c1 c2
  | Lsequence(a1, b1), Lsequence(a2, b2) ->
    same a1 a2 && same b1 b2
  | Lwhile(a1, b1), Lwhile(a2, b2) ->
    same a1 a2 && same b1 b2
  | Lfor(id1, a1, b1, df1, c1), Lfor(id2, a2, b2, df2, c2) ->
    Ident.same id1 id2 &&  same a1 a2 &&
    same b1 b2 && df1 = df2 && same c1 c2
  | Lassign(id1, a1), Lassign(id2, a2) ->
    Ident.same id1 id2 && same a1 a2
  | Lsend(k1, a1, b1, cl1, _), Lsend(k2, a2, b2, cl2, _) ->
    k1 = k2 && same a1 a2 && same b1 b2 && samelist same cl1 cl2
  | Levent(a1, ev1), Levent(a2, ev2) ->
    same a1 a2 && ev1.lev_loc = ev2.lev_loc
  | Lifused(id1, a1), Lifused(id2, a2) ->
    Ident.same id1 id2 && same a1 a2
  | Ltypeapp (l1,t1), Ltypeapp (l2,t2) ->
    same l1 l2 && samelist sametype t1 t2
  | Ltypeabs (ids1,l1), Ltypeabs (ids2,l2) ->
    samelist Ident.same ids1 ids2 && same l1 l2
  | Lascribe (l1,t1), Lascribe (l2,t2) ->
    same l1 l2 && sametype t1 t2
  | _, _ ->
    false

and sameswitch sw1 sw2 =
  let samecase (n1, a1) (n2, a2) = n1 = n2 && same a1 a2 in
  sw1.sw_numconsts = sw2.sw_numconsts &&
  sw1.sw_numblocks = sw2.sw_numblocks &&
  samelist samecase sw1.sw_consts sw2.sw_consts &&
  samelist samecase sw1.sw_blocks sw2.sw_blocks &&
  (match (sw1.sw_failaction, sw2.sw_failaction) with
   | (None, None) -> true
   | (Some a1, Some a2) -> same a1 a2
   | _ -> false)

and sametype t1 t2 = match t1,t2 with
  | Ty_top, Ty_top -> true
  | Ty_var v, Ty_var v' -> Ident.same v v'
  | Ty_link l, Ty_link l' -> l == l'
  | Ty_mu (id,t), Ty_mu (id',t') -> Ident.same id id' && sametype t t'
  | Ty_forall (id,t), Ty_forall (id',t') -> samelist Ident.same id id' && sametype t t'
  | Ty_exists (id,t), Ty_exists (id',t') -> samelist Ident.same id id' && sametype t t'

  | Ty_arrow (a,b), Ty_arrow (a',b') -> sametype a a' && sametype b b'
  | Ty_const c, Ty_const c' -> sametconst c c'
  | Ty_array t, Ty_array t' -> sametype t t'
  | Ty_tagged t, Ty_tagged t' -> sametagset t t'
  | Ty_exn, Ty_exn -> true
  | Ty_witness None, Ty_witness None -> true
  | Ty_witness (Some t), Ty_witness (Some t') -> sametype t t'
  | (Ty_top | Ty_var _ | Ty_mu _ | Ty_forall _ | Ty_exists _ | Ty_arrow _ | Ty_const _ | Ty_array _ | Ty_tagged _ | Ty_witness _ | Ty_exn | Ty_link _), _ -> false

and sametconst c1 c2 = match c1,c2 with
  | Ty_const_int      , Ty_const_int      
  | Ty_const_char     , Ty_const_char
  | Ty_const_string   , Ty_const_string
  | Ty_const_float    , Ty_const_float
  | Ty_const_int32    , Ty_const_int32
  | Ty_const_int64    , Ty_const_int64
  | Ty_const_nativeint, Ty_const_nativeint -> true
  | _, _ -> false

and sametagset t1 t2 = match t1, t2 with
  | Tag_open, Tag_open | Tag_close, Tag_close -> true
  | Tag_const (n1,r1,t1), Tag_const (n2,r2,t2) ->
    n1 = n2 && samerefinement r1 r2 && sametagset t1 t2
  | Tag_block (n1,r1,p1,t1), Tag_block (n2,r2,p2,t2) ->
    n1 = n2 && samerefinement r1 r2 &&
    samelist sametype p1 p2 && sametagset t1 t2
  | _, _ -> false

and samebinding b1 b2 = samepair Ident.same sametype b1 b2
and samerefinement r1 r2 =
  samepair (samelist Ident.same) (samelist samebinding) r1 r2

let same_type = sametype

(* *************************************************** *)
(** {0 Variable substitution on expression and types} **)
(* *************************************************** *)

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst = function
      Lvar id as l ->
      begin try Ident.find_same id s with Not_found -> l end
    | Lconst sc as l -> l
    | Lapply(fn, args, loc) -> Lapply(subst fn, List.map subst args, loc)
    | Lfunction(kind, params, body) -> Lfunction(kind, params, subst body)
    | Llet(str, id, arg, body) -> Llet(str, id, subst arg, subst body)
    | Lletrec(decl, body) -> Lletrec(List.map subst_decl decl, subst body)
    | Lprim(p, args) -> Lprim(p, List.map subst args)
    | Lswitch(id, sw, ty) ->
      Lswitch(id,
        {sw with sw_consts = List.map subst_case sw.sw_consts;
                 sw_blocks = List.map subst_case sw.sw_blocks;
                 sw_failaction =
                   match sw.sw_failaction with
                   | None -> None
                   | Some l -> Some (subst l)}, ty)

    | Lstaticraise (i,args) ->  Lstaticraise (i, List.map subst args)
    | Lstaticcatch(e1, tag, ref, e2) -> Lstaticcatch(subst e1, tag, ref, subst e2)
    | Ltrywith(e1, exn, e2) -> Ltrywith(subst e1, exn, subst e2)
    | Lifthenelse(e1, e2, e3) -> Lifthenelse(subst e1, subst e2, subst e3)
    | Lsequence(e1, e2) -> Lsequence(subst e1, subst e2)
    | Lwhile(e1, e2) -> Lwhile(subst e1, subst e2)
    | Lfor(v, e1, e2, dir, e3) -> Lfor(v, subst e1, subst e2, dir, subst e3)
    | Lassign(id, e) -> Lassign(id, subst e)
    | Lsend (k, met, obj, args, loc) ->
      Lsend (k, subst met, subst obj, List.map subst args, loc)
    | Levent (lam, evt) -> Levent (subst lam, evt)
    | Lifused (v, e) -> Lifused (v, subst e)
    | Ltypeabs (id, l) -> Ltypeabs (id, subst l)
    | Ltypeapp (l, t) -> Ltypeapp (subst l, t)
    | Lascribe (l, t) -> Lascribe (subst l, t)
  and subst_decl (binding, exp) = (binding, subst exp)
  and subst_case (key, case) = (key, subst case)
  in subst lam

let map_type ?marked f ty =
  let marked' = match marked with Some m -> m | None -> ref [] in
  let mark v = match !v with
    | Ln_bound t | Ln_forward t ->
      v := Ln_marked t;
      marked' := v :: !marked'
    | _ -> ()
  in
  let unmark v = match !v with
    | Ln_marked t -> v := Ln_bound t
    | _ -> assert false
  in
  let rec aux ty = match f ~marked:marked' ty with
    | Some ty' -> ty'
    | None -> match ty with
      | Ty_var _ | Ty_exn | Ty_witness None | Ty_top | Ty_const _ -> ty
      | Ty_array t          -> Ty_array (aux t)
      | Ty_witness (Some t) -> Ty_witness (Some (aux t))
      | Ty_arrow (t1,t2)    -> Ty_arrow (aux t1, aux t2)
      | Ty_mu (id,t)        -> Ty_mu (id, aux t)
      | Ty_forall (ids,t)   -> Ty_forall (ids, aux t)
      | Ty_exists (ids,t)   -> Ty_exists (ids, aux t)
      | Ty_tagged ts        -> Ty_tagged (tagset ts)
      | Ty_link v           -> unlink v; ty
  and unlink v = match !v with
    | Ln_unbound _ -> ()
    | Ln_marked _ -> ()
    | Ln_forward _ -> failwith "occur-check"
    | Ln_bound t ->
      v := Ln_forward t;
      v := Ln_bound (aux t);
      mark v
  and tagset = function
    | Tag_close | Tag_open as c -> c
    | Tag_const (tag,ref,ts) ->
      Tag_const (tag, refinement ref, tagset ts)
    | Tag_block (tag,ref,tys,ts) -> 
      Tag_block (tag, refinement ref, List.map aux tys, tagset ts)
  and refinement (ids,bindings) =
    (ids, List.map (fun (id,ty) -> id, aux ty) bindings)
  in
  let ty' = aux ty in
  begin match marked with
    | None -> List.iter unmark !marked';
    | _ -> ()
  end;
  ty'

let subst_type s ty =
  let assert_unbound id =
    try
      ignore (Ident.find_same id s);
      error (Tvar_capture id)
    with Not_found -> ()
  in
  let rec subst ~marked = function
    | Ty_var id ->
      begin try Some (Ident.find_same id s) with Not_found -> None end
    | Ty_mu (id,_) ->
      assert_unbound id; None
    | Ty_exists (ids,_) | Ty_forall (ids,_) ->
      List.iter assert_unbound ids; None
    | Ty_tagged ts -> check_tagset ts; None
    | _ -> None
  and check_tagset = function
    | Tag_close | Tag_open -> ()
    | Tag_const (_,r,ts) | Tag_block (_,r,_,ts) ->
      check_ref r;
      check_tagset ts
  and check_ref (ids,bindings) =
    List.iter assert_unbound ids;
    List.iter (fun (id,_) -> assert_unbound id) bindings
  in
  map_type subst ty

(* ************************ *)
(** {0 Subtyping relation} **)
(* ************************ *)

(** Context management *)
type context = {
  ctx_vars     : ty Ident.tbl;
  ctx_catchers : (int * ty list) list;
} 

let context_empty = { ctx_vars = Ident.empty ; ctx_catchers = [] }
let is_freevar v ctx =
  try match Ident.find_same v ctx.ctx_vars with
    | Ty_var v' -> Ident.same v v'
    | _ -> false
  with Not_found -> false

let bind_var id tyarg ctx =
  { ctx with ctx_vars = Ident.add id tyarg ctx.ctx_vars }

let bind_freevar id ctx = bind_var id (Ty_var id) ctx

let bind_vars ?(using=bind_var) vars ctx =
  List.fold_left (fun ctx (id,t) -> using id t ctx) ctx vars

let bind_catcher tag types ctx =
  { ctx with ctx_catchers = (tag,types) :: ctx.ctx_catchers }

let rec context_refine (ids,bindings) ctx =
  let ctx,tbl = List.fold_left
      (fun (ctx,tbl) id ->
        let id' = Ident.rename id in
        bind_freevar id' ctx,
        Ident.add id (Ty_var id') tbl)
      (ctx,Ident.empty) ids
  in
  let bind ctx (id,ty) =
    let ty' = subst_type tbl ty in
    bind_refine id ty' ctx
  in
  tbl, List.fold_left bind ctx bindings

and bind_refine id tyarg ctx =
  match tyarg with
  | Ty_tagged (Tag_const (c,r,Tag_close)) ->
    let _, ctx = context_refine r ctx in
    bind_var id (Ty_tagged (Tag_const (c,([],[]),Tag_close))) ctx
  | Ty_tagged (Tag_block (c,r,tys,Tag_close)) ->
    let tbl, ctx = context_refine r ctx in
    let tys = List.map (subst_type tbl) tys in
    bind_var id (Ty_tagged (Tag_block (c,([],[]),tys,Tag_close))) ctx
  | _ -> bind_var id tyarg ctx

and tagset_refine ts ctx = match ts with
  | Tag_const (_,r,Tag_close) | Tag_block (_,r,_,Tag_close) ->
    snd (context_refine r ctx)
  | _ -> ctx

(** Assumptions management, used to implement subtype relation *)
module AssumSet = Set.Make(struct type t = Ident.t * ty let compare = compare end)
type assumptions = {
  as_l  : AssumSet.t; 
  as_r  : AssumSet.t;
}

let assuming_l id t { as_l } = AssumSet.mem (id,t) as_l
let assuming_r t id { as_r } = AssumSet.mem (id,t) as_r

let assum_empty = {
  as_l  = AssumSet.empty;
  as_r  = AssumSet.empty;
}

let assum_l id t assum = { assum with as_l = AssumSet.add (id,t) assum.as_l }
let assum_r t id assum = { assum with as_r = AssumSet.add (id,t) assum.as_r }

let rec compare_type ~subtype ~assum ~ctx t1 t2 =
  let compare_type ?(assum=assum) ?(ctx=ctx) ?(subtype=subtype) t1 t2 =
    compare_type ~subtype ~assum ~ctx t1 t2
  in
  match ty_repr t1, ty_repr t2 with
  | _, Ty_top when subtype -> ()
  | Ty_top, Ty_top -> ()

  | Ty_var v1, Ty_var v2 when Ident.same v1 v2
                           && is_freevar v1 ctx ->
    ()

  | Ty_var v, t when assuming_l v t assum -> ()
  | t, Ty_var v when assuming_r t v assum -> ()
  | Ty_var id, t2 when not (is_freevar id ctx) ->
    begin match 
        try Some (Ident.find_same id ctx.ctx_vars)
        with Not_found -> None
      with
      | None -> error (Unbound_tvar id)
      | Some t1 -> compare_type ~assum:(assum_l id t2 assum) t1 t2
    end
  | t1, Ty_var id when not (is_freevar id ctx) ->
    begin match 
        try Some (Ident.find_same id ctx.ctx_vars)
        with Not_found -> None
      with
      | None -> error (Unbound_tvar id)
      | Some t2 -> compare_type ~assum:(assum_r t1 id assum) t1 t2
    end

  | t1, Ty_mu (id,t2) -> 
    compare_type ~ctx:(bind_var id t2 ctx) t1 t2
  | Ty_mu (id,t1), t2 -> 
    compare_type ~ctx:(bind_var id t1 ctx) t1 t2

  | Ty_arrow (a1,a2), Ty_arrow (b1,b2) ->
    compare_type b1 a1;
    compare_type b2 a2

  | Ty_tagged s1, Ty_tagged s2 ->
    compare_tagset ~subtype ~assum ~ctx s1 s2

  | Ty_const c1, Ty_const c2 when sametconst c1 c2 ->  ()

  | Ty_array t1, Ty_array t2 ->
    (* Array are invariant, …slow implementation *)
    compare_type ~subtype:false t1 t2;

  | Ty_forall (idl1,t1), Ty_forall (idl2,t2)
    when List.length idl1 = List.length idl2 ->
    let ctx = List.fold_left2
        (fun ctx id1 id2 ->
          let id' = Ident.rename id1 in
          bind_var id1 (Ty_var id')
            (bind_var id2 (Ty_var id')
            (bind_freevar id' ctx)))
        ctx idl1 idl2
    in
    compare_type ~ctx t1 t2

  | Ty_exists (idl1,t1), Ty_exists (idl2,t2)
    when List.length idl1 = List.length idl2 ->
    let ctx = List.fold_left2
        (fun ctx id1 id2 ->
          let id' = Ident.rename id1 in
          bind_var id1 (Ty_var id')
            (bind_var id2 (Ty_var id')
            (bind_freevar id' ctx)))
        ctx idl1 idl2
    in
    compare_type ~ctx t1 t2

  | Ty_exn, Ty_exn -> ()
  | Ty_witness None, Ty_witness None -> ()
  | Ty_witness (Some t1), Ty_witness (Some t2) ->
    compare_type t1 t2

  | Ty_link {contents=Ln_bound _}, _ | _, Ty_link {contents=Ln_bound _} ->
    assert false

  | Ty_top, _ 
  | (Ty_var _ | Ty_const _ | Ty_tagged _ | Ty_forall _ | Ty_exists _ |
     Ty_arrow _ | Ty_array _ | Ty_exn | Ty_witness _), _ ->
    error (Not_subtype (t1,t2))

and compare_tagset ~subtype ~assum ~ctx s1 s2 =
  let compare_tagset s1 s2 =
    compare_tagset ~subtype ~assum ~ctx s1 s2 in
  match s1,s2 with
  | Tag_open, Tag_open | Tag_close, Tag_close -> ()

  | Tag_const (c1, r1, s1'), Tag_const (c2, r2, s2') when c1 = c2 ->
    ignore (compare_refinement ~subtype ~assum ~ctx r1 r2);
    compare_tagset s1' s2'

  | Tag_block (c1, r1, p1, s1'), Tag_block (c2, r2, p2, s2')
    when c1 = c2 && List.length p1 = List.length p2 ->
    let ctx = compare_refinement ~subtype ~assum ~ctx r1 r2 in
    List.iter2 (compare_type ~subtype ~assum ~ctx) p1 p2;
    compare_tagset s1' s2'

  (* Subtyping *)
  | Tag_close, Tag_open when subtype -> ()
  | Tag_const (c1, _, _), Tag_const (c2, _, s2') when subtype && c2 < c1 ->
    compare_tagset s1 s2'
  | Tag_block (c1, _, _, _), Tag_block (c2, _, _, s2') when subtype && c2 < c1 ->
    compare_tagset s1 s2'
  | Tag_block (_, _, _, _), Tag_const (_, _, s2') when subtype ->
    compare_tagset s1 s2'

  | _, _ -> error (Not_subtype (Ty_tagged s1, Ty_tagged s2))

and compare_refinement ~subtype ~assum ~ctx r1 r2 = match r1,r2 with
  | (ids,p), ([],[]) when subtype ->
    snd (context_refine r1 ctx)

  | (ids1,binds1), (ids2,binds2)
    when List.length ids1 = List.length ids2 ->
    let vars  = List.map Ident.rename ids1 in
    let tvars = List.map (fun i -> Ty_var i) vars in
    let ctx = List.fold_right bind_freevar vars ctx in
    let ctx = List.fold_right2 bind_var ids1 tvars ctx in
    let ctx = List.fold_right2 bind_var ids2 tvars ctx in
    let sort = List.sort (fun (a,_) (b,_) -> compare a b) in
    let rec aux b1 b2 = match b1, b2 with
      | (id1,t1) :: b1', (id2,t2) :: b2' when id1 = id2 ->
        compare_type ~subtype ~assum ~ctx t1 t2;
        aux b1' b2'
      | [], [] -> ()

      | (id1,_) :: b1', (id2,_) :: _ when id1 < id2 && subtype -> aux b1' b2
      | _, [] when subtype -> ()
      | _, _ -> error (Fail "missing binding in refinement")
    in
    aux (sort binds1) (sort binds2);
    let tbl = Ident.empty in
    let tbl = List.fold_right2 Ident.add ids1 tvars tbl in
    let tbl = List.fold_right2 Ident.add ids2 tvars tbl in
    let bind ctx (id,ty) =
      bind_refine id (subst_type tbl ty) ctx in
    List.fold_left bind ctx binds1

  | _, _ -> error (Fail "incompatible refinement")


let (<:) t1 t2 ~ctx =
  compare_type ~subtype:true ~assum:assum_empty ~ctx t1 t2
let (=:) t1 t2 ~ctx =
  compare_type ~subtype:false ~assum:assum_empty ~ctx t1 t2

let (<:?) t1 t2 ~ctx =
  try (t1 <: t2) ~ctx; true
  with _ -> false

(* ************************************* *)
(** {0 Type-checking lambda expression} **)
(* ************************************* *)

let typeof_const = function
  | Const_int    _ -> Ty_const_int
  | Const_char   _ -> Ty_const_char
  | Const_string _ -> Ty_const_string
  | Const_float  _ -> Ty_const_float
  | Const_int32  _ -> Ty_const_int32
  | Const_int64  _ -> Ty_const_int64
  | Const_nativeint _ ->  Ty_const_nativeint

let rec typeof_sconst = function
  | Const_base        c -> Ty_const (typeof_const c)
  | Const_pointer     i -> Ty_tagged (tag_const i Tag_close)
  | Const_block       (i,scs) -> 
    Ty_tagged (tag_block i (List.map typeof_sconst scs) Tag_close)
  | Const_float_array _ -> Ty_array (Ty_const Ty_const_float)
  | Const_immstring _ -> (Ty_const Ty_const_string)


let rec typeof_prim ctx prim targs = match prim, targs with
  | Pidentity, [t] -> ty_bot
  | Pignore, [t] -> ty_unit
  | Prevapply _, [targ;Ty_arrow (targ',tres)] 
  | Pdirapply _, [Ty_arrow (targ',tres);targ] ->
    (targ <: targ') ~ctx;
    tres
  (* Globals *)
  | Pgetglobal i, [] ->
    (try Ident.find_same i ctx.ctx_vars
     with Not_found -> error (Fail "Unbound global"))
  | Psetglobal i, [targ] ->
    let ty = 
      try Ident.find_same i ctx.ctx_vars
      with Not_found -> error (Fail "Unbound global")
    in
    (targ <: ty) ~ctx;
    ty_unit
  (* Operations on heap blocks *)
  | Pmakeblock (tag,_), targs ->
    Ty_tagged (tag_block tag targs Tag_close) 
  | Pfield i, [Ty_tagged (Tag_block (_, _, fields, Tag_close))] ->
    (try List.nth fields i 
     with Failure _ -> error (Fail "invalid field primitive"))
  | Psetfield (i,_), [Ty_tagged (Tag_block (_, _, fields, Tag_close)); tval] ->
    let tfield =
      try List.nth fields i 
      with Failure _ -> error (Fail "invalid field primitive")
    in
    (tval <: tfield) ~ctx;
    ty_unit
  | (Pfield _, [_] | Psetfield _, [_;_]) ->
    error (Fail "invalid field primitive")

  | Pfloatfield i, [Ty_tagged (Tag_block (_, _, fields, Tag_close))] ->
    let t = List.nth fields i in
    (ty_const_float <: t) ~ctx;
    t
  | Psetfloatfield i, [Ty_tagged (Tag_block (_, _, fields, Tag_close)); tval] ->
    let t =
      try List.nth fields i 
      with Failure _ -> error (Fail "invalid field primitive")
    in
    (t <: ty_const_float) ~ctx;
    ty_unit
  | (Pfloatfield _, [_] | Psetfloatfield _, [_;_]) ->
    error (Fail "invalid field primitive")

  | Pduprecord (Types.Record_regular,size), 
    [Ty_tagged (Tag_block (_, _, fields, Tag_close)) as ty] 
    when List.length fields = size ->
    ty
  | Pduprecord (Types.Record_float,size), 
    [Ty_tagged (Tag_block (_, _, fields, Tag_close)) as ty] 
    when List.length fields = size
      && List.for_all ((<:?) ty_const_float ~ctx) fields ->
    ty

  (* Force lazy values, breaks some assumptions,
   * may need special type constructor *)
  | Plazyforce, [targ] ->
    begin match targ with
      | _ -> failwith "TODO"
             (*| Ty_block { ty_blocks = [tag, [Ty_arrow(targ,tres)]] }
               when tag = Obj.lazy_tag ->
               assert_subtype ctx targ ty_unit;
               tres
               | Ty_block { ty_blocks = [tag, [p0]] }
               when tag = Obj.forward_tag ->
               p0
               | Ty_block { ty_blocks = lst }
               when List.exists 
                   (fun (t,_) -> Obj.(t = lazy_tag || t = forward_tag))
                   lst ->
               error (Fail "invalid lazy representation")
               | targ -> targ*)
    end

  (* External call *)
  | Pccall prim, targs when prim.Primitive.prim_arity = List.length targs ->
    ty_bot

  (* Exceptions *)
  | Praise, [e] ->
    ty_bot

  (* Boolean operations *)
  | (Psequand | Psequor), [a;b]
    when (a <:? ty_bool) ~ctx
      && (b <:? ty_bool) ~ctx ->
    ty_bool
  | Pnot, [a] when (a <:? ty_bool) ~ctx ->
    ty_bool

  (* Integer operations *)
  | (Paddint | Psubint | Pmulint | Pdivint | Pmodint |
     Pandint | Porint  | Pxorint | Plslint | Plsrint | Pasrint),
    [a;b] when (a <:? ty_const_int) ~ctx  
            && (b <:? ty_const_int) ~ctx ->
    ty_const_int
  | Pnegint, [a] when (a <:? ty_const_int) ~ctx ->
    ty_const_int
  | Pintcomp _, [a] when (a <:? ty_const_int) ~ctx ->
    ty_bool
  | _ -> failwith "TODO: unhandled primitive"

(*| Poffsetint i -> failwith "TODO"
  | Poffsetref i -> failwith "TODO"
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray   of array_kind
  | Parraylength of array_kind
  | Parrayrefu   of array_kind
  | Parraysetu   of array_kind
  | Parrayrefs   of array_kind
  | Parraysets   of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint   of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint   of boxed_integer
  | Paddbint   of boxed_integer
  | Psubbint   of boxed_integer
  | Pmulbint   of boxed_integer
  | Pdivbint   of boxed_integer
  | Pmodbint   of boxed_integer
  | Pandbint   of boxed_integer
  | Porbint    of boxed_integer
  | Pxorbint   of boxed_integer
  | Plslbint   of boxed_integer
  | Plsrbint   of boxed_integer
  | Pasrbint   of boxed_integer
  | Pbintcomp  of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout *)

and typeof ctx = function
  | Lvar v -> 
    (try Ident.find_same v ctx.ctx_vars
     with Not_found -> error (Unbound_var v))
  | Lconst c -> typeof_sconst c
  | Lapply (lfun,args,_) ->
    let tfun = typeof ctx lfun in
    let tapp ty arg =
      let targ = typeof ctx arg in
      match ty with
      | Ty_arrow (targ',tres) ->
        (targ <: targ') ~ctx;
        tres
      | _ -> error (Fail "expecting function")
    in
    List.fold_left tapp tfun args
  | Lfunction (Curried,args,body) ->
    let tbody = typeof (bind_vars ~using:bind_refine args ctx) body in
    List.fold_right (fun (_,targ) tres -> Ty_arrow (targ,tres))
      args tbody
  | Lfunction (Tupled,args,body) ->
    let tbody = typeof (bind_vars ~using:bind_refine args ctx) body in
    let targs = List.map snd args in
    Ty_arrow (Ty_tagged (tag_block 0 targs Tag_close), tbody)
  | Llet (_,id,expr,body) ->
    let texpr = typeof ctx expr in
    typeof (bind_refine id texpr ctx) body 
  | Lletrec (bindings, body) ->
    let ctx' = bind_vars (List.map fst bindings) ctx in
    let validate ((id,ty),expr) =
      let ty' = typeof ctx' expr in
      (ty' <: ty) ~ctx
    in
    List.iter validate bindings;
    typeof ctx' body 
  | Lprim (p, args) -> 
    let targs = List.map (typeof ctx) args in
    typeof_prim ctx p targs
  | Ltrywith (expr,id,handler) ->
    let texpr = typeof ctx expr in
    let thandler = typeof (bind_var id Ty_exn ctx) handler in
    (thandler <: texpr) ~ctx;
    texpr
  | Lsequence (l1,l2) ->
    (typeof ctx l1 <: Ty_top) ~ctx;
    typeof ctx l2
  | Lwhile (lcond,lbody) ->
    (typeof ctx lcond <: ty_bool) ~ctx;
    (typeof ctx lbody <: Ty_top) ~ctx;
    ty_unit
  | Lfor (id,llo,lhi,_,lbody) ->
    (typeof ctx llo <: ty_const_int) ~ctx;
    (typeof ctx lhi <: ty_const_int) ~ctx;
    (typeof (bind_var id ty_const_int ctx) lbody <: Ty_top) ~ctx;
    ty_unit
  | Lassign (var,lval) ->
    let tvar =
      try Ident.find_same var ctx.ctx_vars
      with Not_found -> error (Unbound_var var)
    and tval = typeof ctx lval
    in
    (tval <: tvar) ~ctx;
    ty_unit
  | Ltypeabs (ids,lam) ->
    let ids' = List.map Ident.rename ids in
    let aux ctx id id' =
      bind_var id (Ty_var id') (bind_freevar id ctx)
    in
    let ctx = List.fold_left2 aux ctx ids ids' in
    let tlam = typeof ctx lam in
    Ty_forall (ids', tlam)
  | Ltypeapp (lam,tys) ->
    let tlam = typeof ctx lam in
    begin match tlam with
      | Ty_forall (ids,lt) when List.length ids = List.length tys ->
        let s = List.fold_right2 Ident.add ids tys Ident.empty in
        subst_type s lt
      | Ty_forall _ -> error (Fail "type application: incorrect arity")
      | _ -> error (Fail "type application: expecting forall") 
    end
  | Lascribe (l,t) ->
    (typeof ctx l <: t) ~ctx;
    t
  | Lsend _ | Levent _ | Lifused _ ->
    failwith "TODO"

  | Lifthenelse (lcond,lthen,lelse) ->
    let tcond = typeof ctx lcond in 
    (tcond <: ty_bool) ~ctx;
    begin match tcond with
      | Ty_tagged ts ->
        let rthen, relse = tagset_apply `Ne 0 ts in
        let tthen = typeof (tagset_refine rthen ctx) lthen 
        and telse = typeof (tagset_refine relse ctx) lelse 
        in
        (telse <: tthen) ~ctx;
        tthen
      | _ ->
        let tthen = typeof ctx lthen in
        (typeof ctx lelse <: tthen) ~ctx;
        tthen
    end

  | Lstaticraise (tag, args) ->
    let targs = List.map (typeof ctx) args in
    let texpected =
      try List.assoc tag ctx.ctx_catchers
      with Not_found ->
        error (Fail "unknown tag in staticraise")
    in
    List.iter2 ((<:) ~ctx) targs texpected;
    ty_bot

  | Lstaticcatch (expr, id, bindings, handler) ->
    let texpr = typeof (bind_catcher id (List.map snd bindings) ctx) expr in
    let thandler = typeof (bind_vars bindings ctx) handler in
    (thandler <: texpr) ~ctx;
    texpr

  | Lswitch (v, sw, ty) ->
    let tagset =
      match try Ident.find_same v ctx.ctx_vars
            with Not_found -> error (Unbound_var v)
      with Ty_tagged ts -> ts
         | _ -> error (Fail "switch on non-tagged type")
    in
    let case_const (tag,body) =
      let tagset = tagset_match_const tag tagset in
      let ctx = tagset_refine tagset ctx in
      let ctx = bind_var v (Ty_tagged tagset) ctx in
      let tbody = typeof ctx body in
      (tbody <: ty) ~ctx
    in
    let case_block (tag,body) =
      let tagset = tagset_match_block tag tagset in
      let ctx = tagset_refine tagset ctx in
      let ctx = bind_var v (Ty_tagged tagset) ctx in
      let tbody = typeof ctx body in
      (tbody <: ty) ~ctx
    in
    List.iter case_const sw.sw_consts;
    List.iter case_block sw.sw_blocks;
    ty

