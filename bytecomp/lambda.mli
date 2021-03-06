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

(* $Id: lambda.mli 12070 2012-01-23 14:49:39Z lefessan $ *)

(* The "lambda" intermediate code *)

open Asttypes

(* ********************** *)
(** {0 Type definitions} **)
(* ********************** *)

(** {1 Basic definitions} *)

type raw_tag = int

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable
(** Meaning of kinds for let x = e in e':
    Strict: e may have side-effets; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e' *)

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list     (* stack size -> code label *)

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
  | Pnegfloat   | Pabsfloat
  | Paddfloat   | Psubfloat   | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
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
  sw_numconsts  : int;                  (* Number of integer cases *)
  sw_consts     : (int * lambda) list;  (* Integer cases *)
  sw_numblocks  : int;                  (* Number of tag block cases *)
  sw_blocks     : (int * lambda) list;  (* Tag block cases *)
  sw_failaction : lambda option;        (* Action to take if failure *)
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

  | Ty_arrow  of ty * ty
  | Ty_const  of ty_const
  (*| Ty_array  of ty*)

  (* Structured values *)
  | Ty_tagged  of tag_set

  (* Exceptions *)
  (*| Ty_exn
    | Ty_witness of ty option*)

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
  | Ln_bound of Ident.t * ty

and tag_set =
  | Tag_const of raw_tag * constraints * tag_set
  | Tag_block of raw_tag * constraints * ty list * tag_set
  | Tag_open
  | Tag_close 

and constraints = Ident.t list * binding list


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

(** {1 Lambda types constructors} *)

val ty_unit : ty
val ty_bool : ty
val ty_bot  : ty
val ty_const_int : ty
val ty_const_float : ty
val ty_TODO : ty

val ln_repr : ty_link ref -> ty_link
val ty_repr : ty -> ty

(** {1 Lambda expressions constructors} *)

val switch_alias: ?name:string -> lambda -> lambda_switch -> 
  ty -> lambda

val const_unit       : structured_constant
val lambda_unit      : lambda
val name_lambda      : lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list : lambda list -> (lambda list -> lambda) -> lambda
val proj_binding     : binding * 'a -> Ident.t * 'a
val proj_bindings    : (binding * 'a) list -> (Ident.t * 'a) list

(*val lt_unit        : ty
val lt_bool        : ty
val lt_bot         : ty
val lt_const_int   : ty
val lt_const_float : ty
val lt_TODO        : ty*)

(** {1 Iterate through lambda} *)
val iter : (lambda -> unit) -> lambda -> unit

(** {1 Extract free identifiers / variables} *)
module IdentSet    : Set.S with type elt = Ident.t
val free_variables : lambda -> IdentSet.t
val free_methods   : lambda -> IdentSet.t

(** {1 Translate an access path} *)
val transl_path : Path.t -> lambda

(** {1 Compile a sequence of expressions} *)
val make_sequence : ('a -> lambda) -> 'a list -> lambda

(** {1 To let-bind expressions to variables *)
val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda

(** {1 Working with comparison *)
val commute_comparison : comparison -> comparison
val negate_comparison  : comparison -> comparison

(** {1 For static failures} *)

(* Get a new static failure ident *)
val next_raise_count : unit -> int
val staticfail       : lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded    : lambda -> bool
val patch_guarded : lambda -> lambda -> lambda

(* ************************************************* *)
(** {0 Syntactic equality on types and expressions} **)
(* ************************************************* *)

val same      : lambda -> lambda -> bool
val same_type : ty -> ty -> bool

(* *************************************************** *)
(** {0 Variable substitution on expression and types} **)
(* *************************************************** *)

val subst_lambda : lambda Ident.tbl -> lambda -> lambda
val subst_type   : ty Ident.tbl -> ty -> ty

(* ************************ *)
(** {0 Subtyping relation} **)
(* ************************ *)

(*type context
val context_empty : context 
val bind_var  : Ident.t -> ty -> context -> context
val bind_freevar : Ident.t -> context -> context
val bind_vars : ?using:(Ident.t -> ty -> context -> context) ->
    (Ident.t * ty) list -> context -> context

val (<:) : ty -> ty -> ctx:context -> unit
val (<:?) : ty -> ty -> ctx:context -> bool*)

(* ************************************* *)
(** {0 Type-checking lambda expression} **)
(* ************************************* *)

(*val typeof_const  : constant -> ty_const
val typeof_sconst : structured_constant -> ty

val typeof : context -> lambda -> ty*)
