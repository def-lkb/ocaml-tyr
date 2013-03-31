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

type tag = int

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
  | Lswitch of Ident.t * lambda_switch * lambda_type
  | Lstaticraise of tag * lambda list
  | Lstaticcatch of lambda * (tag * binding list) * lambda
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
  | Ltypeapp of lambda * lambda_type list
  | Lascribe of lambda * lambda_type

and binding = Ident.t * lambda_type

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

and lambda_type = 
  | Lt_top
  | Lt_arrow  of lambda_type * lambda_type
  | Lt_const  of lambda_type_const
  | Lt_array  of lambda_type
  | Lt_block  of lambda_block
  | Lt_var    of Ident.t
  | Lt_mu     of Ident.t * lambda_type
  | Lt_forall of Ident.t list * lambda_type

and lambda_type_const =
  | Lt_const_int
  | Lt_const_char
  | Lt_const_string
  | Lt_const_float
  | Lt_const_int32
  | Lt_const_int64
  | Lt_const_nativeint

and lambda_block = {
  lt_blocks : (tag * lambda_type list) list;
  lt_consts : tag list;
}

(* ********************************* *)
(** {0 General purpose definitions} **)
(* ********************************* *)


(** {1 Handling of type errors} *)
type error =
  | Not_subtype  of lambda_type * lambda_type
  | Cant_apply   of lambda_type * lambda_type
  | Unbound_var  of Ident.t
  | Unbound_tvar of Ident.t
  | Tvar_capture of Ident.t
  | Fail of string

exception Error of error

(** {1 Lambda types constructors} *)

(** {1 Lambda expressions constructors} *)

val switch_alias: ?name:string -> lambda -> lambda_switch -> 
  lambda_type -> lambda

val const_unit       : structured_constant
val lambda_unit      : lambda
val name_lambda      : lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list : lambda list -> (lambda list -> lambda) -> lambda

val lt_pointer     : int -> lambda_type
val lt_unit        : lambda_type
val lt_bool        : lambda_type
val lt_bot         : lambda_type
val lt_const_int   : lambda_type
val lt_const_float : lambda_type

(** The "top" block, a block we don't know anything about.
    Current definition may make it looks like bottom… *)
val lt_top_block : lambda_type
val is_top_block : lambda_block -> bool

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
val same_type : lambda_type -> lambda_type -> bool

(* *************************************************** *)
(** {0 Variable substitution on expression and types} **)
(* *************************************************** *)

val subst_lambda : lambda Ident.tbl -> lambda -> lambda
val subst_type   : lambda_type Ident.tbl -> lambda_type -> lambda_type

(* ************************ *)
(** {0 Subtyping relation} **)
(* ************************ *)

type context
val context_empty : context 
val bind_var  : Ident.t -> lambda_type -> context -> context
val bind_vars : (Ident.t * lambda_type) list -> context -> context

val assert_subtype : context -> lambda_type -> lambda_type -> unit
val subtype : context -> lambda_type -> lambda_type -> bool

(* ************************************* *)
(** {0 Type-checking lambda expression} **)
(* ************************************* *)

val typeof_const  : constant -> lambda_type_const
val typeof_sconst : structured_constant -> lambda_type

val typeof : context -> lambda -> lambda_type
