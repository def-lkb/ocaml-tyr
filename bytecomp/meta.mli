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

(* $Id: meta.mli 11156 2011-07-27 14:17:02Z doligez $ *)

(* To control the runtime system and bytecode interpreter *)

external global_data : unit -> Obj.t array = "caml_get_global_data"
external realloc_global_data : int -> unit = "caml_realloc_global"
external static_alloc : int -> string = "caml_static_alloc"
external static_free : string -> unit = "caml_static_free"
external static_release_bytecode : string -> int -> unit
                                 = "caml_static_release_bytecode"
external static_resize : string -> int -> string = "caml_static_resize"
type closure = unit -> Obj.t
external reify_bytecode : string -> int -> closure = "caml_reify_bytecode"
external invoke_traced_function : Obj.t -> Obj.t -> Obj.t -> Obj.t
                                = "caml_invoke_traced_function"
external get_section_table : unit -> (string * Obj.t) list
                           = "caml_get_section_table"
