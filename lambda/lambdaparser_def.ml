open Asttypes
open Lambda

type param =
  | PString of string
  | PInt of int
  | PIdent of Ident.t

exception Invalid_primitive of string * param list option

type t =
  | List of Location.t * t list
  | Ident of Ident.t
  | Symbol of string
  | PSymbol of string * param list
  | Const of structured_constant
  | Colon

let rec print ppf = 
  let open Format in 
  function
  | List (_,[]) -> fprintf ppf "()"
  | List (_,[t]) -> fprintf ppf "(%a)" print t
  | List (_,t::ts) -> 
      fprintf ppf "(%a%a)" print t (fun ppf -> List.iter (fprintf ppf "@ %a" print)) ts
  | Ident i -> Ident.print ppf i
  | Symbol s -> fprintf ppf "%s" s
  | PSymbol (s,_) -> fprintf ppf "%s[]" s
  | Const _ -> fprintf ppf "<const>" 
  | Colon -> fprintf ppf ":"

type prim =
  | Prim of primitive
  | Param of (param list -> primitive)

let prim_table = lazy
  begin
    let h = Hashtbl.create 157 in
    let prim_ident f = Param (function
      | [PIdent i] -> f i
      | _ -> raise Not_found)
    in
    let prim_int f = Param (function
      | [PInt i] -> f i
      | _ -> raise Not_found)
    in
    let array_kind = function
      | "gen"   -> Pgenarray  
      | "int"   -> Pintarray  
      | "addr"  -> Paddrarray 
      | "float" -> Pfloatarray
      | _ -> raise Not_found
    in
    let prim_array p = Param (function
      | [PString s] -> p (array_kind s)
      | _ -> raise Not_found)
    in
    let register k v = Hashtbl.add h k v in
    List.iter (fun (k,v) -> register k v) [
      "id",                Prim (Pidentity);
      "ignore",            Prim (Pignore);
      "revapply",          Prim (Prevapply Location.none);
      "dirapply",          Prim (Pdirapply Location.none);
      "force",             Prim (Plazyforce);
      "raise",             Prim (Praise);
      "&&",                Prim (Psequand);
      "||",                Prim (Psequor);
      "not",               Prim (Pnot);
      "~",                 Prim (Pnegint);
      "+",                 Prim (Paddint);
      "-",                 Prim (Psubint);
      "*",                 Prim (Pmulint);
      "/",                 Prim (Pdivint);
      "mod",               Prim (Pmodint);
      "and",               Prim (Pandint);
      "or",                Prim (Porint);
      "xor",               Prim (Pxorint);
      "lsl",               Prim (Plslint);
      "lsr",               Prim (Plsrint);
      "asr",               Prim (Pasrint);
      "==",                Prim (Pintcomp(Ceq));
      "!=",                Prim (Pintcomp(Cneq));
      "<",                 Prim (Pintcomp(Clt));
      "<=",                Prim (Pintcomp(Cle));
      ">",                 Prim (Pintcomp(Cgt));
      ">=",                Prim (Pintcomp(Cge));
      "~.",                Prim (Pnegfloat);
      "abs.",              Prim (Pabsfloat);
      "+.",                Prim (Paddfloat);
      "-.",                Prim (Psubfloat);
      "*.",                Prim (Pmulfloat);
      "/.",                Prim (Pdivfloat);
      "==.",               Prim (Pfloatcomp(Ceq));
      "!=.",               Prim (Pfloatcomp(Cneq));
      "<.",                Prim (Pfloatcomp(Clt));
      "<=.",               Prim (Pfloatcomp(Cle));
      ">.",                Prim (Pfloatcomp(Cgt));
      ">=.",               Prim (Pfloatcomp(Cge));
      "string.length",     Prim (Pstringlength);
      "string.unsafe_get", Prim (Pstringrefu);
      "string.unsafe_set", Prim (Pstringsetu);
      "string.get",        Prim (Pstringrefs);
      "string.set",        Prim (Pstringsets);
      "isint",             Prim (Pisint);
      "isout",             Prim (Pisout);
      "testbit",           Prim (Pbittest);
      "int_of_float",      Prim (Pintoffloat);
	  	"float_of_int",      Prim (Pfloatofint);
	  	
	  	"global",            prim_ident (fun id -> Pgetglobal id);
	  	"setglobal",         prim_ident (fun id -> Psetglobal id);

	  	"makeblock",         prim_int (fun n -> Pmakeblock(n, Immutable));
	  	"makemutable",       prim_int (fun n -> Pmakeblock(n, Mutable));
	  	"field",             prim_int (fun n -> Pfield n);
	  	"setfield_ptr",      prim_int (fun n -> Psetfield(n, true));
	  	"setfield_imm",      prim_int (fun n -> Psetfield(n, false));
	  	"floatfield",        prim_int (fun n -> Pfloatfield n);
	  	"setfloatfield",     prim_int (fun n -> Psetfloatfield n);

      "array.length",      prim_array (fun k -> Parraylength k);
      "makearray",         prim_array (fun k -> Pmakearray k);
      "array.unsafe_get",  prim_array (fun k -> Parrayrefu k);
      "array.unsafe_set",  prim_array (fun k -> Parraysetu k);
      "array.get",         prim_array (fun k -> Parrayrefs k);
      "array.set",         prim_array (fun k -> Parraysets k);

      "duprecord", Param (function
                     | [PString "regular"; PInt n] -> 
                         Pduprecord(Types.Record_regular,n)
                     | [PString "float"; PInt n] -> 
                         Pduprecord(Types.Record_float,n)
                     | _ -> raise Not_found);

      "ccall", Param (
                 let flags_in lst = 
                   List.mem (PString "noalloc") lst,
                   List.mem (PString "float") lst
                 in
                 function
                 | PString prim_name :: PInt prim_arity :: flags ->
                     let prim_alloc, prim_native_float = flags_in flags in
                     Pccall { Primitive. prim_name ; prim_native_name = "";
                             prim_alloc ; prim_native_float ; prim_arity }
                 | PString prim_name :: PString prim_native_name :: PInt prim_arity :: flags ->
                     let prim_alloc, prim_native_float = flags_in flags in
                     Pccall { Primitive. prim_name ; prim_native_name;
                             prim_alloc ; prim_native_float ; prim_arity }
                 | _ -> raise Not_found)

	  ];
    let boxed_ints = [
      (Pnativeint, "nativeint", "Nativeint");
      (Pint32, "int32", "Int32");
      (Pint64, "int64", "Int64");
    ] in
    let boxed_ops = [ 
      ((fun bi -> Pbintofint bi), "of_int");
      ((fun bi -> Pintofbint bi), "to_int");
      ((fun bi -> Pnegbint bi), "neg");
      ((fun bi -> Paddbint bi), "add");
      ((fun bi -> Psubbint bi), "sub");
      ((fun bi -> Pmulbint bi), "mul");
      ((fun bi -> Pdivbint bi), "div");
      ((fun bi -> Pmodbint bi), "mod");
      ((fun bi -> Pandbint bi), "and");
      ((fun bi -> Porbint bi), "or");
      ((fun bi -> Pxorbint bi), "xor");
      ((fun bi -> Plslbint bi), "lsl");
      ((fun bi -> Plsrbint bi), "lsr");
      ((fun bi -> Pasrbint bi), "asr");
      ((fun bi -> Pbintcomp(bi,Ceq)), "==");
      ((fun bi -> Pbintcomp(bi,Cneq)), "!=");
      ((fun bi -> Pbintcomp(bi,Clt)), "<");
      ((fun bi -> Pbintcomp(bi,Cgt)), ">" );
      ((fun bi -> Pbintcomp(bi,Cle)), "<=");
      ((fun bi -> Pbintcomp(bi,Cge)), ">=");
    ] in 
    let register_op (bi,name,mark) (prim,op) =
      register (Printf.sprintf "%s.%s" mark op)
               (Prim (prim bi))
    and register_cvt (bi,name,mark) (bi',name',mark') =
      if bi != bi' then
        register (Printf.sprintf "%s_of_%s" name' name)
                 (Prim (Pcvtbint (bi,bi')))
    in
    List.iter
      (fun bi -> List.iter (register_op bi) boxed_ops;
                 List.iter (register_cvt bi) boxed_ints)
      boxed_ints;
    let bigarray_fun p = 
      Param (function
        | [PInt n; PString kind; PString layout] ->
          let k = match kind with
            | "generic"     -> Pbigarray_unknown   
            | "float32"     -> Pbigarray_float32   
            | "float64"     -> Pbigarray_float64   
            | "sint8"       -> Pbigarray_sint8     
            | "uint8"       -> Pbigarray_uint8     
            | "sint16"      -> Pbigarray_sint16    
            | "uint16"      -> Pbigarray_uint16    
            | "int32"       -> Pbigarray_int32     
            | "int64"       -> Pbigarray_int64     
            | "camlint"     -> Pbigarray_caml_int  
            | "nativeint"   -> Pbigarray_native_int
            | "complex32"   -> Pbigarray_complex32 
            | "complex64"   -> Pbigarray_complex64 
            | _ -> raise Not_found
          and l = match layout with
            | "unknown" -> Pbigarray_unknown_layout
            | "C"       -> Pbigarray_c_layout      
            | "Fortran" -> Pbigarray_fortran_layout
            | _ -> raise Not_found
          in
          p n k l
      | _ -> raise Not_found
    ) in
    register "Bigarray.unsafe_get"
      (bigarray_fun (fun n k l -> Pbigarrayref(true,n,k,l)));
    register "Bigarray.unsafe_set"
      (bigarray_fun (fun n k l -> Pbigarrayset(true,n,k,l)));
    register "Bigarray.get"
      (bigarray_fun (fun n k l -> Pbigarrayref(false,n,k,l)));
    register "Bigarray.set"
      (bigarray_fun (fun n k l -> Pbigarrayset(false,n,k,l)));
	  h
  end

let get_prim ?params name = 
  try match Hashtbl.find (Lazy.force prim_table) name, params with
    | Prim p, None -> p
    | Param f, Some p -> f p
    | _ -> raise Not_found
  with Not_found -> raise (Invalid_primitive (name,params))

