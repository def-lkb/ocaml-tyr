open Lambda
open Lambdaparser_def

let type_const_of_string = function
  | "int"       -> Ty_const_int      
  | "char"      -> Ty_const_char     
  | "string"    -> Ty_const_string   
  | "float"     -> Ty_const_float    
  | "int32"     -> Ty_const_int32    
  | "int64"     -> Ty_const_int64    
  | "nativeint" -> Ty_const_nativeint
  | _ -> failwith "Unknown type constant"

let rec type_of_tree = function
  | List (_,[Symbol "top"]) ->
    Ty_top
  | List (_,[ta; Symbol "->"; tb]) ->
    Ty_arrow (type_of_tree ta, type_of_tree tb)
  | List (_,[Symbol "mu"; Ident v; t]) ->
    Ty_mu (v, type_of_tree t)
  | List (_,[Symbol "forall"; List (_,idents); t]) ->
    let extract_param = function
      | Ident i -> i
      | _ -> failwith "Invalid 'forall'"
    in
    let idents = List.map extract_param idents in
    Ty_forall (idents, type_of_tree t)
  | List (_,Symbol "tagged" :: tagset) ->
    Ty_tagged (tagset_of_list tagset)
  | Ident i ->
    Ty_var i
  | Symbol s ->
    Ty_const (type_const_of_string s)
  | _ -> failwith "Unrecognized type"

and tagset_of_list = function
  | [Symbol "close"] -> Tag_close
  | [Symbol "open"]  -> Tag_open
  | List (_,PSymbol ("const",[PInt tag]) :: args) :: tail ->
    let constraints, args = constraints_of_list args in
    if args <> [] then failwith "Invalid const definition";
    Tag_const (tag, constraints, tagset_of_list tail)
  | List (_,PSymbol ("block",[PInt tag]) :: args) :: tail ->
    let constraints, args = constraints_of_list args in
    Tag_block (tag, constraints, 
               List.map type_of_tree args, tagset_of_list tail)
  | _ -> failwith "invalid tagset"

and constraints_of_list = function
  | Symbol "exists" :: List (_,idents) :: tail ->
    let bindings, tail = bindings_of_list [] tail in
    (idents_of_list idents, bindings), tail
  | tail -> 
    let bindings, tail = bindings_of_list [] tail in
    ([], bindings), tail

and bindings_of_list acc = function
  | List (_,[Ident i ; Symbol "="; ty]) :: tail ->
    bindings_of_list ((i, type_of_tree ty) :: acc) tail
  | tail ->
    List.rev acc, tail

and idents_of_list = function
  | Ident i :: tail -> i :: idents_of_list tail
  | [] -> []
  | _ -> failwith "invalid idents list"

let function_split arguments = 
  let extract_param = function
    | List (_,[Ident id ; Colon ; ty]) -> (id, type_of_tree ty)
    | _ -> failwith "Invalid parameter"
  in
  match List.rev arguments with
  | [body ; List (_,(List (_, [Ident _ ; Colon ; _]) :: _ as params))] ->
    (Tupled, List.map extract_param params, body)
  | (body :: (List (_, [Ident _ ; Colon ; _]) :: _ as params)) ->
    (Curried, List.rev_map extract_param params, body)
  | _ -> failwith "Invalid 'function' definition"

let catch_handler tail =
  let extract_param = function
    | List (_,[Ident id; Colon; ty]) -> id, type_of_tree ty
    | _ -> failwith "Invalid parameter"
  in
  match List.rev tail with
  | body :: vars -> (List.rev_map extract_param vars, body)
  | _ -> failwith "Invalid 'staticcatch'"

let let_kind = function
  | Symbol "let" -> Lambda.Strict
  | PSymbol ("let",[PString "alias"]) -> Lambda.Alias
  | PSymbol ("let",[PString "opt"]) -> Lambda.StrictOpt
  | PSymbol ("let",[PString "var"]) -> Lambda.Variable
  | _ -> failwith "Unknown 'let' kind"

let rec lambda_of_tree = function
  | Ident i -> Lambda.Lvar i
  | Const c -> Lambda.Lconst c
  | List (_, Symbol "apply" :: lfun :: largs) ->
    let lfun = lambda_of_tree lfun in
    let largs = List.map lambda_of_tree largs in
    Lapply (lfun, largs, Location.none)
  | List (_, Symbol "function" :: def) ->
    let kind, params, body = function_split def in
    Lfunction (kind, params, lambda_of_tree body)
  | List (_,[(Symbol "let" | PSymbol ("let",_) as k) ; List (_,vars) ; body]) ->
    let k = let_kind k in
    let rec aux = function
      | (Ident i :: expr :: rest) ->
        Llet (k, i, lambda_of_tree expr, aux rest)
      | [] -> lambda_of_tree body
      | _ -> failwith "Invalid 'let' binding"
    in
    aux vars
  | List (_,[Symbol "letrec" ; List (_,vars) ; body]) ->
    let rec aux = function
      | (List (_,[Ident i ; Colon ; ty]) :: expr :: rest) ->
        ((i,type_of_tree ty), lambda_of_tree expr) :: aux rest 
      | [] -> []
      | _ -> failwith "Invalid 'letrec' binding"
    in
    Lletrec (aux vars, lambda_of_tree body)
  | List (_, PSymbol (("switch*"|"switch"), 
                      [PInt sw_numconsts; PInt sw_numblocks]) :: (Ident id) :: cases) ->
    let rec aux consts blocks failaction = function
      | PSymbol ("case", [PString "int"; PInt i]) :: handler :: rest ->
        aux ((i, lambda_of_tree handler) :: consts) blocks failaction rest
      | PSymbol ("case", [PString "tag"; PInt i]) :: handler :: rest ->
        aux consts ((i, lambda_of_tree handler) :: blocks) failaction rest
      | Symbol "default:" :: handler :: rest ->
        aux consts blocks (Some (lambda_of_tree handler)) rest
      | [Colon; ty] -> List.rev consts, List.rev blocks, failaction, ty
      | _ -> failwith "Invalid 'switch' binding"
    in
    let sw_consts, sw_blocks, sw_failaction, ty = aux [] [] None cases in
    let sw = { sw_numconsts ; sw_consts ; 
               sw_numblocks ; sw_blocks ;
               sw_failaction }
    in
    Lswitch (id, sw, type_of_tree ty)
  | List (_,[Symbol "try"; lbody ; Symbol "with"; Ident param ; lhandler]) ->
    Ltrywith (lambda_of_tree lbody, param, lambda_of_tree lhandler)
  | List (_,[Symbol "if"; lcond; lthen; lelse]) ->
    Lifthenelse (lambda_of_tree lcond,
                 lambda_of_tree lthen,
                 lambda_of_tree lelse)
  | List (_, Symbol "seq" :: tail) ->
    let rec aux = function
      | [l] -> lambda_of_tree l
      | l1 :: l2 -> Lsequence (lambda_of_tree l1, aux l2)
      | _ -> failwith "Empty 'seq'"
    in
    aux tail
  | List (_, [Symbol "while" ; lcond ; lbody]) ->
    Lwhile (lambda_of_tree lcond, lambda_of_tree lbody)
  | List (_, [Symbol "for" ; Ident i ; lo ; hi ; Symbol dir ; body]) ->
    let dir = match dir with
      | "to" -> Asttypes.Upto
      | "downto" -> Asttypes.Downto
      | _ -> failwith "Invalid direction in 'for'"
    in
    Lfor (i,lambda_of_tree lo, lambda_of_tree hi, dir, lambda_of_tree body)
  | List (_, [Symbol "assign" ; Ident id ; expr]) ->
    Lassign (id, lambda_of_tree expr)
  | List (_, PSymbol ("exit",[PInt i]) :: lams) ->
    Lstaticraise (i,List.map lambda_of_tree lams)
  | List (_, Symbol "catch" :: lbody :: PSymbol ("with",[PInt i]) :: tail) ->
    let vars, handler = catch_handler tail in
    Lstaticcatch (lambda_of_tree lbody, i, vars, lambda_of_tree handler)

  | List (_, [Symbol "typeabs"; List (_,idents); lam]) ->
    let extract_param = function
      | Ident i -> i
      | _ -> failwith "Invalid 'typeabs'"
    in
    let idents = List.map extract_param idents in
    Ltypeabs (idents,lambda_of_tree lam)

  | List (_, (Symbol "typeapp" :: lam :: tys)) ->
    let lam = lambda_of_tree lam in
    Ltypeapp (lam, List.map type_of_tree tys)

  | List (_, [lam ; Colon ; ty]) ->
    let lam = lambda_of_tree lam and
      ty  = type_of_tree ty    in
    Lascribe (lam,ty)

(* TODO: Lsend, Levent, Lifused *)
  | List (_, Symbol prim :: args) ->
    Lprim (get_prim prim, List.map lambda_of_tree args)
  | List (_, PSymbol (prim,params) :: args) ->
    Lprim (get_prim ~params prim, List.map lambda_of_tree args)

  | Colon | Symbol _ | PSymbol _ | List _ as lam ->
    Lambdaparser_def.print Format.std_formatter lam;
    failwith "Invalid lambda"

