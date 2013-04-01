open Lambda
open Lambdaparser_def

let type_const_of_string = function
  | "int"       -> Lt_const_int      
  | "char"      -> Lt_const_char     
  | "string"    -> Lt_const_string   
  | "float"     -> Lt_const_float    
  | "int32"     -> Lt_const_int32    
  | "int64"     -> Lt_const_int64    
  | "nativeint" -> Lt_const_nativeint
  | _ -> failwith "Unknown type constant"

let rec type_of_tree = function
  | List (_,[Symbol "top"]) ->
    Lt_top
  | List (_,[Symbol "exn"]) ->
    lt_top_block
  | List (_,[ta; Symbol "->"; tb]) ->
    Lt_arrow (type_of_tree ta, type_of_tree tb)
  | List (_,[Symbol "array"; t]) ->
    Lt_array (type_of_tree t)
  | List (_,[Symbol "mu"; Ident v; t]) ->
    Lt_mu (v, type_of_tree t)
  | List (_,[Symbol "forall"; List (_,idents); t]) ->
    let extract_param = function
      | Ident i -> i
      | _ -> failwith "Invalid 'forall'"
    in
    let idents = List.map extract_param idents in
    Lt_forall (idents, type_of_tree t)
  | List (_,Symbol "block" :: tags) ->
    let rec aux consts blocks = function
      | [] -> consts, blocks
      | Const (Const_base (Asttypes.Const_int i)) :: rest ->
        aux (i :: consts) blocks rest
      | List (_,(Const (Const_pointer i) :: Colon :: tys)) :: rest ->
        aux consts ((i, List.map type_of_tree tys) :: blocks) rest
      | _ -> failwith "Invalid 'block'"
    in 
    let lt_consts, lt_blocks = aux [] [] tags in
    Lt_block { lt_consts ; lt_blocks }
  | Ident i ->
    Lt_var i
  | Symbol s ->
    Lt_const (type_const_of_string s)
  | _ -> failwith "Unrecognized type"


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
    Lstaticcatch (lambda_of_tree lbody, (i, vars), lambda_of_tree handler)

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

