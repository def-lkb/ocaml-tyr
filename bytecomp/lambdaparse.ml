open Lambda
open Lambdaparser_def

let type_of_tree _ = Lt_bot

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
    | Ident id -> id
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
        | (Ident i :: expr :: rest) ->
           (i, lambda_of_tree expr) :: aux rest 
        | [] -> []
        | _ -> failwith "Invalid 'letrec' binding"
      in
      Lletrec (aux vars, lambda_of_tree body)
  | List (_, PSymbol (("switch*"|"switch"), [PInt sw_numconsts; PInt
  sw_numblocks]) :: (Ident id) :: cases) ->
      let rec aux consts blocks = function
        | PSymbol ("case", [PString "int"; PInt i]) :: handler :: rest ->
            aux ((i, lambda_of_tree handler) :: consts) blocks rest
        | PSymbol ("case", [PString "tag"; PInt i]) :: handler :: rest ->
            aux consts ((i, lambda_of_tree handler) :: blocks) rest
        | [] -> None, List.rev consts, List.rev blocks
        | [Symbol "default:" ; handler] -> Some (lambda_of_tree handler),
                                           List.rev consts, List.rev blocks
        | _ -> failwith "Invalid 'switch' binding"
      in
      let sw_failaction, sw_consts, sw_blocks = aux [] [] cases in
      Lswitch (id, { sw_numconsts ; sw_numblocks ; sw_consts ; sw_blocks ; sw_failaction })
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
  (* TODO: Lsend, Levent, Lifused *)
  | List (_, Symbol prim :: args) ->
      Lprim (get_prim prim, List.map lambda_of_tree args)
  | List (_, PSymbol (prim,params) :: args) ->
      Lprim (get_prim ~params prim, List.map lambda_of_tree args)

  | Colon | Symbol _ | PSymbol _ | List _ as lam ->
      Lambdaparser_def.print Format.std_formatter lam;
      failwith "Invalid lambda"

