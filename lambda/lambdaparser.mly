%{
open Lexing
open Asttypes
open Lambda
module Def = Lambdaparser_def

let parse_failure what =
  let pos = symbol_start_pos () in
  let msg =
    Printf.sprintf "Lambdaparser: failed to parse line %d char %d: %s"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol) what in
  failwith msg
%}

%token <Lambda.structured_constant> CONST
%token <Lambda.primitive> PRIM
%token <Ident.t> IDENT
%token <string> SYMBOL
%token <string> PSYMBOL
%token <int> INT
%token <string> FLOAT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACKETBAR RBRACKETBAR
%token COLON COMMA
%token EOF

%start lambda
%type <Lambdaparser_def.t> lambda

%%

lambda:
  | CONST { Def.Const $1 }
  | IDENT { Def.Ident $1 }
  | INT   { Def.Const (Const_base (Const_int $1)) }
  | FLOAT { Def.Const (Const_base (Const_float $1)) }
  | COLON { Def.Colon }
  | SYMBOL { Def.Symbol $1 }
  | PSYMBOL symbol_params RBRACKET { Def.PSymbol ($1, List.rev $2) }
  | PSYMBOL symbol_params RBRACKET COLON { Def.PSymbol ($1, List.rev $2) }
  | LPAREN RPAREN { Def.List (Location.symbol_rloc (), []) }
  | LPAREN lambda_list RPAREN { Def.List (Location.symbol_rloc (), List.rev $2) }
  | LBRACKET INT RBRACKET { Def.Const (Const_block ($2, [])) }
  | LBRACKET INT COLON const_list RBRACKET { Def.Const (Const_block ($2, List.rev $4)) }
  | LBRACKETBAR RBRACKETBAR { Def.Const (Const_float_array []) }
  | LBRACKETBAR float_list RBRACKETBAR { Def.Const (Const_float_array (List.rev $2)) }
  | error { parse_failure "sexp" }

lambda_list:
  | lambda { [$1] }
  | lambda_list lambda { $2 :: $1 }

const:
  | INT { Const_base (Const_int $1) }
  | FLOAT { Const_base (Const_float $1) }
  | CONST { $1 }
  | LBRACKET INT RBRACKET { Const_block ($2, []) }
  | LBRACKET INT COLON const_list RBRACKET { Const_block ($2, List.rev $4) }
  | LBRACKETBAR RBRACKETBAR { Const_float_array [] }
  | LBRACKETBAR float_list RBRACKETBAR { Const_float_array (List.rev $2) }

const_list:
  | const { [$1] }
  | const_list const { $2 :: $1 }

float_list:
  | FLOAT { [$1] }
  | float_list FLOAT { $2 :: $1 }

symbol_param:
  | SYMBOL { Def.PString $1 }
  | INT { Def.PInt $1 }
  | IDENT { Def.PIdent $1 }

symbol_params:
  | symbol_param { [$1] }
  | symbol_params COMMA symbol_param { $3 :: $1 }
