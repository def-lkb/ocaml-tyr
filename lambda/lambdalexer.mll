{
open Lexing
open Lambdaparser
open Printf
open Asttypes
open Lambda

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_string
  | Literal_overflow of string
;;

exception Error of error * Location.t;;

let buf_contents b =
  let r = Buffer.contents b in
  Buffer.reset b;
  r

let string_start_loc = ref Location.none

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255)
  then raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                     Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = pred (String.length s) and c = ref 0 in
  for i = 0 to  l do
    if s.[i] != '_' then incr c
  done;
  let s' = String.make !c ' ' and c = ref 0 in
  for i = 0 to l do
    if s.[i] != '_' then
    begin 
      s'.[!c] <- s.[i];
      incr c
    end
  done;
  s'

let double_nl = "\013\010"

let found_newline lexbuf diff =
  let curr_p = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    curr_p with
    pos_lnum = curr_p.pos_lnum + 1;
    pos_bol = max 1 (curr_p.pos_cnum - diff);
  }

let token_to_string = function
  | CONST _ -> "CONST(...)"
  | PRIM _ -> "PRIM(...)"
  | IDENT id -> Printf.sprintf "IDENT(%s)" id.Ident.name
  | SYMBOL s -> Printf.sprintf "SYM(%s)" s
  | PSYMBOL s -> Printf.sprintf "SYM[](%s)" s
  | INT i -> Printf.sprintf "INT(%i)" i
  | FLOAT f -> Printf.sprintf "FLOAT(%s)" f
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACKETBAR -> "[|"
  | RBRACKETBAR -> "|]"
  | COLON -> ":"
  | COMMA -> ","
  | EOF -> "EOF"
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let whitespace = [' ' '\010' '\013' '\009' '\012']

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*

let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*

let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*

let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*

let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token buf = parse
  | newline { found_newline lexbuf 1; token buf lexbuf }
  | whitespace+ { token buf lexbuf }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '['  { LBRACKET }
  | ']'  { RBRACKET }
  | "[|" { LBRACKETBAR }
  | "|]" { RBRACKETBAR }
  | ':'  { COLON }
  | ','  { COMMA }
  | (int_literal as lit) (('a'|'l'|'L'|'n')? as kind)
    { try
        begin match kind with
          | ""  -> INT (cvt_int_literal lit)
          | "a" -> CONST (Const_pointer (cvt_int_literal lit))
          | "l" -> CONST (Const_base (Const_int32 (cvt_int32_literal lit)))
          | "L" -> CONST (Const_base (Const_int64 (cvt_int64_literal lit)))
          | "n" -> CONST (Const_base (Const_nativeint (cvt_nativeint_literal lit)))
          | _   -> assert false
        end
      with Failure _ ->
        let k = match kind with
          | "" -> "int"
          | "a" -> "pointer"
          | "l" -> "int32"
          | "L" -> "int64"
          | "n" -> "nativeint"
          | _ -> assert false
        in
        raise (Error (Literal_overflow k, Location.curr lexbuf))
    }

  | float_literal as lit
    { FLOAT (remove_underscores lit) }
  | "#\""
    { let string_start = lexbuf.lex_start_p in
      string_start_loc := Location.curr lexbuf;
      scan_string buf lexbuf;
      lexbuf.lex_start_p <- string_start;
      CONST (Const_immstring (buf_contents buf)) }
  | '"'
    { let string_start = lexbuf.lex_start_p in
      string_start_loc := Location.curr lexbuf;
      scan_string buf lexbuf;
      lexbuf.lex_start_p <- string_start;
      CONST (Const_base (Const_string (buf_contents buf))) }

  (* Identifiers *)
  | (([^ ';' '[' ']' '/' ',' '(' ')' '"'] # whitespace)+ as name)
    '/' (int_literal as i) ('g'? as g)
    { try 
        let ident = { Ident. name ; flags = 0 ; stamp = (cvt_int_literal i) } in
        (match g with "g" -> Ident.make_global ident | _ -> ());
        IDENT ident
      with Failure _ ->
        raise (Error (Literal_overflow "Ident stamp", Location.curr lexbuf)) }
  | (([^ ';' '[' ']' '(' ')' '"'] # whitespace)+ as name) (['!' '#'] as tag)
    { IDENT { Ident. name ; flags = 0 ; stamp = match tag with '#' -> -1 | '!' | _ -> 0 } }

  (* Symbols *)
  | (([^ ';' '[' ']' '/' ',' '(' ')' '"'] # whitespace)+ as str) ('['? as param)
    { match param with "[" -> PSYMBOL str | _ -> SYMBOL str }

  | "'" newline "'"
    { let c = Lexing.lexeme_char lexbuf 1 in
      found_newline lexbuf 1;
      CONST (Const_base (Const_char c)) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
    { let c = Lexing.lexeme_char lexbuf 1 in
      CONST (Const_base (Const_char c)) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { let c = Lexing.lexeme_char lexbuf 2 in
      CONST (Const_base (Const_char c)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { let c = char_for_decimal_code lexbuf 2 in
      CONST (Const_base (Const_char c)) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { let c = char_for_hexadecimal_code lexbuf 3 in
      CONST (Const_base (Const_char c)) }
  | eof { EOF }

and scan_string buf = parse
  | '"'
    { () }
  | '\\' newline ([' ' '\t']*)
    { (*update_loc lexbuf None 1 false (String.length space);*)
      scan_string buf lexbuf }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    { Buffer.add_char buf (char_for_backslash (Lexing.lexeme_char lexbuf 1));
      scan_string buf lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { Buffer.add_char buf (char_for_decimal_code lexbuf 1);
      scan_string buf lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { Buffer.add_char buf (char_for_hexadecimal_code lexbuf 2);
      scan_string buf lexbuf }
  | '\\' _
    { (* Should be an error, but we are very lax.
       * raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
       * Location.curr lexbuf)) *)
      (* let loc = Location.curr lexbuf in
       * Location.prerr_warning loc Warnings.Illegal_backslash; *)
      Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
      Buffer.add_char buf (Lexing.lexeme_char lexbuf 1);
      scan_string buf lexbuf }
  | newline
    { (* if not (in_comment ()) then
         Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string; *)
      (*update_loc lexbuf None 1 false 0;*)
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      scan_string buf lexbuf }
  | eof
    { raise (Error (Unterminated_string, !string_start_loc)) }
  | _
    { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0);
      scan_string buf lexbuf }

{
  let token ?(buf=Buffer.create 64) = token buf
}
