
(* The type of tokens. *)

type token = 
  | ZERO
  | WITH
  | VAR of (string)
  | UNITT
  | UNIT
  | UNDERSCORE
  | TRY
  | TRUE
  | THEN
  | SUCC
  | SOME
  | SEMICOLON
  | RP
  | REFT
  | REF
  | RB
  | RAISE
  | PRED
  | PIPE
  | OPTION
  | OF
  | NULL
  | NONE
  | NAT
  | LP
  | LB
  | LAM
  | ISZERO
  | ISNULL
  | IF
  | FATARROW
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DOT
  | COLON
  | CASE
  | BOT
  | BOOL
  | BANG
  | ASSN
  | AS
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val ptyp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Etlc.typ)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Etlc.term)
