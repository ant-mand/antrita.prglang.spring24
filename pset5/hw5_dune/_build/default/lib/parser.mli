
(* The type of tokens. *)

type token = 
  | ZERO
  | VAR of (string)
  | UNITT
  | UNIT
  | UNDERSCORE
  | TWO
  | TRUE
  | TIMES
  | THEN
  | TAIL
  | SUCC
  | SOME
  | SMALL_OPTION
  | SEMICOLON
  | RP
  | RCB
  | RB
  | PRED
  | PLUS
  | PIPE
  | OPTION
  | ONE
  | OF
  | NONE
  | NIL
  | NAT
  | LP
  | LIST
  | LETREC
  | LET
  | LCB
  | LB
  | LAM
  | ISZERO
  | ISNIL
  | INR
  | INL
  | IN
  | IF
  | HEAD
  | FIX
  | FATARROW
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DOT
  | CONS
  | COMMA
  | COLON
  | CASE
  | BOOL
  | AS
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val ptyp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Etlc.typ)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Etlc.term)
