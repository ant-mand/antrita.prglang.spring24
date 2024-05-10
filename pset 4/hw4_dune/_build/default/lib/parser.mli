
(* The type of tokens. *)

type token = 
  | ZERO
  | VAR of (string)
  | TRUE
  | THEN
  | SUCC
  | RP
  | PRED
  | NAT
  | LP
  | LAM
  | ISZERO
  | IF
  | FALSE
  | EOF
  | ELSE
  | DOT
  | COLON
  | BOOL
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tlc.term)
