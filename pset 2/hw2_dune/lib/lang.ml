open List
open Char
open Format

exception Not_implemented
exception Parse_exn

(* Data Definitions *)

type token =
  | LParen
  | RParen
  | TokTrue
  | TokFalse
  | TokZero
  | TokIf
  | TokSucc
  | TokPred
  | TokIsZero
  | TokAnd
  | TokOr
  | TokNot
  | TokPlus

type term =
  | True
  | False
  | Zero
  | If of term * term * term
  | Succ of term
  | Pred of term
  | IsZero of term
  | And of term * term
  | Or of term * term
  | Not of term
  | Plus of term * term

(* Utilities *)

(* Strings in ocaml are not represented as lists of characters. For lexing and parsing purposes, it's easier to work
   with lists of characters. You shouldn't need to touch these functions that convert between the two, but feel free to use them for debugging. *)
let string_to_char_list (s : string) : char list =
  s |> String.to_seq |> List.of_seq

let char_list_to_string (cl : char list) : string =
  cl |> List.to_seq |> String.of_seq

(* Value Judgements *)

let rec is_num_val (t: term) : bool = 
  match t with
  | Zero -> true
  | Succ tm -> is_num_val tm
  | _ -> false

(* Returns true if the term is a value, false otherwise. *)
let is_val (t: term) : bool = 
  match t with
  | True | False -> true
  | _ -> is_num_val t

(* Lexical Scanner *)

(* nextToken should return the next token from a list of characters, along with the characters thereafter
    - return None if the list of chars is empty
    - skip whitespace characters
    - throw an exception if the characters cannot be tokenized
   Some basic cases have been given for you. *)
let rec nextToken (cs: char list) : (token * char list) option = 
  match cs with
  | [] -> None 
  | ' '::tl | '\n'::tl | '\t'::tl -> nextToken tl (* skip whitespace *)

  | '('::tl -> Some (LParen, tl)
  | 't'::'r'::'u'::'e'::tl -> Some (TokTrue, tl)
  | 'f'::'a'::'l'::'s'::'e'::tl -> Some (TokFalse, tl)
  | '0'::tl -> Some (TokZero, tl)
  | 'i'::'f'::tl -> Some (TokIf, tl)
  | 's'::'u'::'c'::'c'::tl -> Some (TokSucc, tl)
  | 'p'::'r'::'e'::'d'::tl -> Some (TokPred, tl)
  | 'i'::'s'::'z'::'e'::'r'::'o'::tl -> Some (TokIsZero, tl)
  | 'a'::'n'::'d'::tl -> Some (TokAnd, tl)
  | 'o'::'r'::tl -> Some (TokOr, tl)
  | 'n'::'o'::'t'::tl -> Some (TokNot, tl)
  | 'p'::'l'::'u'::'s'::tl -> Some (TokPlus, tl)
  | ')'::tl -> Some (RParen, [])

  | _ -> raise Parse_exn


(* turn a string of code (like "(pred 0)" into a list of tokens (like [LParen, TokPred, TokZero, RParen]) *)
(* turn a string of code (like "(pred 0)" into a list of tokens (
   like [LParen, TokPred, TokZero, RParen]) *)
let rec scan (ls : char list) : token list = 
  match nextToken ls with
  | Some (token, rest) -> token :: scan rest
  | None -> []


(* Parser *)

(* nextTerm should return the next term from a list of tokens, along with the tokens thereafter
   - return None if the list of tokens is empty
   - throw an exception if the characters cannot be tokenized *)
let rec nextTerm (ts: token list) : (term * token list) option = 
  match ts with
  | [] -> None

  | TokTrue::tl -> Some (True, tl)
  | TokFalse::tl -> Some (False, tl)
  | TokZero::tl -> Some (Zero, tl)

  | LParen :: TokIf :: tl -> (
    match nextTerm tl with
    | Some (t1, rest) ->
        (match nextTerm rest with
        | Some (t2, rest') ->
            (match nextTerm rest' with
            | Some (t3, RParen :: rest'') -> Some (If (t1, t2, t3), rest'')
            | _ -> raise Parse_exn) 
        | _ -> raise Parse_exn)     
    | _ -> raise Parse_exn          
  )

  | LParen :: TokSucc :: tl -> (
    match nextTerm tl with
    | Some (t1, RParen :: rest) -> Some (Succ (t1), rest)
    | _ -> raise Parse_exn)

  | LParen :: TokPred :: tl -> (
    match nextTerm tl with
    | Some (t1, RParen :: rest) -> Some (Pred (t1), rest)
    | _ -> raise Parse_exn)

  | LParen :: TokIsZero :: tl -> (
    match nextTerm tl with
    | Some (t1, RParen :: rest) -> Some (IsZero (t1), rest)
    | _ -> raise Parse_exn)

  | LParen :: TokNot :: tl -> (
      match nextTerm tl with
      | Some (t1, RParen :: rest) -> Some (Not (t1), rest)
      | _ -> raise Parse_exn)

  | LParen :: TokAnd :: tl -> (
    match nextTerm tl with
    | Some (t1, rest) -> 
      (match nextTerm rest with
      | Some (t2, RParen :: rest') -> Some (And (t1, t2), rest')
      | _ -> raise Parse_exn)
    | _ -> raise Parse_exn)

  | LParen :: TokOr :: tl -> (
    match nextTerm tl with
    | Some (t1, rest) -> 
      (match nextTerm rest with
      | Some (t2, RParen :: rest') -> Some (Or (t1, t2), rest')
      | _ -> raise Parse_exn)
    | _ -> raise Parse_exn)

  | LParen :: TokPlus :: tl -> (
    match nextTerm tl with
    | Some (t1, rest) -> 
      (match nextTerm rest with
      | Some (t2, RParen :: rest') -> Some (Plus (t1, t2), rest')
      | _ -> raise Parse_exn)
    | _ -> raise Parse_exn)

  | _ -> raise Parse_exn

(* turn a list of tokens (like [LParen ,TokPred, TokZero, RParen] into a term (like Pred 0) *)
let parse (tokens : token list) : term = 
  match nextTerm tokens with
  | Some (term, []) -> term  
  | Some (_, _ :: _) -> raise Parse_exn  
  | None -> raise Parse_exn  


(* Small Step evaluator *)

(* Implement the small-step evaluation relation from class.
   For And, Or and Not, you should be able to determine
   appropriate rules by extrapolating from the If rules.
   If a term is not a normal form, take the next possible step
   - i.e., if t -> u, then step(t) should return Some(u)
   if a term is a normal form, return None *)
let rec small_step (t : term) : term option = 
  match t with
  | Zero | True | False -> None 
  | Succ tm ->
    (match is_num_val tm with
    | true -> None
    | false -> (
      match small_step tm with
      | Some t' -> Some (Succ t')
      | None -> None))

  | Pred tm -> 
    (match tm with
    | Zero -> Some (Zero)
    | Succ nv when is_num_val nv -> Some (nv)
    | _ -> 
      (match small_step tm with
      | Some t' -> Some (Pred t')
      | None -> None))

  | IsZero tm ->
    (match tm with
    | Zero -> Some True
    | Succ nv when is_num_val nv -> Some False
    | _ ->
      (match small_step tm with
      | Some t' -> Some (IsZero t')
      | None -> None))

  | If (t1, t2, t3) ->
    (match t1 with
    | True -> Some t2
    | False -> Some t3
    | _ ->
      (match small_step t1 with
      | Some t' -> Some (If (t', t2, t3))
      | None -> None))

  | And (t1, t2) ->
    (match t1, t2 with
    | (True, _ )-> Some t2
    | (False, _) -> Some False
    | (_, _)->
      (match small_step t1 with
      | Some t' -> Some (And (t', t2))
      | None -> 
        (match small_step t2 with
        | Some t'' -> Some (And (t1, t''))
        | None -> None)))

  | Or (t1, t2) ->
    (match t1, t2 with
    | True, _ -> Some True
    | False, _ -> Some t2
    | _, _ ->
      (match small_step t1 with
      | Some t' -> Some (Or (t', t2))
      | None -> 
        (match small_step t2 with
        | Some t'' -> Some (Or (t1, t''))
        | None -> None)))

  | Not tm ->
    (match tm with
    | True -> Some False
    | False -> Some True
    | _ ->
      (match small_step tm with
      | Some t' -> Some (Not (t'))
      | None -> None))

  | Plus (t1, t2) ->
    (match t1, t2 with
    | (_, True) | (_, False) | (True, _) | (False, _) -> None
    | Zero, _ -> 
      if is_val t2 
        then Some t2 
      else (
        match small_step t2 with
        | Some t' -> Some (Plus (t', t2))
        | None -> None)
    | _, Zero ->
      if is_val t1 
        then Some t1 
      else (
        match small_step t1 with
        | Some t' -> Some (Plus (t1, t'))
        | None -> None)
    | (Succ n1, _) when is_num_val n1 -> 
      (match small_step t2 with
      | Some t2' -> Some (Plus (t1, t2'))
      | None -> None)
    | (_, Succ n2) when is_num_val n2 ->
      (match small_step t1 with
      | Some t1' -> Some (Plus (t1', t2))
      | None -> None)
    | _, _ -> 
      (match small_step t1 with
      | Some t' -> Some (Plus (t', t2))
      | None -> 
        (match small_step t2 with
        | Some t'' -> Some (Plus (t1, t''))
        | None -> None)))

(* Returns true if the term is a normal form, false otherwise. *)
let is_normal (t: term) : bool =  
  match small_step t with
  | None -> true
  | Some _ -> false
  
(* Returns true if the term is stuck, false otherwise. *)
let is_stuck (t: term) : bool = 
  (is_normal t) && not (is_val t)

(* Given t, return t' such that t ->* t' and t' is a normal form. *)
let rec multistep_full (t : term) : term =
  match is_normal t with 
  | true -> t
  | false -> 
    match small_step t with
    | Some t' -> multistep_full t'
    | None -> t

(* Returns true if a term steps to a value, and false otherwise. *)
let rec multisteps_to_value (t : term) : bool = 
  if is_val t then
    true
  else
    match small_step t with
    | Some t' -> multisteps_to_value t'
    | None -> false 

(* Big Step evaluator *)

(* Now implement the big-step relation from class.
   Again, return none if the program doesn't (big) step.
   You should be able to infer the big step semantics of
   and, or, not and plus from the small-step ones. *)
let rec big_step (t : term) : term option = 
  match t with
  | Zero -> Some Zero
  | True -> Some True
  | False -> Some False

  | Succ tm ->
    (match big_step tm with
    | Some n when is_num_val n -> Some (Succ n)
    | _ -> None)

  | Pred tm ->
    (match big_step tm with
    | Some Zero -> Some Zero
    | Some (Succ n) when is_num_val n -> Some n
    | _ -> None)

  | IsZero tm ->
    (match big_step tm with
    | Some Zero -> Some True
    | Some (Succ _) -> Some False
    | _ -> None)

  | If (t1, t2, t3) ->
    (match big_step t1 with
    | Some True -> big_step t2
    | Some False -> big_step t3
    | _ -> None)

  | Plus (t1, t2) ->
    (match big_step t1, big_step t2 with
    | Some Zero, Some n2 when is_num_val n2 -> Some n2
    | Some n1, Some Zero when is_num_val n1 -> Some n1
    | Some (Succ n1), Some n2 when is_num_val n1 && is_num_val n2 -> big_step (Plus (n1, Succ n2))
    | _ -> None)

  | And (t1, t2) ->
    (match big_step t1, big_step t2 with
    | Some True, Some True -> Some True
    | Some True, _ -> Some t2
    | Some False, Some False -> Some False
    | Some False, _ -> Some False
    | _, Some False -> None
    | _ -> None)

  | Or (t1, t2) ->
    (match big_step t1, big_step t2 with
    | Some True, _ | _, Some True -> Some True
    | Some False, Some False -> Some False
    | Some False, _-> Some t2
    | _ -> None)

  | Not tm ->
    (match big_step tm with
    | Some True -> Some False
    | Some False -> Some True
    | _ -> None)


(* Interpreter *)

(* You should not need to modify this code -- feel free to add statements for debugging,
   but remember to delete them before submitting. *)

let rec term_to_string (t : term) : string =
  match t with
  | True -> "true"
  | False -> "false"
  | Zero -> "zero"
  | If (t1, t2, t3) ->
      "(" ^ "if " ^ term_to_string t1 ^ " then " ^ term_to_string t2 ^ " else "
      ^ term_to_string t3 ^ ")"
  | Succ t1 -> "(" ^ "succ " ^ term_to_string t1 ^ ")"
  | Pred t1 -> "(" ^ "pred " ^ term_to_string t1 ^ ")"
  | IsZero t1 -> "(" ^ "iszero " ^ term_to_string t1 ^ ")"
  | And (t1, t2) -> "(" ^ term_to_string t1 ^ " and " ^ term_to_string t2 ^ ")"
  | Or (t1, t2) -> "(" ^ term_to_string t1 ^ " or " ^ term_to_string t2 ^ ")"
  | Not t1 -> "(" ^ "not " ^ term_to_string t1 ^ ")"
  | Plus (t1, t2) -> "(" ^ term_to_string t1 ^ " + " ^ term_to_string t2 ^ ")"
