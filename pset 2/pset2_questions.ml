open List
open Char
open Format

exception Not_implemented
exception Parse_exn

(* Data Definitions *)

type token
= LParen
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

type term
= True
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
let string_to_char_list (s: string) : char list =
  s |> String.to_seq |> List.of_seq
  
let char_list_to_string (cl: char list) : string =
  cl |> List.to_seq |> String.of_seq
  
(* Value Judgements *)

(* Returns true if the term is a numeric value, false otherwise. *)
let rec is_num_val (t: term) : bool = raise Not_implemented

(* Returns true if the term is a value, false otherwise. *)
let is_val (t: term) : bool = raise Not_implemented

(* Lexical Scanner *)

(* nextToken should return the next token from a list of characters, along with the characters thereafter
   - return None if the list of chars is empty
   - skip whitespace characters
   - throw an exception if the characters cannot be tokenized 
  Some basic cases have been given for you. *)
let rec nextToken (cs: char list) : (token * char list) option = 
  match cs with
  | [] -> None
  | '('::tl -> Some (LParen, tl)
  | 't'::'r'::'u'::'e'::tl -> Some (TokTrue, tl)
  | _ -> raise Parse_exn

(* turn a string of code (like "(pred 0)" into a list of tokens (like [LParen, TokPred, TokZero, RParen]) *)
let rec scan (ls : char list) : token list = raise Not_implemented

(* Parser *)

(* nextTerm should return the next term from a list of tokens, along with the tokens thereafter
   - return None if the list of tokens is empty
   - throw an exception if the characters cannot be tokenized *)
let rec nextTerm (ts: token list) : (term * token list) option = raise Not_implemented

(* turn a list of tokens (like [LParen ,TokPred, TokZero, RParen] into a term (like Pred 0) *)
let parse (tokens : token list) : term = raise Not_implemented


(* Small Step evaluator *)

(* Implement the small-step evaluation relation from class. 
   For And, Or and Not, you should be able to determine 
   appropriate rules by extrapolating from the If rules.
   If a term is not a normal form, take the next possible step
   - i.e., if t -> u, then step(t) should return Some(u)
   if a term is a normal form, return None *)
let rec small_step (t : term) : term option = raise Not_implemented

(* Returns true if the term is a normal form, false otherwise. *)
let is_normal (t: term) : bool = raise Not_implemented

(* Returns true if the term is stuck, false otherwise. *)
let is_stuck (t: term) : bool = raise Not_implemented

(* Given t, return t' such that t ->* t' and t' is a normal form. *)
let rec multistep_full (t : term) : term = raise Not_implemented

(* Returns true if a term steps to a value, and false otherwise. *)
let rec multisteps_to_value (t: term) : bool = raise Not_implemented

(* Big Step evaluator *)

(* Now implement the big-step relation from class. 
   Again, return none if the program doesn't (big) step. 
   You should be able to infer the big step semantics of
   and, or, not and plus from the small-step ones. *)
let rec big_step (t : term) : term option = raise Not_implemented

(* Interpreter *)

(* You should not need to modify this code -- feel free to add statements for debugging,
   but remember to delete them before submitting. *)

let rec term_to_string (t : term) : string = match t with
| True -> "true"
| False -> "false"
| Zero -> "zero"
| If (t1, t2, t3) -> "(" ^ "if " ^ term_to_string t1 ^ " then " ^ term_to_string t2 ^ " else " ^ term_to_string t3  ^ ")"
| Succ (t1) -> "(" ^ "succ " ^ term_to_string t1 ^ ")"
| Pred (t1) -> "(" ^ "pred " ^ term_to_string t1 ^ ")"
| IsZero (t1) ->  "(" ^ "iszero " ^ term_to_string t1 ^ ")"
| And (t1, t2) -> "(" ^ term_to_string t1 ^ " and " ^ term_to_string t2 ^ ")"
| Or (t1, t2) -> "(" ^ term_to_string t1 ^ " or " ^ term_to_string t2 ^ ")"
| Not (t1) -> "(" ^ "not " ^ term_to_string t1 ^ ")"
| Plus (t1, t2) -> "(" ^ term_to_string t1 ^ " + " ^ term_to_string t2 ^ ")"

let opt_term_to_string (o : term option) : string = 
  match o with
  | None -> " "
  | Some t -> term_to_string t 

  let interpret (str : string) : unit =
    let chars = string_to_char_list str in
    let tokens = scan chars in
    let ast = parse tokens in
    let ss_term = small_step ast in
    let bs_term = big_step ast in
    let ms_term = multistep_full ast in
    let term_str = term_to_string ast in 
    let ss_term_str = opt_term_to_string ss_term in
    let bs_term_str = opt_term_to_string bs_term in
    let ms_term_str = term_to_string ms_term in
    let _ = print_endline ("----- Small Step Evaluation -----") in
    let _ = print_endline ("      " ^ term_str) in 
    let _ = print_endline ("->    " ^ ss_term_str) in
    let _ = print_endline "" in
    let _ = print_endline "-----------------------------------" in
    let _ = print_endline "" in
    let _ = print_endline ("----- Big Step Evaluation -----") in
    let _ = print_endline ("      " ^ term_str) in 
    let _ = print_endline ("||    " ^ bs_term_str) in
    let _ = print_endline "" in
    let _ = print_endline "-----------------------------------" in
    let _ = print_endline "" in
    let _ = print_endline ("----- Multi Step Full Evaluation ------") in
    let _ = print_endline ("      " ^ term_str) in 
    print_endline ("-->*    " ^ ms_term_str);;

interpret Sys.argv.(1)
