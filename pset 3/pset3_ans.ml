module SS = Set.Make(String)

open List
open Char
open Format

exception Not_implemented
exception Parse_exn
exception CaptureException of string


(* Data Definitions *)

type term
= TmVar of string
| TmAbs of string * term
| TmApp of term * term


(* Alpha Conversion *)

(* takes a list of variable strings and produces a new string not in the set *)
let fresh_var (vars : SS.t) = 
  raise Not_implemented

(* takes a term of the form λx.t1 and turns it into an equivalent λy.t1', where
   y is a fresh variable and t1' is t1 with (free) x's replaced with y's. 
   If t is not a lambda abstraction return a NotAbs exception *)
let alpha_convert  (t : term) (vars : SS.t) : term = 
  raise Not_implemented


(* Substitution *)

(* Implement the substitution function `[x |-> s] t` discussed in class. *)

(* Get the free variables of t as a string set . *)
let rec fv (t:term) : SS.t = 
  raise Not_implemented

let rec subst (x : string) (s : term) (t : term) : term = 
  raise Not_implemented


(* Small-step evaluation *)

(* Implement the small-step evaluation relations from class. 
   We will implement both variants from class: call-by-name and
   call-by-value. 
   We will also implement a third approach: Full beta reduction,
   which reduces terms under a lambda. 
   Return none if a term doesn't step. *)
  
let rec cbn (t : term) : term option = 
  raise Not_implemented

let rec cbv (t : term) : term option = 
  raise Not_implemented

let rec beta (t : term) : term option = 
  raise Not_implemented


(* Given an evaluation strategy above and a term t, return t' 
  such that t ->* t' and t' is a normal form for the given evaluation 
  strategy. *)

let rec multistep (strat : term -> term option) (t : term) : term = 
  raise Not_implemented


(* Define the boolean terms true and false as given in class.
  (We'll use the book's `tru` and `fls` to avoid notation clashes.)
   Define a lambda term that implements an `xor` operation on bools. *)

let tru : term = 
  raise Not_implemented

let fls : term = 
  raise Not_implemented

let xor : term = 
  raise Not_implemented


(* Define triples (tuples of length three) using lambda calculus.
   Define fst, snd and trd expressions that can extract the relevant 
   members from the triples *)

let triple : term = 
  raise Not_implemented

let fst : term = 
  raise Not_implemented

let snd : term = 
  raise Not_implemented

let trd : term = 
  raise Not_implemented