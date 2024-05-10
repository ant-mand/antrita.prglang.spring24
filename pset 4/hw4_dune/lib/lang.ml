module SS = Set.Make (String)
module MS = Map.Make (String)
open List
open Char
open Format
open Tlc

exception Not_implemented
exception CaptureException of string

(* Typechecking *)

(* give a reasonable type to context *)
type ctx = (string * typ) list 

(* define the empty context *)
let empty_ctx : ctx = []

(* look up a given variable's type, returning None if not found *)
let lookup (g : ctx) (x : string) : typ option = 
  List.assoc_opt x g

(* extend a context with a new variable-typ association *)
(* if the variable x already exists in g, replace it with the new type *)
let extend (g : ctx) (x : string) (tp : typ) : ctx = 
  let new_ctx = List.filter(fun(var, _) -> var <> x) g in (x, tp) :: new_ctx

(* return the type of a term in the given context *)
(* return None if the term is ill-typed *)
let rec type_infer (g: ctx) (t: term) = match t with
  | TmVar x -> lookup g x

  | TmAbs (x, tp', body) ->
    let new_ctx = extend g x tp' in
    (match type_infer new_ctx body with
      | Some body_tp -> Some (TFun (tp', body_tp))
      | None -> None)

  | TmApp (t1, t2) ->
    (match (type_infer g t1, type_infer g t2) with
      | (Some (TFun (arg_tp, ret_tp)), Some arg_tp2) 
        when arg_tp = arg_tp2 -> Some ret_tp
      | _ -> None)

  | TmTrue -> Some TBool
  | TmFalse -> Some TBool
  | TmZero -> Some TNat

  | TmSucc t1 | TmPred t1 -> if type_infer g t1 = Some TNat 
    then Some TNat else None
  | TmIsZero t1 -> if type_infer g t1 = Some TNat 
    then Some TBool else None
  | TmIf (t1, t2, t3) ->
    if type_infer g t1 = Some TBool && 
      type_infer g t2 = type_infer g t3 && 
      type_infer g t2 != None 
      then type_infer g t2 else None

(* return true if t has the given type in the context, else return false *)
and type_check (g: ctx) (t: term) (tp: typ) = 
  match type_infer g t with
  | Some tp' when tp' = tp -> true
  | _ -> false
