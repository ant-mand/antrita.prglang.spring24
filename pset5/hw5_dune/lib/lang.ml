open List
open Char
open Format

exception Parse_exn
exception NotAbs of string
exception NotFound of string
exception DuplicateVar of string

open Etlc

(* Small-step evaluation *)

(* Implement the small-step evaluation relations from class.
   Note that we're only concerned with call-by-value for this homework.
   Feel free to reuse code from homework 3. *)

let rec substitute (x : string) v (t : term) : term = 
  match t with
  | TmVar y -> if x = y then v else t
  | TmAbs(y, ty, t1) -> if x = y then TmAbs(y, ty, t1) else TmAbs(y, ty, substitute x v t1)
  | TmApp(t1, t2) -> TmApp(substitute x v t1, substitute x v t2)
  | TmIf(t0, t1, t2) -> TmIf(substitute x v t0, substitute x v t1, substitute x v t2)
  | TmSucc t -> TmSucc (substitute x v t)
  | TmPred t -> TmPred (substitute x v t)
  | TmIsZero t -> TmIsZero (substitute x v t)
  | TmPair(t1, t2) -> TmPair(substitute x v t1, substitute x v t2)
  | TmFst t -> TmFst (substitute x v t)
  | TmSnd t -> TmSnd (substitute x v t)
  | TmInl(tp, t) -> TmInl(tp, substitute x v t)
  | TmInr(tp, t) -> TmInr(tp, substitute x v t)
  | TmCase(t0, y1, t1, y2, t2) ->
      let newt1 = if x = y1 then t1 else substitute x v t1 in
      let newt2 = if x = y2 then t2 else substitute x v t2 in
      TmCase(substitute x v t0, y1, newt1, y2, newt2)
  | TmNil _ | TmTrue | TmFalse | TmZero | TmUnit -> t
  | _ -> t

let is_val (t : term) : bool = 
  match t with
  | TmAbs(_, TBool, _) | TmAbs(_, TNat, _) | TmAbs(_, TUnit, _) -> true
  | _ -> false

let rec is_numeric_value (t: term) : bool =
  match t with
  | TmZero -> true
  | TmSucc t -> is_numeric_value t
  | _ -> false

and cbv (t : term) : term option =
  match t with
  | TmVar _ -> None
  | TmTrue | TmFalse | TmZero | TmUnit | TmNil _ -> Some t

  | TmAbs(x, ty, t) -> Some (TmAbs(x, ty, t))
  | TmApp(t1, t2) ->
    (match cbv t1 with
    | Some (TmAbs(x, _, body)) ->
        (match cbv t2 with
        | Some v2 -> Some (substitute x v2 body) 
        | None -> None)
    | Some _ -> None
    | None -> None)

  | TmIf(t1, t2, t3) -> 
    (match cbv t1 with
    | Some TmTrue -> cbv t2
    | Some TmFalse -> cbv t3
    | Some _ | None -> None)

  | TmSucc(t1) -> 
    (match cbv t1 with
    | Some nv ->
      (match nv with
      | TmZero -> Some (TmSucc TmZero)
      | TmSucc _ -> Some nv
      | _ -> None)
    | None -> None)
  | TmPred(t1) ->
    (match cbv t1 with
    | Some TmZero -> Some TmZero
    | Some (TmSucc nv) when is_numeric_value nv -> Some nv
    | Some _ | None -> None)
  | TmIsZero(t1) ->
    (match cbv t1 with
    | Some TmZero -> Some TmTrue
    | Some (TmSucc nv) when is_numeric_value nv -> Some TmFalse
    | Some _ | None -> None)

  | TmPair(t1, t2) ->
    (match cbv t1, cbv t2 with
    | Some v1, Some v2 -> Some (TmPair(v1, v2))
    | _ -> None)
  | TmFst(t1) -> 
    (match cbv t1 with
    | Some (TmPair(v1, _)) -> Some v1
    | Some _ | None -> None)
  | TmSnd(t1) ->
    (match cbv t1 with
    | Some (TmPair(_, v2)) -> Some v2
    | Some _ | None -> None) 

  | TmInl(_, t1) -> 
    (match cbv t1 with
    | Some v -> Some (TmInl (TUnit, v))
    | None -> None)
  | TmInr(_, t1) ->
    (match cbv t1 with
    | Some v -> Some (TmInr (TUnit, v))
    | None -> None) 
  | TmCase(t1, x1, t2, x2, t3) -> 
    (match cbv t1 with
    | Some (TmInl (_, v1)) -> Some (tm_let x1 TUnit v1 t2)
    | Some (TmInr (_, v1)) -> Some (tm_let x2 TUnit v1 t3)
    | Some _ | None -> None)

  | TmCons(tp, t1, t2) ->
    (match (is_val t1, is_val t2) with
    | (false, _) ->
      (match cbv t1 with
      | Some v1 -> Some (TmCons(tp, v1, t2))
      | _ -> None)
    | (true, false) ->
      (match cbv t2 with
      | Some v2 -> Some (TmCons(tp, t1, v2))
      | _ -> None)
    | (true, true) -> None)
  | TmIsNil(_, t1) -> 
    (match cbv t1 with
    | Some (TmNil _) -> Some TmTrue
    | Some _ -> Some TmFalse
    | None -> None)
  | TmHead(_, t1) ->
    (match cbv t1 with
    | Some (TmCons(_, v1, _)) -> Some v1
    | Some _ | None -> None)
  | TmTail(_, t1) ->
    (match cbv t1 with
    | Some (TmCons(_, _, v1)) -> Some v1
    | Some _ | None -> None)  

  | TmFix(t1) ->
    (match cbv t1 with
    | Some (TmAbs(x, _, t)) -> Some (tm_letrec x TUnit t t)
    | Some _ | None -> None)


let rec multistep (t : term) : term = 
  let rec loop current_term =
    match cbv current_term with
    | Some next_term when next_term <> current_term -> loop next_term
    | Some same_term -> same_term
    | None -> current_term  (* Return the last valid term if no further reduction is possible *)
  in
  loop t


(* Typechecking utilities *)

(* These first few functions can be copied from prior homeworks.
   We will try to give solutions shortly after the late deadline. *)

(* give a reasonable type to context *)
type ctx = (string * typ) list 

(* define the empty context *)
let empty_ctx : ctx = []

(* look up a given variable's typ, throw a NotFound exception if the variable is not found *)
let lookup (g : ctx) (x : string) : typ = 
  try List.assoc x g
  with Not_found -> raise (NotFound x)

(* check if a variable is already in the context *)

(* extend a context with a new variable-typ association *)
let extend (g : ctx) (x : string) (t : typ): ctx =
  let new_ctx = List.filter(fun(var, _) -> var <> x) g 
  in (x, t) :: new_ctx


(* Typechecking *)

(* This time, we'll explicitly make type_check and type_infer mutually
   recursive, so you can call one from the other. *)

(* return the type of a term in the given context *)
(* return None if the term is ill-typed *)
let rec type_infer (g : ctx) (t : term) : typ option = 
  match t with
  | TmVar x -> Some (lookup g x)
  | TmAbs (x, tp, t1) -> 
      let ctx' = extend g x tp in
      (match type_infer ctx' t1 with
       | Some tp1 -> Some (TFun (tp, tp1))
       | None -> None)
  | TmApp (t1, t2) ->
      (match (type_infer g t1, type_infer g t2) with
       | (Some (TFun (tp_arg, tp_res)), Some tp2) when tp_arg = tp2 -> Some tp_res
       | _ -> None)
  | TmTrue | TmFalse -> Some TBool
  | TmIf (t1, t2, t3) ->
      (match (type_infer g t1, type_infer g t2, type_infer g t3) with
       | (Some TBool, Some tp2, Some tp3) when tp2 = tp3 -> Some tp2
       | _ -> None)
  | TmZero -> Some TNat
  | TmSucc t1 | TmPred t1 -> 
      (match type_infer g t1 with
       | Some TNat -> Some TNat
       | _ -> None)
  | TmIsZero t1 ->
      (match type_infer g t1 with
       | Some TNat -> Some TBool
       | _ -> None)
  | TmUnit -> Some TUnit
  | TmPair (t1, t2) ->
      (match (type_infer g t1, type_infer g t2) with
       | (Some tp1, Some tp2) -> Some (TProd (tp1, tp2))
       | _ -> None)
  | TmFst t1 ->
      (match type_infer g t1 with
       | Some (TProd (tp1, _)) -> Some tp1
       | _ -> None)
  | TmSnd t1 ->
      (match type_infer g t1 with
       | Some (TProd (_, tp2)) -> Some tp2
       | _ -> None)
  | TmInl (tp, t1) -> 
      (match type_infer g t1 with
       | Some tp1 when tp1 = tp -> Some (TSum (tp, tp))  (* Simplified, assumes both sum types are the same *)
       | _ -> None)
  | TmInr (tp, t1) -> 
      (match type_infer g t1 with
       | Some tp1 when tp1 = tp -> Some (TSum (tp, tp))  (* Simplified, assumes both sum types are the same *)
       | _ -> None)
  | TmCase (t1, x, t2, y, t3) ->
      (match type_infer g t1 with
       | Some (TSum (tp1, tp2)) ->
           let ctx1 = extend g x tp1 in
           let ctx2 = extend g y tp2 in
           (match (type_infer ctx1 t2, type_infer ctx2 t3) with
            | (Some tp12, Some tp32) when tp12 = tp32 -> Some tp12
            | _ -> None)
       | _ -> None)
  | TmNil tp -> Some (TList tp)
  | TmCons (tp, t1, t2) ->
      (match (type_infer g t1, type_infer g t2) with
       | (Some tp1, Some (TList tp2)) when tp = tp1 && tp1 = tp2 -> Some (TList tp)
       | _ -> None)
  | TmIsNil (tp, t1) ->
      (match type_infer g t1 with
       | Some (TList tp1) when tp1 = tp -> Some TBool
       | _ -> None)
  | TmHead (tp, t1) ->
      (match type_infer g t1 with
       | Some (TList tp1) when tp1 = tp -> Some tp
       | _ -> None)
  | TmTail (tp, t1) ->
      (match type_infer g t1 with
       | Some (TList tp1) when tp1 = tp -> Some (TList tp)
       | _ -> None)
  | TmFix t1 ->
      (match type_infer g t1 with
       | Some (TFun (tp1, tp2)) when tp1 = tp2 -> Some tp1
       | _ -> None)

(* return true if t has the given type in the context, else return false *)
and type_check (g : ctx) (t : term) (tp : typ) : bool =
  match type_infer g t with
  | Some tp' -> tp = tp'
  | None -> false
