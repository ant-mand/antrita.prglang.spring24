module SS = Set.Make(String)
module SMap = Map.Make(String)
module IMap = Map.Make(Int)

open List
open Char
open Format

exception Not_implemented
exception Parse_exn
exception NotAbs of string
exception NotFound of string
exception DuplicateVar of string

(* Data Definitions *)

type label = string

type typ 
= TUnit
| TBool
| TNat
| TFun of typ * typ
| TRef of typ
| TVariant of (label * typ) list 
| TBot

type term
= TmVar of string
| TmAbs of string * typ * term
| TmApp of term * term
| TmUnit
| TmTrue 
| TmFalse 
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term
| TmVariant of label * term * typ (* eg. red = 6 as [red:Nat; blue:Nat] *)
| TmCase of term * (label * string * term) list (* eg. case red 2 of [red x => 5 | blue y => 9] *)
| TmRef of term 
| TmLoc of int 
| TmBang of term (* !t *)
| TmAssn of term * term 
| TmRaise of term
| TmTry of term * term
| TmNull
| TmIsNull of typ * term

(* Utility functions from hw4 *)

let rec fv = function
  | TmTrue | TmFalse | TmUnit | TmZero -> SS.empty
  | TmIf (x, y, z) -> SS.union (SS.union (fv x) (fv y)) (fv z)
  | TmSucc t | TmPred t | TmIsZero t -> fv t
  | TmVar x -> SS.singleton x
  | TmAbs (v, _, body) -> SS.diff (fv body) (SS.singleton v)
  | TmCase (t0, branches) ->
      let fv_branches = List.fold_left (fun acc (l, v, t) ->
        SS.union acc (SS.diff (fv t) (SS.singleton v))
      ) SS.empty branches
      in SS.union (fv t0) fv_branches
  | _ -> SS.empty  (* Ensure all term types are covered; this is a placeholder *)

let fresh_var (vars : SS.t) =
  match SS.max_elt_opt vars with Some var -> var ^ "0" | None -> "x0"

(* Variants *)

(* Implement an option type, some and none as variants *)

let tp_opt (tp : typ) : typ = TVariant ([("Some", tp); ("None", TUnit)]) 

let tm_some (t : term) (tp : typ) : term = TmVariant ("Some", t, tp_opt tp)

let tm_none (tp : typ) : term = TmVariant ("None", TmUnit, tp_opt tp)

(* Implement an exception type as a variant. 
   There are at least three possible exceptions that you should be able to handle. 
   (These should become clear as you progress through the homework, but feel free to start
    with some obvious candidates. No points will be deducted for having extra exceptions.) *)

let tp_exn : typ = TVariant ([("Not found", TUnit); 
                              ("Duplicate Var", TUnit);
                              ("Parse Error", TUnit);
                              ("Not Abs", TUnit);
                              ("Capture Exception", TUnit);
                              ("Type Error", TUnit);
                              ("Store Error", TUnit);
                              ("Value Error", TUnit);
                              ("Type Store Error", TUnit);
                              ("Null Pointer", TUnit);
                              ("Divide By Zero", TUnit);
                              ("Overflow", TUnit);
                              ("Other", TUnit)])


(* Small-step evaluation *)

(* Implement the small-step evaluation relations from class. 
   Note the presence of a store and the possibility of encountering 
   raise and null. *)

type store = term IMap.t

let rec is_num_val = function
  | TmZero -> true
  | TmSucc v when is_num_val v -> true
  | _ -> false

and is_val = function
  | TmTrue | TmFalse | TmUnit | TmAbs (_, _, _) -> true
  | TmLoc _ -> true
  | v -> is_num_val v

let rec subst (x : string) (s : term) (t : term) : term =
  match t with
  | TmVar v ->
      if v = x then s
      else TmVar v

  | TmAbs (v, ty, body) ->
      if v = x then
        TmAbs (v, ty, body)  (* x is shadowed in the abstraction *)
      else if SS.mem v (fv s) then
        let new_v = fresh_var (SS.union (fv body) (fv s)) in
        let body' = subst v (TmVar new_v) body in
        TmAbs (new_v, ty, subst x s body')
      else
        TmAbs (v, ty, subst x s body)

  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)

  | TmCase (t0, branches) ->
      TmCase (subst x s t0,
              branches |> List.map (fun (l, v, t) ->
                if v = x then
                  (l, v, t)  (* x is shadowed in this case arm *)
                else if SS.mem v (fv s) then
                  let new_v = fresh_var (SS.union (fv t) (fv s)) in
                  let t' = subst v (TmVar new_v) t in
                  (l, new_v, subst x s t')
                else
                  (l, v, subst x s t)
              ))
  | _ -> t 

let rec cbv (t : term) (mu : store) : (term*store) option =
  let update_store (mu : store) (loc : int) (v : term) : store = IMap.add loc v mu in
  let ( >> ) (t : term) (mu: store) (constructor : term -> store -> (term*store) option) : (term*store) option =
  match cbv t mu with 
  | None -> None 
  | Some (t', mu') -> constructor t' mu'
  in

  match t, mu with
  | TmApp (TmAbs (x, _, body), t0), mu when is_val t0 -> Some (subst x t0 body, mu)
  | TmApp ((TmAbs (_, _, _) as func), input), mu -> (
    match cbv input mu with
    | None -> None
    | Some (input', mu') -> Some (TmApp (func, input'), mu'))
  | TmApp (func, input), mu -> (
    match cbv func mu with
    | None -> None
    | Some (func', mu') -> Some (TmApp (func', input), mu'))

  | TmIf (TmTrue, t1, _), mu -> Some (t1, mu)
  | TmIf (TmFalse, _, t2), mu -> Some (t2, mu)
  | TmIf (t0, t1, t2), mu -> (t0 >> mu >> fun t0' mu') -> Some (TmIf (t0', t1, t2), mu')

  | TmSucc t0, mu -> (t0 >> mu >> fun t0' mu') -> Some (TmSucc t0', mu')

  | TmPred TmZero, mu -> Some (TmZero, mu)
  | TmPred (TmSucc t0), mu when is_num_val t0 -> Some (t0, mu)
  | TmPred t0, mu -> (t0 >> mu >> fun t0' mu') -> Some (TmPred t0', mu')

  | TmIsZero TmZero, mu -> Some (TmTrue, mu)
  | TmIsZero (TmSucc t0), mu when is_num_val t0 -> Some (TmFalse, mu)
  | TmIsZero t0, mu -> (t0 >> mu >> fun t0' mu') -> Some (TmIsZero t0', mu')

  | TmVariant (label, t0, _), mu -> (t0 >> mu >> fun t0' mu') -> Some (TmVariant (label, t0', TUnit), mu')
  | TmCase (TmVariant (label, t0, _), cases), mu -> (
    match find_opt (fun (label', _, _) -> label = label') cases with
    | None -> None
    | Some (_, x, body) -> Some (subst x t0 body, mu))
  | TmCase (t0, cases), mu -> (t0 >> mu >> fun t0' mu') -> Some (TmCase (t0', cases), mu')

  | TmBang (TmRef t0), mu -> (t0 >> mu >> fun t0' mu') -> Some (TmLoc (IMap.cardinal mu'), update_store mu' (IMap.cardinal mu') t0')
  | TmBang (TmLoc loc), mu -> (
    match IMap.find_opt loc mu with
    | None -> None
    | Some v -> Some (v, mu))
  | TmBang (TmNull), mu -> raise tp_exn

  | TmAssn (t0, t1), mu -> (t0 >> mu >> fun t0' mu') -> Some (TmAssn (t0', t1), mu')
  | TmAssn (t0, t1), mu -> (t1 >> mu >> fun t1' mu') -> Some (TmAssn (t0, t1'), mu')
  | TmAssn (TmLoc loc, t1), mu -> (
    match IMap.find_opt loc mu with
    | None -> None
    | Some _ -> (t1 >> mu >> fun t1' mu') -> Some (TmUnit, update_store mu' loc t1'))

  | TmRef t0, mu when is_val t0 -> let l = IMap.cardinal mu in Some (TmLoc l, update_store mu l t0)
  | TmRef t0, mu -> (t0 >> mu >> fun t0' mu') -> Some (TmRef t0', mu')

  | TmRaise t0, mu -> (t0 >> mu >> fun t0' mu') -> Some (TmRaise t0', mu')
  | TmRaise (TmRaise t0), mu -> Some (TmRaise t0, mu)
  | TmApp (TmRaise v11, t2), mu when is_val v11 && not is_val t2 -> Some (TmRaise v11, mu)
  | TmApp (v11, TmRaise v21), mu when is_val v11 && is_val v21 -> Some (TmRaise v21, mu)

  | TmTry (v1, t2), mu when is_val v1 -> Some (v1, mu)
  | TmTry (TmRaise v11, t2), mu -> Some (TmApp (t2, v11), mu)
  | TmTry (t1, t2), mu -> (t1 >> mu >> fun t1' mu') -> Some (TmTry (t1', t2), mu')

  | TmIsNull (TUnit, TmNull), mu -> Some (TmTrue, mu)
  | _ -> None

let rec multistep (t : term) (mu : store) : term*store = 
  match cbv t mu with 
  | None -> (t, mu) 
  | Some (t', mu') -> multistep t' mu'

(* Typechecking utilities *)

type ctx = typ SMap.t
type typ_store = typ IMap.t 

let empty_ctx : ctx = SMap.empty

let empty_store : typ_store = IMap.empty

(* look up a given variable's typ, throw a NotFound exception if the variable is not found *)
let lookup (g : ctx) (x : string) : typ = match SMap.find_opt x g with
| Some t -> t
| None -> raise (NotFound x)

(* extend a context with a new variable-typ association *)
let extend (g : ctx) (x : string) (t : typ): ctx = 
  if SMap.mem x g then raise (DuplicateVar x)
  else SMap.add x t g


(* Typechecking *)

(* check if a term has the given type. *)
(* Takes in a context and a store, as well as a term t and type T *)
(* Returns true iff gamma | sigma |- t : T *)
let type_check (g : ctx) (s : typ_store) (t : term) (tp : typ) = raise Not_implemented

(* This should infer the type of a term in the given context *)
(* return None if the term is ill-typed *)
and type_infer (g : ctx) (s : typ_store) (t : term) : typ option = raise Not_implemented

(* Checks if the given store is well typed with respect to the given type_store
   and typing context. *)
(* Returns true iff gamma | sigma |- mu *)
let store_well_typed (g : ctx) (s : typ_store) (mu : store) = raise Not_implemented 

