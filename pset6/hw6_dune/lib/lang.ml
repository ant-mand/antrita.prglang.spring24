module SS = Set.Make (String)
module SMap = Map.Make (String)
module IMap = Map.Make (Int)

open Etlc

exception CustomException of typ

(* Utility functions from hw4 *)

let rec fv = function
  | TmTrue | TmFalse | TmUnit | TmZero -> SS.empty
  | TmIf (x, y, z) -> SS.union (SS.union (fv x) (fv y)) (fv z)
  | TmSucc t | TmPred t | TmIsZero t -> fv t
  | TmVar x -> SS.singleton x
  | TmAbs (v, _, body) -> SS.diff (fv body) (SS.singleton v)
  | TmCase (t0, branches) ->
      let fv_branches = List.fold_left (fun acc (_, v, t) ->
        SS.union acc (SS.diff (fv t) (SS.singleton v))
      ) SS.empty branches
      in SS.union (fv t0) fv_branches
  | _ -> SS.empty  (* Ensure all term types are covered; this is a placeholder *)

let fresh_var (vars : SS.t) =
  match SS.max_elt_opt vars with Some var -> var ^ "0" | None -> "x0"

let raise_tp_exn label =
  match label with
  | "not_found" -> raise (CustomException (TVariant [("not_found", TUnit)]))
  | "duplicate_var" -> raise (CustomException (TVariant [("duplicate_var", TUnit)]))
  | "parse_error" -> raise (CustomException (TVariant [("parse_error", TUnit)]))
  | "not_abs" -> raise (CustomException (TVariant [("not_abs", TUnit)]))
  | "capture_exception" -> raise (CustomException (TVariant [("capture_exception", TUnit)]))
  | "type_error" -> raise (CustomException (TVariant [("type_error", TUnit)]))
  | "store_error" -> raise (CustomException (TVariant [("store_error", TUnit)]))
  | "value_error" -> raise (CustomException (TVariant [("value_error", TUnit)]))
  | "type_store_error" -> raise (CustomException (TVariant [("type_store_error", TUnit)]))
  | "null_pointer" -> raise (CustomException (TVariant [("null_pointer", TUnit)]))
  | "divide_by_zero" -> raise (CustomException (TVariant [("divide_by_zero", TUnit)]))
  | "overflow" -> raise (CustomException (TVariant [("overflow", TUnit)]))
  | _ -> raise (CustomException (TVariant [("other", TUnit)]))

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

(* 
let rec subst (x : string) (s : term) (t : term) : term =
  match t with
  | TmVar y when x = y -> s
  | TmVar _ -> t
  | TmAbs (v, tp1, t1) ->
    if x = v then TmAbs (v, tp1, t1)
    else if SS.mem v (fv s) then
      let fresh = fresh_var (SS.union (fv s) (fv t1)) in
      TmAbs (fresh, tp1, subst x s t1) 
    else
      TmAbs (v, tp1, subst x s t1)
  | TmApp (t1, t2) -> TmApp (subst x s t1, subst x s t2)
  | TmIf (t1, t2, t3) -> TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmSucc t1 -> TmSucc (subst x s t1)
  | TmPred t1 -> TmPred (subst x s t1)
  | TmIsZero t1 -> TmIsZero (subst x s t1)
  | TmVariant (lbl, v, tp) -> TmVariant (lbl, subst x s v, tp)
  | TmCase (t1, branches) ->
      let branches' = List.map (fun (l, x, t) -> (l, x, subst x s t)) branches in
      TmCase (subst x s t1, branches')
  | TmRef t -> TmRef (subst x s t)
  | TmBang t -> TmBang (subst x s t)
  | TmAssn (t1, t2) -> TmAssn (subst x s t1, subst x s t2)
  | TmRaise t -> TmRaise (subst x s t)
  | TmTry (t1, t2) -> TmTry (subst x s t1, subst x s t2)
  | TmIsNull (tp, t) -> TmIsNull (tp, subst x s t)
  | _ -> t

let rec cbv (t : term) (mu : store) : (term * store) option =
  match t with
  | TmIf (TmTrue, t2, _) -> Some (t2, mu)
  | TmIf (TmFalse, _, t3) -> Some (t3, mu)
  | TmIf (t1, t2, t3) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmIf (t1', t2, t3), mu')
      | None -> None
      end
  | TmSucc t1 ->
      begin match cbv t1 mu with
      | Some (TmZero, mu') -> Some (TmSucc TmZero, mu')
      | Some (TmSucc nv, mu') -> Some (TmSucc (TmSucc nv), mu')
      | Some (t1', mu') -> Some (TmSucc t1', mu')
      | None -> None
      end
  | TmPred TmZero -> Some (TmZero, mu)
  | TmPred (TmSucc nv) -> Some (nv, mu)
  | TmPred t1 ->
      begin match cbv t1 mu with
      | Some (TmZero, mu') -> Some (TmZero, mu')
      | Some (TmSucc nv, mu') -> Some (nv, mu')
      | Some (t1', mu') -> Some (TmPred t1', mu')
      | None -> None
      end
  | TmIsZero TmZero -> Some (TmTrue, mu)
  | TmIsZero (TmSucc _) -> Some (TmFalse, mu)
  | TmIsZero t1 ->
      begin match cbv t1 mu with
      | Some (TmZero, mu') -> Some (TmTrue, mu')
      | Some (TmSucc _, mu') -> Some (TmFalse, mu')
      | Some (t1', mu') -> Some (TmIsZero t1', mu')
      | None -> None
      end
  | TmVariant (lbl, t1, _) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmVariant (lbl, t1', TBot), mu')
      | None -> None
      end
  | TmCase (TmVariant (lbl, v, _), branches) ->
      begin
        try
          let branches' = List.map (fun (l, x, t) -> (l, (x, t))) branches in
          let x, t = List.assoc lbl branches' in
          let t' = subst x v t in
          Some (t', mu)
        with Not_found -> None
      end
  | TmCase (t1, branches) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmCase (t1', branches), mu')
      | None -> None
      end
  | TmRef t1 ->
      begin match cbv t1 mu with
      | Some (t1', mu') ->
          let loc = IMap.cardinal mu' + 1 in
          let mu'' = IMap.add loc t1' mu' in
          Some (TmLoc loc, mu'')
      | None -> None
      end
  | TmBang (TmLoc loc) ->
      begin
        try
          let t' = IMap.find loc mu in
          Some (t', mu)
        with Not_found -> None
      end
  | TmBang t1 ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmBang t1', mu')
      | None -> None
      end
  | TmAssn (TmLoc loc, t1) ->
      begin
        try
          let mu' = IMap.add loc t1 mu in
          Some (TmUnit, mu')
        with Not_found -> None
      end
  | TmAssn (t1, t2) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmAssn (t1', t2), mu')
      | None -> None
      end
  | TmTry (TmRaise t1, t2) ->
      begin match cbv t2 mu with
      | Some (t2', mu') -> Some (t2', mu')
      | None -> None
      end
  | TmTry (t1, t2) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmTry (t1', t2), mu')
      | None -> None
      end
  | TmIsNull (TRef _, TmNull) -> Some (TmTrue, mu)
  | TmIsNull (_, TmNull) -> Some (TmFalse, mu)
  | TmIsNull (tp, t1) ->
      begin match cbv t1 mu with
      | Some (t1', mu') -> Some (TmIsNull (tp, t1'), mu')
      | None -> None
      end
  | TmRaise t1 ->
      begin match cbv t1 mu with
      | Some (TmTry (t', t2), mu') -> cbv t2 mu'
      | _ -> None
      end
  | _ -> None


let rec multistep (t : term) (mu : store) : term * store =
  match cbv t mu with
  | Some (new_t, new_mu) -> multistep new_t new_mu
  | None -> (t, mu)
  | Some (TmRaise _, mu') ->
      begin match t with
      | TmTry (_, t2) -> multistep t2 mu'
      | _ -> (t, mu')  (* If we encounter a TmRaise outside a try-with block, we return the original term and store *)
      end *)


let rec subst (x : string) (s : term) (t : term) : term =
  match t with
  | TmVar v ->
      if v = x then s
      else TmVar v

  | TmAbs (v, ty, body) ->
      if v = x then
        TmAbs (v, ty, body)
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
                  (l, v, t)
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
  let ( >> ) ((t, mu) : (term * store)) (constructor : term -> store -> (term*store) option) : (term*store) option =
  match cbv t mu with 
  | None -> None 
  | Some (t', mu') -> constructor t' mu'
  in

  (match t, mu with
  | TmApp (TmRaise v11, t2), mu when is_val v11 && not (is_val t2) -> Some (TmRaise v11, mu)
  | TmApp (v11, TmRaise v21), mu when is_val v11 && is_val v21 -> Some (TmRaise v21, mu)
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
  | TmIf (t0, t1, t2), mu -> (t0, mu) >> fun t0' mu' -> Some (TmIf (t0', t1, t2), mu')

  | TmSucc t0, mu -> (t0, mu) >> fun t0' mu' -> Some (TmSucc t0', mu')

  | TmPred TmZero, mu -> Some (TmZero, mu)
  | TmPred (TmSucc t0), mu when is_num_val t0 -> Some (t0, mu)
  | TmPred t0, mu -> (t0, mu) >> fun t0' mu' -> Some (TmPred t0', mu')

  | TmIsZero TmZero, mu -> Some (TmTrue, mu)
  | TmIsZero (TmSucc t0), mu when is_num_val t0 -> Some (TmFalse, mu)
  | TmIsZero t0, mu -> (t0, mu) >> fun t0' mu' -> Some (TmIsZero t0', mu')

  | TmVariant (label, t0, _), mu -> (t0, mu) >> fun t0' mu' -> Some (TmVariant (label, t0', TBot), mu')
  | TmCase (TmVariant (label, t0, _), cases), mu -> (
    match List.find_opt (fun (label', _, _) -> label = label') cases with
    | None -> None
    | Some (_, x, body) -> Some (subst x t0 body, mu))
  | TmCase (t0, cases), mu -> (t0, mu) >> fun t0' mu' -> Some (TmCase (t0', cases), mu')

  | TmBang (TmRef t0), mu -> (t0, mu) >> fun t0' mu' -> Some (TmLoc (IMap.cardinal mu'), update_store mu' (IMap.cardinal mu') t0')
  | TmBang (TmLoc loc), mu -> (
    match IMap.find_opt loc mu with
    | None -> None
    | Some v -> Some (v, mu))
  | TmBang (TmNull), _ -> raise_tp_exn "null_pointer"

  | TmAssn (TmLoc loc, t1), mu -> (
    match IMap.find_opt loc mu with
    | None -> None
    | Some _ -> (t1, mu) >> fun t1' mu' -> Some (TmUnit, update_store mu' loc t1'))
  | TmAssn (t0, t1), mu -> (t0, mu) >> fun t0' mu' -> Some (TmAssn (t0', t1), mu')
  (* | TmAssn (t0, t1), mu -> (t1, mu) >> fun t1' mu' -> Some (TmAssn (t0, t1'), mu') *)

  | TmRef t0, mu when is_val t0 -> let l = IMap.cardinal mu in Some (TmLoc l, update_store mu l t0)
  | TmRef t0, mu -> (t0, mu) >> fun t0' mu' -> Some (TmRef t0', mu')

  | TmRaise (TmRaise t0), mu -> Some (TmRaise t0, mu)
  | TmRaise t0, mu -> (t0, mu) >> fun t0' mu' -> Some (TmRaise t0', mu')

  | TmTry (v1, _), mu when is_val v1 -> Some (v1, mu)
  | TmTry (TmRaise v11, t2), mu -> Some (TmApp (t2, v11), mu)
  | TmTry (t1, t2), mu -> (t1, mu) >> fun t1' mu' -> Some (TmTry (t1', t2), mu')

  | TmIsNull (TRef _, TmNull), mu -> Some (TmTrue, mu)
  | TmIsNull (_, TmNull), mu -> Some (TmFalse, mu)
  | TmIsNull (tp, t1), mu ->
    begin match cbv t1 mu with
    | Some (t1', mu') -> Some (TmIsNull (tp, t1'), mu')
    | None -> None
    end

  | _ -> None)

  
let rec multistep (t : term) (mu : store) : term*store = 
  try
    match cbv t mu with 
    | None -> (t, mu)
    | Some (t', mu') -> multistep t' mu'
  with
  | CustomException _ -> (TmVar "0", mu)


(* Typechecking utilities *)

type ctx = typ SMap.t
type typ_store = typ IMap.t

let empty_ctx : ctx = SMap.empty
let empty_store : typ_store = IMap.empty

(* look up a given variable's typ, throw a NotFound exception if the variable is not found *)
let lookup (g : ctx) (x : string) : typ =
  match SMap.find_opt x g with Some t -> t | None -> raise (NotFound x)

(* extend a context with a new variable-typ association *)
let extend (g : ctx) (x : string) (t : typ) : ctx =
  if SMap.mem x g then raise (DuplicateVar x) else SMap.add x t g

(* Typechecking *)

(* This should infer the type of a term in the given context *)
(* return None if the term is ill-typed OR the type cannot be inferred *)
(* You may want to use type_check or other helper functions in writing this. *)

let rec type_infer (g : ctx) (s : typ_store) (t : term) : typ option =
  match t with
  | TmVar v -> Some (lookup g v)
  | TmAbs (v, ty, tr) -> let new_ctx = extend g v ty in
    (match type_infer new_ctx s tr with
      | Some ty0 -> Some (TFun (ty, ty0))
      | None -> None)
  | TmApp (tl, tr) -> (match type_infer g s tl with
    | Some (TFun (ty_arg, ty_res)) -> (match type_infer g s tr with
      | Some ty_tr when ty_tr = ty_arg -> Some ty_res | _ -> None)
    | _ -> None)

  | TmTrue | TmFalse -> Some TBool
  | TmZero -> Some TNat
  | TmUnit -> Some TUnit

  | TmSucc trm | TmPred trm -> (match type_infer g s trm with Some TNat -> Some TNat | _ -> None)
  | TmIsZero trm -> (match type_infer g s trm with Some TNat -> Some TBool | _ -> None)

  | TmIf (t0, t1, t2) -> (match type_infer g s t0 with
    | Some TBool -> 
      (match type_infer g s t1, type_infer g s t2 with
      | Some ty1, Some ty2 when ty1 = ty2 -> Some ty1
      | _ -> None)
    | _ -> None)

  | TmVariant (lbl, trm, tp) -> (match type_infer g s trm with
    | Some tp' when tp = tp' -> Some (TVariant [(lbl, tp)])
    | _ -> None)

  | TmCase (trm, cases) -> (match type_infer g s trm with
    | Some (TVariant lst) ->
      let case_types = List.map (fun (lbl, x, case_trm) ->
        let type_for_label = List.assoc lbl lst in
        let new_ctx = extend g x type_for_label in
        type_infer new_ctx s case_trm
      ) cases in
      if List.for_all (fun ct -> ct = List.hd case_types) case_types 
        then List.hd case_types else None
    | _ -> None)

  | TmRef trm -> (match type_infer g s trm with
    | Some tp -> Some (TRef tp) | _ -> None)
  | TmLoc l -> (match IMap.find_opt l s with
    | Some tp -> Some (TRef tp) | None -> None)
  | TmNull -> Some TBot
  | TmBang trm -> (match type_infer g s trm with
    | Some (TRef tp) -> Some tp | _ -> None)

  | TmAssn (tl, tr) -> (match type_infer g s tl with
    | Some (TRef tp1) ->
      (match type_infer g s tr with
      | Some tp2 when tp1 = tp2 -> Some TUnit
      | _ -> None)
    | _ -> None)

  | TmIsNull (_, trm) -> (match type_infer g s trm with
    | Some TBot -> Some TBool | _ -> None)
  | TmRaise trm -> (match type_infer g s trm with
    | Some _ -> Some TBot | _ -> None)

  | TmTry (trm, handler) -> (match type_infer g s trm, type_infer g s handler with
    | Some tp, Some TBot -> Some tp | _ -> None)

(* check if a term has the given type. *)
(* Takes in a context and a store, as well as a term t and type T *)
(* Returns true iff gamma | sigma |- t : T *)

and type_check (g : ctx) (s : typ_store) (t : term) (tp : typ) : bool =
  match t with
  | TmVar v -> (match lookup g v with
    | exception NotFound _ -> false
    | typ -> typ = tp)

  | TmAbs (v, ty, tr) -> (match tp with
    | TFun (param_type, return_type) ->
        param_type = ty && type_check (extend g v ty) s tr return_type
    | _ -> false)

  | TmApp (tl, tr) -> (match type_infer g s tl with
    | Some (TFun (ty_arg, ty_res)) when ty_res = tp ->
      (match type_infer g s tr with
      | Some ty_tr when ty_tr = ty_arg -> true
      | _ -> false)
    | _ -> false)

  | TmTrue | TmFalse -> tp = TBool
  | TmZero -> tp = TNat
  | TmUnit -> tp = TUnit
  | TmSucc trm | TmPred trm -> tp = TNat && type_check g s trm TNat
  | TmIsZero trm -> tp = TBool && type_check g s trm TNat
  | TmIf (t0, t1, t2) -> type_check g s t0 TBool && type_check g s t1 tp && type_check g s t2 tp
  | TmRef trm -> (match tp with
    | TRef tp_ref -> type_check g s trm tp_ref | _ -> false)

  | TmLoc l -> (match IMap.find_opt l s with
    | Some typ -> typ = tp | None -> false)

  | TmBang trm -> (match type_infer g s trm with
    | Some (TRef tp_ref) -> tp_ref = tp | _ -> false)

  | TmAssn (tl, tr) -> (match type_infer g s tl, type_infer g s tr with
    | Some (TRef tp1), Some tp2 when tp1 = tp2 -> tp = TUnit | _ -> false)

  | TmIsNull (_, trm) ->
  (match type_infer g s trm with
    | Some TBot -> tp = TBool | _ -> false)

  | TmRaise _ -> tp = TBot
  | TmTry (trm, handler) ->
  (match type_infer g s trm, type_infer g s handler with
    | Some _, Some TBot -> true | _ -> false)

  | TmNull -> tp = TBot
  | TmVariant (_, trm, _) -> (match type_infer g s trm with
    | Some _ -> true | _ -> false)
  | TmCase (trm, _) -> (match type_infer g s trm with
    | Some (TVariant _) -> true | _ -> false)

(* 
and type_check (g : ctx) (s : typ_store) (t : term) (tp : typ) : bool =
  match type_infer g s t with
  | Some inferred_type -> (inferred_type = tp)
  | None -> false *)

(* Checks if the given store is well typed with respect to the given type_store
   and typing context. *)
(* Returns true iff gamma | sigma |- mu *)
let store_well_typed (g : ctx) (s : typ_store) (mu : store) : bool =
  try
    IMap.fold (fun loc term acc ->
      if not acc then raise (CustomException (TVariant [("store_error", TUnit)]))
      else
        match IMap.find_opt loc s with
        | Some expected_typ ->
            (match type_infer g s term with
              | Some inferred_typ when inferred_typ = expected_typ -> true
              | _ -> raise (CustomException (TVariant [("type_error", TUnit)])))
        | None -> raise (CustomException (TVariant [("type_store_error", TUnit)]))
    ) mu true
  with
  | CustomException exn -> 
      print_endline ("Error in store typing: " ^ (match exn with
        | TVariant [(label, _)] -> label
        | _ -> "unknown")); 
      false
