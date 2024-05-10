module SS = Set.Make (String)
module SMap = Map.Make (String)
module IMap = Map.Make (Int)

open Etlc

exception CustomException of typ

(* Utility functions from hw4 *)

let rec fv (t: term) : SS.t = 
  match t with
  | TmVar x -> SS.singleton x

  | TmSucc t1 | TmPred t1 | TmIsZero t1 
  | TmVariant (_, t1, _) | TmRef t1 | TmBang t1 
  | TmRaise t1 | TmIsNull (_, t1) -> fv t1

  | TmAbs (x, _, t1) -> SS.remove x (fv t1)
  | TmApp (t1, t2) | TmAssn (t1, t2) | TmTry (t1, t2) -> SS.union (fv t1) (fv t2)
  | TmIf (x, y, z) -> SS.union (SS.union (fv x) (fv y)) (fv z)

  | TmCase (t1, cs) ->
    SS.union (fv t1) (List.fold_left (fun acc (_, x, t') -> 
      SS.diff acc (SS.singleton x) |> SS.union (fv t'))
      SS.empty cs)

  | _ -> SS.empty  (* Ensure all term types are covered; this is a placeholder *)

let fresh_var (vars : SS.t) =
  match SS.max_elt_opt vars with Some var -> var ^ "0" | None -> "x0"

let exn' (l: label) (t: term) : term =
  TmVariant(l, t, tp_exn)

(* Small-step evaluation *)

(* Implement the small-step evaluation relations from class. 
   Note the presence of a store and the possibility of encountering 
   raise and null. *)

type store = term IMap.t

let rec is_num_val = function
  | TmZero -> true
  | TmSucc v when is_num_val v -> true
  | _ -> false

let is_val (t: term) : bool = 
  if is_num_val t then true else
  (match t with
  | TmTrue | TmFalse | TmUnit | TmAbs _ | TmNull -> true
  | _ -> false)

let rec subst (x : string) (s : term) (t : term) : term =
  (match t with
  | TmVar v -> if v = x then s else TmVar v
  | TmApp (t1, t2) -> TmApp (subst x s t1, subst x s t2)
  | TmAbs (v, ty, t1) ->
      if x = v then TmAbs (v, ty, t1)
      else if (SS.mem v (fv s)) then 
        let new_v = fresh_var (SS.union (fv t1) (fv s)) in
        let t1' = subst v (TmVar new_v) t1 in
        TmAbs (new_v, ty, subst x s t1')
      else
        TmAbs (v, ty, subst x s t1)

  | TmIf (t1, t2, t3) -> TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmSucc (t1) -> TmSucc (subst x s t1)
  | TmPred (t1) -> TmPred (subst x s t1)
  | TmIsZero (t1) -> TmIsZero (subst x s t1)

  | TmVariant (l, t1, ty) -> TmVariant (l, subst x s t1, ty)
  | TmCase (t0, cs) ->
    let t1' = subst x s t0 in
    let cs' =
      List.map (fun (l, v, t') -> 
        let t'' = if x = v then t' else subst x s t' in
        (l, v, t''))
      cs in
    TmCase (t1', cs')
  
  | TmRef t1 -> TmRef (subst x s t1)
  | TmBang t1 -> TmBang (subst x s t1)
  | TmAssn (t1, t2) -> TmAssn (subst x s t1, subst x s t2)
  | TmRaise t1 -> TmRaise (subst x s t1)
  | TmTry (t1, t2) -> TmTry (subst x s t1, subst x s t2)
  | TmIsNull (ty, t1) -> TmIsNull (ty, subst x s t1)

  | _ -> t)

(* let fresh_loc vars = 
  (let rec helper (n: int) =
    if IMap.mem n vars then helper (n + 1) else n
  in helper 0) *)

let fresh_loc (mu: store) =
  match IMap.max_binding_opt mu with
  | Some (i, _) -> i + 1
  | None -> 0

let rec case_helper (t: term) (cs: (label * string * term) list) : term =
  (match t with
  | TmVariant (l, t1, _) ->
    begin match cs with
    | (l', s', t') :: cs' -> if l = l' then (subst s' t1 t')
    else case_helper t cs'
    | [] -> t
  end
  | _ -> t)
  
let rec cbv (t : term) (mu : store) : (term*store) option =
  (match t with
  | TmApp (TmRaise v1, _) -> Some (TmRaise v1, mu)
  | TmApp (_, TmRaise v2) -> Some (TmRaise v2, mu)
  | TmApp (TmNull, _) | TmApp (_, TmNull) -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmApp (t1, t2) ->
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmApp (t1', t2), mu')
    | None ->
      (match cbv t2 mu with
      | Some (t2', mu') -> Some (TmApp (t1, t2'), mu')
      | None ->
        (match t1 with
        | TmAbs (x, _, t1') -> Some (subst x t2 t1', mu)
        | _ -> None)))
    
  | TmAbs _ -> None
  | TmVar _ -> None
  | TmTrue | TmFalse | TmZero -> None

  | TmIf (TmRaise v, _, _) -> Some (TmRaise v, mu)
  | TmIf (_, TmRaise v, _) -> Some (TmRaise v, mu)
  | TmIf (_, _, TmRaise v) -> Some (TmRaise v, mu)
  | TmIf (TmNull, _, _) -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmIf (TmTrue, t2, _) -> Some (t2, mu)
  | TmIf (TmFalse, _, t3) -> Some (t3, mu)
  | TmIf (t1, t2, t3) ->
    (match cbv t1 mu with
    | Some (t1', mu) -> Some (TmIf (t1', t2, t3), mu)
    | None -> None)

  | TmSucc TmNull | TmPred TmNull | TmIsZero TmNull -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmSucc TmRaise v | TmPred TmRaise v | TmIsZero TmRaise v
    when is_val v -> Some (TmRaise v, mu)

  | TmSucc t1 -> 
    (match cbv t1 mu with
    | Some (t1', mu) -> Some (TmSucc t1', mu)
    | _ -> None)

  | TmPred TmZero -> Some (TmZero, mu)
  | TmPred (TmSucc t0) when is_num_val t0 -> Some (t0, mu)
  | TmPred t1 ->
    (match cbv t1 mu with
    | None -> None | Some (t1', mu') -> Some (TmPred t1', mu'))

  | TmUnit -> None

  | TmIsZero TmZero -> Some (TmTrue, mu)
  | TmIsZero (TmSucc t0) when is_num_val t0 -> Some (TmFalse, mu)
  | TmIsZero t0 ->
    (match cbv t0 mu with
    | None -> None | Some (t0', mu') -> Some (TmIsZero t0', mu'))

  | TmCase (TmVariant (lb, t0, ty), cs) ->
    (if is_val t0 then Some ((case_helper (TmVariant (lb, t0, ty)) cs), mu)
    else None)
  | TmCase (t0, cs) -> 
    (match cbv t0 mu with
    | Some (t0', mu') -> Some (TmCase (t0', cs), mu')
    | None -> None)
  | TmVariant (lb, t1, ty) -> 
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmVariant (lb, t1', ty), mu')
    | None -> None)

  | TmRef (TmRaise v) when is_val v -> Some (TmRaise v, mu)
  | TmRef v1 when is_val v1 -> 
    let i = (fresh_loc mu) in Some (TmLoc i, IMap.add i v1 mu)
  | TmRef t1 -> 
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmRef t1', mu')
    | None -> None)

  | TmLoc _ -> None

  | TmBang (TmRaise v) when is_val v -> Some (TmRaise v, mu)
  | TmBang TmNull -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmBang (TmLoc l) -> 
    (match IMap.find_opt l mu with
    | Some t1 -> Some (t1, mu)
    | None -> Some (TmRaise (exn' "not_found" TmUnit), mu))
  | TmBang t1 ->
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmBang t1', mu')
    | None -> None)

  | TmAssn (TmRaise v1, _) when is_val v1 -> Some (TmRaise v1, mu)
  | TmAssn (TmLoc _, TmRaise v2) when is_val v2 -> Some (TmRaise v2, mu)
  | TmAssn (TmNull, _) -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmAssn (TmLoc l, v2) when is_val v2 -> 
    (match IMap.find_opt l mu with 
    | Some _ -> Some (TmUnit, IMap.add l v2 mu)
    | None -> Some (TmRaise (exn' "not_found" TmUnit), mu))
  | TmAssn (t1, t2) -> 
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmAssn (t1', t2), mu')
    | None ->
      (match cbv t2 mu with
      | Some (t2', mu') -> Some (TmAssn (t1, t2'), mu')
      | None -> None))

  | TmRaise TmNull -> Some (TmRaise (exn' "null_pointer" TmUnit), mu)
  | TmRaise (TmRaise v) when is_val v -> Some (TmRaise v, mu)
  | TmRaise t1 ->
    (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmRaise t1', mu')
    | None -> None)

  | TmTry (v1, _) when is_val v1 -> Some (v1, mu)
  | TmTry (TmRaise v', t2) when is_val v' -> Some (TmApp (t2, v'), mu)
  | TmTry (t1, t2) -> 
   (match cbv t1 mu with
    | Some (t1', mu') -> Some (TmTry (t1', t2), mu')
    | None -> None)

  | TmNull -> None

  | TmIsNull (_, TmRaise v11) when is_val v11 -> Some (TmFalse, mu)
  | TmIsNull (_, TmNull) -> Some (TmTrue, mu)
  | TmIsNull (_, TmLoc _) -> Some (TmFalse, mu)
  | TmIsNull (tp, t1) ->
    (match cbv t1 mu with
    | Some (t1', mu) -> Some (TmIsNull (tp, t1'), mu)
    | None -> Some (TmFalse, mu)))

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
  | TmRaise _ -> true  (* `raise v` type checks with any type *)
  | TmNull -> true     (* `null` type checks with any type *)
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
    | Some typ -> typ = tp 
    | None -> false)
  | TmBang trm -> (match type_infer g s trm with
    | Some (TRef tp_ref) -> (tp_ref = tp)
    | _ -> false)
  | TmAssn (tl, tr) -> (match type_infer g s tl, type_infer g s tr with
    | Some (TRef tp1), (Some tp2) when (tp1 = tp2) -> (tp = TUnit) 
    | _ -> false)
  | TmIsNull (_, trm) -> (match type_infer g s trm with
    | Some TBot -> tp = TBool 
    | _ -> false)
  | TmTry (trm, handler) -> (match type_infer g s trm, type_infer g s handler with
    | Some _, Some TBot -> true 
    | _ -> false)

  | TmVariant (_, trm, _) -> (match type_infer g s trm with
    | Some _ -> true 
    | _ -> false)
  | TmCase (trm, _) -> (match type_infer g s trm with
    | Some (TVariant _) -> true 
    | _ -> false)

(* Checks if the given store is well typed with respect to the given type_store
   and typing context. *)
(* Returns true iff gamma | sigma |- mu *)
let store_well_typed (g : ctx) (s : typ_store) (mu : store) : bool =
  IMap.for_all (fun k v ->
    begin match (IMap.find_opt k s) with
    | Some ty ->
      begin match (type_infer g s v) with
      | Some ty2 -> ty = ty2
      | None -> false
      end
    | None -> false
    end) mu

(*   
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
      false *)
