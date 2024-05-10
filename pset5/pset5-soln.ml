module SS = Set.Make (String)
module MS = Map.Make (String)

exception Not_implemented
exception Parse_exn
exception NotAbs of string
exception NotFound of string
exception DuplicateVar of string
exception CaptureException of string

type typ =
  | TUnit
  | TBool
  | TNat
  | TFun of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ
  | TList of typ

type term =
  | TmVar of string
  | TmAbs of string * typ * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmZero
  | TmIf of term * term * term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmUnit
  | TmPair of term * term
  | TmFst of term
  | TmSnd of term
  | TmInl of typ * term
  | TmInr of typ * term
  | TmCase of
      term
      * string
      * term
      * string
      * term (* case term1 of inl string1 => term2 | inr string2 => term3 *)
  | TmNil of typ
  | TmCons of typ * term * term
  | TmIsNil of typ * term
  | TmHead of typ * term
  | TmTail of typ * term
  | TmFix of term

(* Utility functions from hw4 *)

let rec fv = function
  | TmTrue | TmFalse | TmUnit | TmZero | TmNil _ -> SS.empty
  | TmIf (x, y, z) -> SS.union (SS.union (fv x) (fv y)) (fv z)
  | TmSucc t
  | TmPred t
  | TmIsZero t
  | TmFst t
  | TmSnd t
  | TmHead (_, t)
  | TmTail (_, t)
  | TmInl (_, t)
  | TmIsNil (_, t)
  | TmInr (_, t) -> fv t
  | TmPair (t0, t1) | TmApp (t0, t1) | TmCons (_, t0, t1) -> SS.union (fv t0) (fv t1)
  | TmCase (t0, s1, t1, s2, t2) ->
    SS.union
      (fv t0)
      (SS.union (SS.diff (fv t1) (SS.singleton s1)) (SS.diff (fv t2) (SS.singleton s2)))
  | TmVar x -> SS.singleton x
  | TmAbs (v, _, body) -> SS.diff (fv body) (SS.singleton v)
  | TmFix t -> fv t

let fresh_var (vars : SS.t) =
  match SS.max_elt_opt vars with Some var -> var ^ "0" | None -> "x0"

(* Derived forms *)

(* Implement the derived forms t;t, let x : T = t in t, option T
   and option case from the book and class. *)
(* In t;t, the first t should have type unit. *)
(* In let, note that x is annotated with a type (unlike the book).  *)
(* Ascription should follow the book *)
(* For option T use a sum type to encode an option type *)
(* option case should case on None and Some t, returning a term for each case *)
(* letrec should combine let and fix as in the book *)

let tm_seq (t1 : term) (t2 : term) : term =
  TmApp (TmAbs (fresh_var (fv t2), TUnit, t2), t1)

let tm_let (x : string) (tp : typ) (t1 : term) (t2 : term) : term =
  TmApp (TmAbs (x, tp, t2), t1)

let tm_ascribe (t : term) (ty : typ) : term =
  TmApp (TmAbs ("x", ty, TmVar "x"), t)

let tp_opt (tp : typ) : typ = TSum (TUnit, tp)
let tm_some (t : term) : term = TmInr (TSum (TUnit, TNat), t)
let tm_none (tp : typ) : term = TmInl (tp, TmUnit)

let tm_opt_case (t : term) (x : string) (t_some : term) (t_none : term) : term =
  TmCase (t, "_", t_none, x, t_some)

let tm_letrec (x : string) (tp : typ) (t1 : term) (t2 : term) : term =
  tm_let x tp (TmFix (TmAbs (x, tp, t1))) t2

(* Small-step evaluation *)

(* Implement the small-step evaluation relations from class.
   Note that we're only concerned with call-by-value for this homework.
   Feel free to reuse code from homework 3.
   (Implementing capture-avoiding substitution is encouraged, but not what we
   will be grading this on) *)

let ( <$> ) (f : term -> term) (t : term) : term =
  match t with
  | TmFalse -> TmFalse
  | TmTrue -> TmTrue
  | TmZero -> TmZero
  | TmUnit -> TmUnit
  | TmNil _ as t0 -> t0
  | TmVar _ as t0 -> t0
  | TmAbs (v, typ, t0) -> TmAbs (v, typ, f t0)
  | TmIf (t0, t1, t2) -> TmIf (f t0, f t1, f t2)
  | TmSucc t0 -> TmSucc (f t0)
  | TmPred t0 -> TmPred (f t0)
  | TmIsZero t0 -> TmIsZero (f t0)
  | TmApp (t0, t1) -> TmApp (f t0, f t1)
  | TmHead (tp, t0) -> TmHead (tp, f t0)
  | TmTail (tp, t0) -> TmTail (tp, f t0)
  | TmIsNil (tp, t0) -> TmIsNil (tp, f t0)
  | TmCons (tp, t0, t1) -> TmCons (tp, f t0, f t1)
  | TmPair (t0, t1) -> TmPair (f t0, f t1)
  | TmFst t0 -> TmFst (f t0)
  | TmSnd t0 -> TmSnd (f t0)
  | TmInl (tp, t0) -> TmInl (tp, f t0)
  | TmInr (tp, t0) -> TmInr (tp, f t0)
  | TmCase (t0, s1, t1, s2, t2) -> TmCase (f t0, s1, f t1, s2, f t2)
  | TmFix t -> TmFix (f t)

let rec rename (f : string) (t : string) (term : term) : term =
  match term with
  | TmVar v when v = f -> TmVar t
  | TmAbs (v, _, _) as t0 when v = f -> t0
  | TmAbs (v, _, _) as t0 when v = t ->
      t0 |> alpha_convert (SS.singleton t) |> rename f t
  | TmCase (t0, s1, t1, s2, t2) ->
      let s1', t1' =
        if s1 = t then
          let new_s = fresh_var (SS.singleton t) in
          (new_s, t1 |> rename t new_s |> rename f t)
        else (s1, t1)
      in
      let s2', t2' =
        if s2 = t then
          let new_s = fresh_var (SS.singleton t) in
          (new_s, t2 |> rename t new_s |> rename f t)
        else (s2, t2)
      in
      TmCase (rename f t t0, s1', t1', s2', t2')
  | t0 -> rename f t <$> t0

and alpha_convert (vars : SS.t) (t : term) : term =
  match t with
  | TmAbs (v, typ, body) ->
      let v' = fresh_var (SS.add v vars) in
      TmAbs (v', typ, rename v v' body)
  | TmCase (t0, s1, t1, s2, t2) ->
      let s1' = fresh_var (SS.add s1 vars) in
      let s2' = fresh_var (SS.add s2 vars) in
      TmCase (t0, s1', rename s1 s1' t1, s2', rename s2 s2' t2)
  | _ -> raise (NotAbs "error")

let rec subst (x : string) (s : term) (t : term) : term =
  let free_vars = fv s in
  match t with
  | TmVar v when v = x -> s
  | TmAbs (v, _, _) as t0 when v = x -> t0
  | TmAbs (v, _, _) as t0 when SS.mem v free_vars ->
      t0 |> alpha_convert SS.empty |> subst x s
  | TmCase (_, s1, _, s2, _) as case'
    when SS.mem s1 free_vars || SS.mem s2 free_vars ->
      case' |> alpha_convert SS.empty |> subst x s
  | TmCase (t0, s1, t1, s2, t2) ->
      TmCase
        ( subst x s t0,
          s1,
          (if s1 = x then Fun.id else subst x s)
            t1 (* for when x bounds are already captured by the case's var *),
          s2,
          (if s2 = x then Fun.id else subst x s) t2 )
  | _ -> subst x s <$> t

let rec is_num_val = function
  | TmZero -> true
  | TmSucc v when is_num_val v -> true
  | _ -> false

let rec is_list_val = function
  | TmNil _ -> true
  | TmCons (_, h, tail) -> is_val h && is_list_val tail
  | _ -> false

and is_val = function
  | TmTrue | TmFalse | TmUnit | TmAbs (_, _, _) -> true
  | TmPair (t0, t1) -> is_val t0 && is_val t1
  | TmInl (_, t0) | TmInr (_, t0) -> is_val t0
  | v -> is_num_val v || is_list_val v

let rec cbv (t : term) : term option =
  let ( >> ) (t : term) (constructor : term -> term) : term option =
    match cbv t with None -> None | Some t0' -> Some (constructor t0')
  in
  match t with
  | TmApp (TmAbs (x, _, body), t0) when is_val t0 -> Some (subst x t0 body)
  | TmApp ((TmAbs (_, _, _) as func), input) -> (
      match cbv input with
      | None -> None
      | Some input' -> Some (TmApp (func, input')))
  | TmApp (func, input) -> (
      match cbv func with
      | None -> None
      | Some func' -> Some (TmApp (func', input)))
  | TmIf (TmTrue, t1, _) -> Some t1
  | TmIf (TmFalse, _, t2) -> Some t2
  | TmIf (t0, t1, t2) -> t0 >> fun t0' -> TmIf (t0', t1, t2)
  | TmSucc t0 -> t0 >> fun t0' -> TmSucc t0'
  | TmPred TmZero -> Some TmZero
  | TmPred (TmSucc t0) when is_num_val t0 -> Some t0
  | TmPred t0 -> t0 >> fun t0' -> TmPred t0'
  | TmIsZero TmZero -> Some TmTrue
  | TmIsZero (TmSucc t0) when is_num_val t0 -> Some TmFalse
  | TmIsZero t0 -> t0 >> fun t0' -> TmIsZero t0'
  | TmPair (t0, t1) ->
      if is_val t0 then
        if is_val t1 then None else t1 >> fun t1' -> TmPair (t0, t1')
      else t0 >> fun t0' -> TmPair (t0', t1)
  | TmFst (TmPair (t0, _) as pair) when is_val pair -> Some t0
  | TmFst t0 -> t0 >> fun t0' -> TmFst t0'
  | TmSnd (TmPair (_, t1) as pair) when is_val pair -> Some t1
  | TmSnd t0 -> t0 >> fun t0' -> TmSnd t0'
  | TmInl (tp, t0) -> t0 >> fun t0' -> TmInl (tp, t0')
  | TmInr (tp, t0) -> t0 >> fun t0' -> TmInr (tp, t0')
  | TmCase (TmInl (_, s), s1, t1, _, _) when is_val s -> Some (subst s1 s t1)
  | TmCase (TmInr (_, s), _, _, s2, t2) when is_val s -> Some (subst s2 s t2)
  | TmCase (t0, s1, t1, s2, t2) -> t0 >> fun t0' -> TmCase (t0', s1, t1, s2, t2)
  | TmCons (_, _, _) as lst when is_list_val lst -> None
  | TmCons (tp, t0, t1) when is_val t0 -> t1 >> fun t1' -> TmCons (tp, t0, t1')
  | TmCons (tp, t0, t1) -> t0 >> fun t0' -> TmCons (tp, t0', t1)
  | TmIsNil (_, TmNil _) -> Some TmTrue
  | TmIsNil (_, plst) when is_list_val plst -> Some TmFalse
  | TmIsNil (tp, t0) -> t0 >> fun t0' -> TmIsNil (tp, t0')
  | TmHead (_, TmNil tp) -> Some (tm_none (tp_opt tp))
  | TmHead (_, (TmCons (_, h, _) as plst)) when is_list_val plst ->
      Some (tm_some h)
  | TmHead (tp, t0) -> t0 >> fun t0' -> TmHead (tp, t0')
  | TmTail (_, (TmNil _ as nil)) -> Some nil
  | TmTail (_, (TmCons (_, _, t) as plst)) when is_list_val plst -> Some t
  | TmTail (tp, t0) -> t0 >> fun t0' -> TmTail (tp, t0')
  | TmFix (TmAbs (x, _, t')) -> Some (subst x t t')
  | TmFix t0 -> t0 >> fun t0' -> TmFix t0'
  | _ -> None

let rec multistep (t : term) : term =
  match cbv t with None -> t | Some t' -> multistep t'

(* Typechecking utilities *)

(* These first few functions can be copied from prior homeworks.
   We will try to give solutions shortly after the late deadline. *)

(* give a reasonable type to context *)
type ctx = typ MS.t

(* define the empty context *)
let empty_ctx : ctx = MS.empty

(* look up a given variable's typ, throw a NotFound exception if the variable is not found *)
let lookup (g : ctx) (x : string) : typ =
  match MS.find_opt x g with Some t -> t | None -> raise (NotFound x)

(* extend a context with a new variable-typ association *)
let extend (g : ctx) (x : string) (t : typ) : ctx = MS.add x t g

(* Typechecking *)

(* This time, we'll explicitly make type_check and type_infer mutually
   recursive, so you can call one from the other. *)

(* return the type of a term in the given context *)
(* return None if the term is ill-typed *)
let rec type_infer (g : ctx) (t : term) : typ option =
  match t with
  | TmVar v -> Some (lookup g v)
  | TmAbs (v, ty, tr) -> (
      match type_infer (extend g v ty) tr with
      | Some ty0 -> Some (TFun (ty, ty0))
      | _ -> None)
  | TmApp (tl, tr) -> (
      match type_infer g tl with
      | Some (TFun (dmn, rng)) -> (
          match type_infer g tr with
          | Some tyarg -> if tyarg = dmn then Some rng else None
          | _ -> None)
      | _ -> None)
  | TmTrue | TmFalse -> Some TBool
  | TmZero -> Some TNat
  | TmIsZero trm -> (
      match type_infer g trm with Some TNat -> Some TBool | _ -> None)
  | TmSucc trm | TmPred trm -> (
      match type_infer g trm with Some TNat -> Some TNat | _ -> None)
  | TmIf (t0, t1, t2) -> (
      match type_infer g t0 with
      | Some TBool ->
          if type_infer g t1 = type_infer g t2 then type_infer g t1 else None
      | _ -> None)
  | TmPair (t1, t2) -> (
      match (type_infer g t1, type_infer g t2) with
      | Some ty1, Some ty2 -> Some (TProd (ty1, ty2))
      | _ -> None)
  | TmFst t -> (
      match type_infer g t with Some (TProd (ty1, _)) -> Some ty1 | _ -> None)
  | TmSnd t -> (
      match type_infer g t with Some (TProd (_, ty2)) -> Some ty2 | _ -> None)
  | TmUnit -> Some TUnit
  | TmInl (tysum, t) -> (
      match (tysum, type_infer g t) with
      | TSum (tyl, tyr), Some tyl' when tyl = tyl' -> Some (TSum (tyl, tyr))
      | _, _ -> None)
  | TmInr (tysum, t) -> (
      match (tysum, type_infer g t) with
      | TSum (tyl, tyr), Some tyr' when tyr = tyr' -> Some (TSum (tyl, tyr))
      | _, _ -> None)
  | TmCase (t0, x1, t1, x2, t2) -> (
      match type_infer g t0 with
      | Some (TSum (ty1, ty2)) -> (
          match
            (type_infer (extend g x1 ty1) t1, type_infer (extend g x2 ty2) t2)
          with
          | Some ty1', Some ty2' -> if ty1' = ty2' then Some ty1' else None
          | _ -> None)
      | _ -> None)
  | TmNil ty -> Some (TList ty)
  | TmCons (ty, t0, t1) -> (
      match (type_infer g t0, type_infer g t1) with
      | Some ty0, Some (TList ty1) ->
          if ty0 = ty1 && ty0 = ty then Some (TList ty) else None
      | _ -> None)
  | TmIsNil (ty, t) -> (
      match type_infer g t with
      | Some (TList ty') -> if ty = ty' then Some TBool else None
      | _ -> None)
  | TmHead (ty, t) -> (
      match type_infer g t with
      | Some (TList ty') -> if ty = ty' then Some (tp_opt ty) else None
      | _ -> None)
  | TmTail (ty, t) -> (
      match type_infer g t with
      | Some (TList ty') -> if ty = ty' then Some (TList ty) else None
      | _ -> None)
  | TmFix t -> (
      match type_infer g t with
      | Some (TFun (dmn, rng)) when dmn = rng -> Some dmn
      | _ -> None)

and
    (* return true if t has the given type in the context, else return false *)
    type_check (g : ctx) (t : term) (tp : typ) : bool =
  match t with
  | TmVar v -> lookup g v = tp
  | TmAbs (v, ty, tr) -> (
      match tp with
      | TFun (dmn, rng) -> dmn = ty && type_check (extend g v ty) tr rng
      | _ -> false)
  | TmApp (tl, tr) -> (
      let tpr = type_infer g tr in
      match tpr with
      | Some tpr' -> type_check g tl (TFun (tpr', tp))
      | _ -> false)
  | TmTrue | TmFalse -> tp = TBool
  | TmZero -> tp = TNat
  | TmIsZero trm -> tp = TBool && type_check g trm TNat
  | TmSucc trm | TmPred trm -> tp = TNat && type_check g trm TNat
  | TmIf (t0, t1, t2) ->
      type_check g t0 TBool && type_check g t1 tp && type_check g t2 tp
  | TmPair (t1, t2) -> (
      match tp with
      | TProd (tp1, tp2) -> type_check g t1 tp1 && type_check g t2 tp2
      | _ -> false)
  | TmFst t -> (
      match type_infer g t with Some (TProd (tp1, _)) -> tp1 = tp | _ -> false)
  | TmSnd t -> (
      match type_infer g t with Some (TProd (_, tp2)) -> tp2 = tp | _ -> false)
  | TmUnit -> tp = TUnit
  | TmInl (tysum, t) -> (
      tp = tysum
      && match tp with TSum (tp1, _) -> type_check g t tp1 | _ -> false)
  | TmInr (tysum, t) -> (
      tp = tysum
      && match tp with TSum (_, tp2) -> type_check g t tp2 | _ -> false)
  | TmCase (t0, x1, t1, x2, t2) -> (
      match type_infer g t0 with
      | Some (TSum (ty1, ty2)) ->
          type_check (extend g x1 ty1) t1 tp
          && type_check (extend g x2 ty2) t2 tp
      | _ -> false)
  | TmNil ty -> tp = TList ty
  | TmCons (ty, t0, t1) -> (
      match tp with
      | TList tp' -> ty = tp' && type_check g t0 tp' && type_check g t1 tp
      | _ -> false)
  | TmIsNil (ty, t) -> (
      match type_infer g t with
      | Some (TList ty') -> ty = ty' && tp = TBool
      | _ -> false)
  | TmHead (ty, t) -> (
      match type_infer g t with
      | Some (TList ty') -> ty = ty' && tp = tp_opt ty
      | _ -> false)
  | TmTail (ty, t) -> (
      match tp with TList tp' -> tp' = ty && type_check g t tp | _ -> false)
  | TmFix t -> (
      match type_infer g t with
      | Some (TFun (dmn, rng)) -> dmn = rng
      | _ -> false)