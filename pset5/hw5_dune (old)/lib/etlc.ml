module SS = Set.Make (String)

exception Not_implemented

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
  
  (* | TmAscribe of term * typ  ascription: t as T -> t *)

(* Utility functions from hw4 *)

let rec fv (t : term) : SS.t =
  match t with
  | TmVar var -> SS.singleton var
  | TmAbs (var, _, t) -> SS.remove var (fv t)
  | TmApp (t1, t2) -> SS.union (fv t1) (fv t2)
  | TmIf (t0, t1, t2) -> SS.union (SS.union (fv t0) (fv t1)) (fv t2)
  | TmSucc t -> fv t
  | TmPred t -> fv t
  | TmIsZero t -> fv t
  | TmPair (t1, t2) -> SS.union (fv t1) (fv t2)
  | TmFst t -> fv t
  | TmSnd t -> fv t
  | TmInl (_, t) -> fv t
  | TmInr (_, t) -> fv t
  | TmCase (t0, x1, t1, x2, t2) ->
      SS.union (SS.union (fv t0) (SS.remove x1 (fv t1))) (SS.remove x2 (fv t2))
  | TmCons (_, t0, t1) -> SS.union (fv t1) (fv t0)
  | TmIsNil (_, t) -> fv t
  | TmHead (_, t) -> fv t
  | TmTail (_, t) -> fv t
  | _ -> SS.empty

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
  let x = fresh_var (SS.union (fv t1) (fv t2)) in
  TmApp(TmAbs(x, TUnit, t2), t1)

let tm_let (x : string) (tp : typ) (t1 : term) (t2 : term) : term =
  TmApp(TmAbs(x, tp, t2), t1)

let tm_ascribe (t : term) (ty : typ) : term = t
let tp_opt (tp : typ) : typ =  TSum(tp, TUnit)
let tm_some (t : term) : term = TmInr (TSum(TNat, TUnit), t)
let tm_none (tp : typ) : term = TmInl (TSum(tp, TUnit), TmUnit)

(* x here is the variable that may be bound in the t_some term *)
let tm_opt_case (t : term) (x : string) (t_some : term) (t_none : term) : term =
  TmCase(t, x, t_some, "_", t_none)

let tm_letrec (x : string) (tp : typ) (t1 : term) (t2 : term) : term =
  TmApp(TmAbs(x, tp, t2), TmFix(TmAbs(x, tp, t1)))

