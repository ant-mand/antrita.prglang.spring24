exception Not_implemented
exception NotAbs of string
exception NotFound of string
exception DuplicateVar of string
exception CaptureException of string

(* Data Definitions *)

type label = string

type typ =
  | TUnit
  | TBool
  | TNat
  | TFun of typ * typ
  | TRef of typ
  | TVariant of (label * typ) list
  | TBot

type term =
  | TmVar of string
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
  | TmCase of term * (label * string * term) list
    (* eg. case red 2 of [red x => 5 | blue y => 9] *)
  | TmRef of term
  | TmLoc of int
  | TmBang of term (* !t *)
  | TmAssn of term * term
  | TmRaise of term
  | TmTry of term * term
  | TmNull
  | TmIsNull of typ * term

(* Variants *)

(* Implement an option type, some and none as variants *)

let tp_opt (tp : typ) : typ = TVariant ([("some", tp); ("none", TUnit)]) 

let tm_some (t : term) (tp : typ) : term = TmVariant ("some", t, (tp_opt tp))

let tm_none (tp : typ) : term = TmVariant ("none", TmUnit, (tp_opt tp))

(* Implement an exception type as a variant. 
   There are at least three possible exceptions that you should be able to handle. 
   (These should become clear as you progress through the homework, but feel free to start
    with some obvious candidates. No points will be deducted for having extra exceptions.) *)
let tp_exn : typ = TVariant ([("not_found", TUnit); 
                              ("null_pointer", TUnit);
                              ("no_match", TUnit)])
