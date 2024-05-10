open Hw5.Etlc
open Hw5.Lang

(* Interpreter *)

(* This is for you to debug your code. *)
(* The current interpret function will parse a term and return its
   type in the empty context. *)
(* You're encouraged to add additional tests. *)

let rec typ_to_string (t : typ) : string =
  match t with
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TFun (t1, t2) -> "(" ^ typ_to_string t1 ^ " → " ^ typ_to_string t2 ^ ")"
  | TList t1 -> "List (" ^ typ_to_string t1 ^ ")"
  | TProd (t1, t2) -> typ_to_string t1 ^ " × " ^ typ_to_string t2
  | TSum (t1, t2) -> typ_to_string t1 ^ " + " ^ typ_to_string t2
  | TUnit -> "Unit"

let rec term_to_string (t : term) : string =
  match t with
  | TmVar str -> str
  | TmAbs (var, tp, body) ->
      "&" ^ var ^ ":" ^ typ_to_string tp ^ "." ^ term_to_string body
  | TmApp (t1, t2) -> "(" ^ term_to_string t1 ^ ") (" ^ term_to_string t2 ^ ")"
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"
  | TmIf (t1, t2, t3) ->
      "if " ^ term_to_string t1 ^ " then " ^ term_to_string t2 ^ " else "
      ^ term_to_string t3
  | TmSucc t1 -> "succ " ^ term_to_string t1
  | TmPred t1 -> "pred " ^ term_to_string t1
  | TmIsZero t1 -> "0? " ^ term_to_string t1
  | TmCase (t0, x1, t1, x2, t2) ->
      "case " ^ term_to_string t0 ^ " of inl " ^ x1 ^ " => " ^ term_to_string t1
      ^ " | inr " ^ x2 ^ " => " ^ term_to_string t2
  | TmInl (ty, tm) -> "inl " ^ term_to_string tm ^ " with " ^ typ_to_string ty
  | TmInr (ty, tm) -> "inr " ^ term_to_string tm ^ " with " ^ typ_to_string ty
  | TmNil ty -> "nil[" ^ typ_to_string ty ^ "]"
  | TmCons (ty, tm1, tm2) ->
      "cons[" ^ typ_to_string ty ^ "] " ^ term_to_string tm1 ^ " "
      ^ term_to_string tm2
  | TmIsNil (ty, tm) -> "isnil[" ^ typ_to_string ty ^ "] " ^ term_to_string tm
  | TmHead (ty, tm) -> "head[" ^ typ_to_string ty ^ "] " ^ term_to_string tm
  | TmTail (ty, tm) -> "tail[" ^ typ_to_string ty ^ "] " ^ term_to_string tm
  | TmFst tm -> term_to_string tm ^ ".1"
  | TmSnd tm -> term_to_string tm ^ ".2"
  | TmUnit -> "unit"
  | TmPair (tm1, tm2) ->
      "{ " ^ term_to_string tm1 ^ " , " ^ term_to_string tm2 ^ " }"
  | TmFix t -> "fix " ^ term_to_string t

let opt_typ_to_string (o : typ option) : string =
  match o with None -> "??" | Some t -> typ_to_string t

let interpret (ast : term) : unit =
  let term_str = term_to_string ast in
  let _ = print_endline "----- Call by Value Multistep Evaluation -----" in
  let _ = print_endline ("      " ^ term_str) in
  let _ =
    print_endline
      ("->    "
      ^ term_to_string (multistep ast)
      ^ " : "
      ^ opt_typ_to_string (type_infer empty_ctx ast))
  in
  print_endline ""
;;

let arg1 = Sys.argv.(1) in
if arg1 = "-f" then
  let ast = Hw5.Parse.parse_file Sys.argv.(2) in
  interpret ast
else
  let ast = Hw5.Parse.parse arg1 in
  interpret ast
