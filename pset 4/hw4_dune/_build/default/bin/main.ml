open Hw4.Tlc
open Hw4.Lang

let rec typ_to_string (t : typ) : string =
  match t with
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TFun (t1, t2) -> "(" ^ typ_to_string t1 ^ " → " ^ typ_to_string t2 ^ ")"

let rec term_to_string (t : term) : string =
  match t with
  | TmVar str -> str
  | TmAbs (var, tp, body) ->
      "λ" ^ var ^ ":" ^ typ_to_string tp ^ "." ^ term_to_string body
  | TmApp (t1, t2) -> "(" ^ term_to_string t1 ^ " " ^ term_to_string t2 ^ ")"
  | TmTrue -> "True"
  | TmFalse -> "False"
  | TmZero -> "0"
  | TmIf (t1, t2, t3) ->
      "If " ^ term_to_string t1 ^ " Then " ^ term_to_string t2 ^ " Else "
      ^ term_to_string t3
  | TmSucc t1 -> "Succ " ^ term_to_string t1
  | TmPred t1 -> "Pred " ^ term_to_string t1
  | TmIsZero t1 -> "0? " ^ term_to_string t1

let opt_typ_to_string (o : typ option) : string =
  match o with None -> " " | Some t -> typ_to_string t

let interpret (str : string) : unit =
  let ast = Hw4.Parse.parse str in
  let term_str = term_to_string ast in
  let _ = print_endline "----- Type Checking -----" in
  let _ = print_endline ("      " ^ term_str) in
  let _ = print_endline (": " ^ opt_typ_to_string (type_infer empty_ctx ast)) in
  print_endline ""
;;

interpret Sys.argv.(1)
