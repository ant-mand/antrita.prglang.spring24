open Etlc

let rec unparse_typ = function
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TNat -> "Nat"
  | TFun (dmn, rng) -> unparse_typ dmn ^ " → " ^ unparse_typ rng
  | TRef ty -> "Ref (" ^ unparse_typ ty ^ ")"
  | TVariant tys ->
      "["
      ^ String.concat ";"
          (List.map (fun (lbl, ty) -> lbl ^ ":" ^ unparse_typ ty) tys)
      ^ "]"
  | TBot -> "!"

let rec unparse = function
  | TmVar v -> v
  | TmAbs (v, ty, body) ->
      Printf.sprintf "λ%s:%s.(%s)" v (unparse_typ ty) (unparse body)
  | TmApp (t0, t1) -> Printf.sprintf "((%s) (%s))" (unparse t0) (unparse t1)
  | TmUnit -> "unit"
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t0, t1, t2) ->
      Printf.sprintf "if %s then %s else (%s)" (unparse t0) (unparse t1)
        (unparse t2)
  | TmZero -> "0"
  | TmSucc t0 -> "succ (" ^ unparse t0 ^ ")"
  | TmPred t0 -> "pred (" ^ unparse t0 ^ ")"
  | TmIsZero t0 -> "0? (" ^ unparse t0 ^ ")"
  | TmVariant (lbl, t0, ty) ->
      Printf.sprintf "%s = %s as %s" lbl (unparse t0) (unparse_typ ty)
  | TmCase (t0, rls) ->
      Printf.sprintf "case %s of [%s]" (unparse t0)
        (String.concat " | "
           (List.map
              (fun (lbl, pat, t1) ->
                Printf.sprintf "%s %s => %s" lbl pat (unparse t1))
              rls))
  | TmRef t0 -> "ref (" ^ unparse t0 ^ ")"
  (* this is not meant to be parsable *)
  | TmLoc i -> Printf.sprintf "<loc:%d>" i
  | TmBang t0 -> "!" ^ unparse t0
  | TmAssn (t0, t1) -> Printf.sprintf "(%s) := (%s)" (unparse t0) (unparse t1)
  | TmRaise t0 -> "raise (" ^ unparse t0 ^ ")"
  | TmTry (t0, t1) ->
      Printf.sprintf "try %s with (%s)" (unparse t0) (unparse t1)
  | TmNull -> "null"
  | TmIsNull (ty, t0) ->
      Printf.sprintf "null?[%s] (%s)" (unparse_typ ty) (unparse t0)
