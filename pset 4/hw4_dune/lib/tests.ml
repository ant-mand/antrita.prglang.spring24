open Lang
open Tlc
open Parse

(****** ctx, empty_ctx, lookup, & extend *****)
let%test _ = lookup empty_ctx "x" = None
let%test _ = lookup empty_ctx "y" = None
let%test _ = lookup (extend empty_ctx "x" TNat) "x" = Some TNat

let%test _ =
  lookup (extend (extend empty_ctx "x" TBool) "y" TNat) "x" = Some TBool

let%test _ =
  lookup (extend (extend empty_ctx "x" TBool) "y" TNat) "y" = Some TNat

let%test _ = lookup (extend (extend empty_ctx "x" TBool) "y" TNat) "z" = None

let%test _ =
  lookup (extend (extend empty_ctx "x" TBool) "x" TNat) "x" = Some TNat

(***************** type_check *****************)
let%test _ = not (type_check empty_ctx (parse "x") TNat)
let%test _ = type_check (extend empty_ctx "x" TNat) (parse "x") TNat
let%test _ = type_check empty_ctx (parse "0") TNat
let%test _ = type_check empty_ctx (parse "Succ Succ 0") TNat
let%test _ = not (type_check empty_ctx (parse "Succ True") TNat)
let%test _ = not (type_check empty_ctx (parse "True") TNat)
let%test _ = type_check empty_ctx (parse "IsZero 0") TBool
let%test _ = type_check empty_ctx (parse "λx:Nat.x") (TFun (TNat, TNat))

let%test _ =
  not (type_check empty_ctx (parse "λx:Nat.0? x") (TFun (TNat, TNat)))

let%test _ = type_check empty_ctx (parse "((λx:Nat.0? x) (Succ 0))") TBool

let%test _ =
  type_check empty_ctx
    (parse "((λx:Nat.λy:Nat.λz:Nat.If 0? x Then y Else z) (Succ 0))")
    (TFun (TNat, TFun (TNat, TNat)))

let%test _ =
  not
    (type_check empty_ctx
       (parse "((λx:Nat.λy:Nat.λz:Nat.If 0? x Then y Else 0? z) (Succ 0))")
       (TFun (TNat, TFun (TNat, TNat))))

let%test _ =
  not
    (type_check empty_ctx
       (parse "((λx:Nat.λy:Nat.λz:Nat.If x Then y Else z) (Succ 0))")
       (TFun (TNat, TFun (TNat, TNat))))

let%test _ =
  type_check empty_ctx
    (parse "((λx:(Nat -> Nat).λy:Nat.(x y) (λx:Nat.Succ x)) 0)")
    TNat

(**************** type_infer *****************)
let%test _ = type_infer empty_ctx (parse "x") = None
let%test _ = type_infer (extend empty_ctx "x" TNat) (parse "x") = Some TNat
let%test _ = type_infer empty_ctx (parse "0") = Some TNat
let%test _ = type_infer empty_ctx (parse "Succ Succ 0") = Some TNat
let%test _ = type_infer empty_ctx (parse "Succ True") = None
let%test _ = type_infer empty_ctx (parse "True") = Some TBool
let%test _ = type_infer empty_ctx (parse "IsZero 0") = Some TBool
let%test _ = type_infer empty_ctx (parse "λx:Nat.x") = Some (TFun (TNat, TNat))

let%test _ =
  type_infer empty_ctx (parse "λx:Nat.0? x") = Some (TFun (TNat, TBool))

let%test _ =
  type_infer empty_ctx (parse "((λx:Nat.0? x) (Succ 0))") = Some TBool

let%test _ =
  type_infer empty_ctx
    (parse "((λx:Nat.λy:Nat.λz:Nat.If 0? x Then y Else z) (Succ 0))")
  = Some (TFun (TNat, TFun (TNat, TNat)))

let%test _ =
  type_infer empty_ctx
    (parse "((λx:Nat.λy:Nat.λz:Nat.If 0? x Then y Else 0? z) (Succ 0))")
  = None

let%test _ =
  type_infer empty_ctx
    (parse "((λx:Nat.λy:Nat.λz:Nat.If x Then y Else z) (Succ 0))")
  = None

let%test _ =
  type_infer empty_ctx
    (parse "((λx:(Nat -> Nat).λy:Nat.(x y) (λx:Nat.Succ x)) 0)")
  = Some TNat
