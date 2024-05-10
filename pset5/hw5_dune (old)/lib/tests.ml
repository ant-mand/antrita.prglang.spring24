open Etlc
open Lang

let p = Parse.parse
let pt = Parse.parse_typ

let iseven () =
  p
    {|
fix (
    λie:Nat → Bool.λx:Nat.
      if 0? x then true
      else if 0? (pred x) then false 
      else ie (pred (pred x))
    )
  |}

(* spoilers for exercise 11.11.1*)
let fact () =
  p
    {|
let plus : Nat → Nat → Nat =
  fix (λf:Nat → Nat → Nat.λx:Nat.λy:Nat.if 0? x then y else succ (f (pred x) y))
in
let mult : Nat × Nat → Nat =
  fix (λf:Nat × Nat → Nat.λp:Nat × Nat.if 0? p.1
                                       then 0
                                       else if 0? (pred p.1)
                                       then p.2
                                       else plus p.2 (f {pred p.1, p.2}))
in
letrec fact : Nat → Nat = λx:Nat.if 0? x then succ 0 else mult {x, (fact (pred x))} 
in
  fact 
|}

let filter () =
  p
    {|
letrec filter : List Nat × List Nat → List Nat = λpl:List Nat × List Nat.(
  let headd : Option Nat = head[Nat] pl.1 in
  let flag  : Option Nat = head[Nat] pl.2 in
  option case headd of
      some p => (option case flag 
      of some f => if 0? f
                  then filter {tail[Nat] pl.1, tail[Nat] pl.2}
                  else cons[Nat] p (filter {tail[Nat] pl.1, tail[Nat] pl.2})

       | none => nil[Nat]
      )
    | none => nil[Nat]

  )
in 
  filter
|}

let reverse () =
  p
    {|
let reverse : List Nat → List Nat = 
  letrec reverseaux : List Nat → List Nat → List Nat =
    λacc:List Nat.λl:List Nat.
      (option case head[Nat] l of
          some h => reverseaux (cons[Nat] h acc) (tail[Nat] l)
        | none   => acc
      )
  in
    reverseaux nil[Nat]
in 
  reverse 
|}

let unwrap () =
  p
    {|
let unwrap : Option Nat → Nat = λol:Option Nat.
  option case ol of
     some l => l
   | none => 0
in 
  unwrap
|}


(* The following tests depend on multistep (and thus cbv) *)
(********************************* tm_seq *********************************)

let%test _ = multistep (p "unit;false") = TmFalse

let%test _ =
  multistep (p "unit;inr false as Bool + Bool") = p "inr false as Bool + Bool"

(********************************* tm_let *********************************)
let%test _ = multistep (p "let x : Bool = true in x") = p "true"

let%test _ =
  multistep
    (p
       {|
      let iszerobutfun : Nat → Bool = λx:Nat.0? x in 
      let succbutfun : Nat → Nat = λx:Nat.succ x in
      iszerobutfun (succbutfun 0)
   |})
  = p "false"

(*

(* The following tests depend on type_check *)
(********************************* tm_ascribe *********************************)
let%test _ = type_check empty_ctx (p "0 as Nat") TNat
let%test _ = not (type_check empty_ctx (p "true as Nat") TNat)
let%test _ = not (type_check empty_ctx (p "true as Nat") TBool)

let%test _ =
  type_check empty_ctx
    (p "(λs:Nat + Bool.0) as Nat + Bool → Nat")
    (pt "Nat + Bool → Nat")

let%test _ =
  not
    (type_check empty_ctx
       (p "(λs:Nat + Bool.0) as Nat + Bool → Bool")
       (pt "Nat + Bool → Bool"))

let%test _ = type_check empty_ctx (p "inl 0 as Nat + Bool") (pt "Nat + Bool")
let%test _ = type_check empty_ctx (p "inr true as Nat + Bool") (pt "Nat + Bool")

(* The following tests depend on empty_ctx & type_check *)
(************************ tp_opt, tm_some, tm_none ************************)
let%test _ = type_check empty_ctx (p "some[Nat] 0") (pt "Option Nat")
let%test _ = type_check empty_ctx (p "none[Bool]") (pt "Option Bool")

*)

(* The following tests depend on multistep (and thus cbv) *)
(************************ tm_opt_case ************************)
let%test _ =
  multistep (p "option case head[Nat] nil[Nat] of some x => succ x | none => 0")
  = p "0"

let%test _ =
  multistep
    (p
       "option case head[Nat] cons[Nat] 0 nil[Nat] of some x => succ x | none \
        => 0")
  = p "succ 0"

(************************ tm_letrec ************************)
let%test _ =
  multistep
    (TmApp
       ( filter (),
         p
           {|
    {cons[Nat] 0 (cons[Nat] succ 0 (cons[Nat] succ succ 0 nil[Nat])),
    cons[Nat] succ 0 (cons[Nat] 0 (cons[Nat] succ 0 nil[Nat]))}
  |}
       ))
  = p "cons[Nat] 0 (cons[Nat] succ succ 0 nil[Nat])"

  (*

(* some of these tests depend on the derived forms. make sure you work on those first
   maybe by observing the behavior specified in the tests (or exactly following the book) *)
(************************ cbv ************************)
let%test _ =
  cbv
    (p
       "cons[Bool] (if true then false else true) (if false then cons[Bool] \
        true nil[Bool] else nil[Bool])")
  = Some
      (p
         "cons[Bool] false (if false then cons[Bool] true nil[Bool] else \
          nil[Bool])")

let%test _ =
  cbv
    (p
       "cons[Bool] false (if false then cons[Bool] true nil[Bool] else \
        nil[Bool])")
  = Some (p "cons[Bool] false nil[Bool]")

let%test _ = cbv (p "cons[Bool] false nil[Bool]") = None

let%test _ =
  cbv (p "nil?[Bool] (cons[Bool] false nil[Bool])") = Some (p "false")

let%test _ =
  cbv (p "nil?[Bool] (if true then nil[Bool] else cons[Bool] true nil[Bool])")
  = Some (p "nil?[Bool] nil[Bool]")

let%test _ = cbv (p "nil?[Bool] nil[Bool]") = Some (p "true")
let%test _ = cbv (p "head[Nat] (cons[Nat] 0 nil[Nat])") = Some (p "some[Nat] 0")
let%test _ = cbv (p "head[Nat] nil[Nat]") = Some (p "none[Nat]")
let%test _ = cbv (p "tail[Bool] nil[Bool]") = Some (p "nil[Bool]")

let%test _ =
  cbv (p "tail[Bool] (cons[Bool] true nil[Bool])") = Some (p "nil[Bool]")

let%test _ =
  cbv (p "tail[Bool] (cons[Bool] false (cons[Bool] true nil[Bool]))")
  = Some (p "cons[Bool] true nil[Bool]")

let%test _ =
  cbv
    (p
       {|
  case inl 0 as Nat + Unit of 
      inl r => succ r
    | inr _ => 0
  |})
  = Some (p "succ 0")

let%test _ = cbv (p "{0, succ 0}.1") = Some (p "0")
let%test _ = cbv (p "{0, succ 0}.2") = Some (p "succ 0")

let%test _ =
  cbv (p "{if true then 0 else succ 0, if true then false else true}.1")
  = Some (p "{0, if true then false else true}.1")

let%test _ =
  cbv (p "{0, if true then false else true}.1") = Some (p "{0, false}.1")

let%test _ = cbv (p "{0, false}.1") = Some (p "0")

let%test _ =
  cbv (TmApp (iseven (), p "0"))
  = Some
      (TmApp
         ( TmAbs
             ( "x",
               TNat,
               TmIf
                 ( p "0? x",
                   p "true",
                   TmIf
                     ( p "0? (pred x)",
                       p "false",
                       TmApp (iseven (), p "pred (pred x)") ) ) ),
           p "0" ))

let%test _ =
  cbv
    (TmApp
       ( TmAbs
           ( "x",
             TNat,
             TmIf
               ( p "0? x",
                 p "true",
                 TmIf
                   ( p "0? (pred x)",
                     p "false",
                     TmApp (iseven (), p "pred (pred x)") ) ) ),
         p "0" ))
  = Some
      (TmIf
         ( p "0? 0",
           p "true",
           TmIf
             (p "0? (pred 0)", p "false", TmApp (iseven (), p "pred (pred 0)"))
         ))

let%test _ =
  cbv
    (TmIf
       ( p "0? 0",
         p "true",
         TmIf (p "0? (pred 0)", p "false", TmApp (iseven (), p "pred (pred 0)"))
       ))
  = Some
      (TmIf
         ( p "true",
           p "true",
           TmIf
             (p "0? (pred 0)", p "false", TmApp (iseven (), p "pred (pred 0)"))
         ))

let%test _ =
  cbv
    (TmIf
       ( p "true",
         p "true",
         TmIf (p "0? (pred 0)", p "false", TmApp (iseven (), p "pred (pred 0)"))
       ))
  = Some (p "true")


(************************ multistep ************************)
let%test _ = multistep (TmApp (iseven (), p "succ succ succ 0")) = p "false"
let%test _ = multistep (TmApp (iseven (), p "succ succ succ succ 0")) = p "true"
let%test _ = multistep (TmApp (fact (), p "0")) = p "succ 0"

let%test _ =
  multistep (TmApp (fact (), p "succ succ succ 0"))
  = p "succ succ succ succ succ succ 0"

let%test _ =
  multistep
    (TmApp
       ( reverse (),
         p "cons[Nat] 0 (cons[Nat] succ 0 (cons[Nat] succ succ 0 nil[Nat]))" ))
  = p "cons[Nat] succ succ 0 (cons[Nat] succ 0 (cons[Nat] 0 nil[Nat]))"

let%test _ = multistep (TmApp (unwrap (), p "some[Nat] 0")) = p "0"
let%test _ = multistep (TmApp (unwrap (), p "some[Nat] succ 0")) = p "succ 0"
let%test _ = multistep (TmApp (unwrap (), p "none[Nat]")) = p "0"

*)

(*

(************************ type_infer ************************)
let%test _ =
  type_infer empty_ctx
    (p
       "cons[Bool] (if true then false else true) (if false then cons[Bool] \
        true nil[Bool] else nil[Bool])")
  = Some (pt "List Bool")

let%test _ =
  type_infer empty_ctx (p "nil?[Bool] (cons[Bool] false nil[Bool])")
  = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx (p "head[Nat] (cons[Nat] 0 nil[Nat])")
  = Some (pt "Option Nat")

let%test _ =
  type_infer empty_ctx (p "head[Nat] nil[Nat]") = Some (pt "Option Nat")

let%test _ =
  type_infer empty_ctx (p "tail[Bool] nil[Bool]") = Some (pt "List Bool")

let%test _ =
  type_infer empty_ctx (p "tail[Bool] (cons[Bool] true nil[Bool])")
  = Some (pt "List Bool")

let%test _ =
  type_infer empty_ctx
    (p "tail[Bool] (cons[Bool] false (cons[Bool] true nil[Bool]))")
  = Some (pt "List Bool")

let%test _ =
  type_infer empty_ctx (p "inl 0 as Nat + Bool") = Some (pt "Nat + Bool")

let%test _ =
  type_infer empty_ctx (p "inr 0 as Bool + Nat") = Some (pt "Bool + Nat")

let%test _ =
  type_infer empty_ctx
    (p
       {|
  case inr 0 as Bool + Nat of 
      inl b => b
    | inr n => n
      |})
  = None

let%test _ = type_infer empty_ctx (p "{0, succ 0}") = Some (pt "Nat × Nat")
let%test _ = type_infer empty_ctx (p "{0, succ 0}.1") = Some (pt "Nat")
let%test _ = type_infer empty_ctx (p "{0, succ 0}.2") = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx
    (p "{if true then 0 else succ 0, if true then false else true}.1")
  = Some (pt "Nat")

let%test _ = type_infer empty_ctx (p "{0, false}.2") = Some (pt "Bool")
let%test _ = type_infer empty_ctx (TmApp (iseven (), p "0")) = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx (TmApp (reverse (), p "nil[Nat]")) = Some (pt "List Nat")

let%test _ =
  type_infer empty_ctx (TmApp (filter (), p "{nil[Nat], nil[Nat]}"))
  = Some (pt "List Nat")

let%test _ =
  (* do not multistep this term! *)
  type_infer empty_ctx (p "letrec x : Nat = x in x") = Some (pt "Nat")

(************************ type_check ************************)
let%test _ =
  type_check empty_ctx
    (p
       "cons[Bool] (if true then false else true) (if false then cons[Bool] \
        true nil[Bool] else nil[Bool])")
    (pt "List Bool")

let%test _ =
  type_check empty_ctx (p "nil?[Bool] (cons[Bool] false nil[Bool])") (pt "Bool")

let%test _ =
  type_check empty_ctx (p "head[Nat] (cons[Nat] 0 nil[Nat])") (pt "Option Nat")

let%test _ = type_check empty_ctx (p "head[Nat] nil[Nat]") (pt "Option Nat")
let%test _ = type_check empty_ctx (p "tail[Bool] nil[Bool]") (pt "List Bool")

let%test _ =
  type_check empty_ctx
    (p "tail[Bool] (cons[Bool] true nil[Bool])")
    (pt "List Bool")

let%test _ =
  type_check empty_ctx
    (p "tail[Bool] (cons[Bool] false (cons[Bool] true nil[Bool]))")
    (pt "List Bool")

let%test _ = type_check empty_ctx (p "inl 0 as Nat + Bool") (pt "Nat + Bool")
let%test _ = type_check empty_ctx (p "inr 0 as Bool + Nat") (pt "Bool + Nat")

let%test _ =
  not
    (type_check empty_ctx
       (p
          {|
  case inr 0 as Bool + Nat of 
      inl b => b
    | inr n => n
      |})
       (pt "Nat + Bool"))

let%test _ =
  type_check empty_ctx
    (p
       {|
  case inr 0 as Bool + Nat of 
      inl b => inr b as Nat + Bool
    | inr n => inl n as Nat + Bool
      |})
    (pt "Nat + Bool")

let%test _ = type_check empty_ctx (p "{0, succ 0}") (pt "Nat × Nat")
let%test _ = type_check empty_ctx (p "{0, succ 0}.1") (pt "Nat")
let%test _ = type_check empty_ctx (p "{0, succ 0}.2") (pt "Nat")

let%test _ =
  type_check empty_ctx
    (p "{if true then 0 else succ 0, if true then false else true}.1")
    (pt "Nat")

let%test _ = type_check empty_ctx (p "{0, false}.2") (pt "Bool")
let%test _ = type_check empty_ctx (TmApp (iseven (), p "0")) (pt "Bool")

let%test _ =
  type_check empty_ctx (TmApp (reverse (), p "nil[Nat]")) (pt "List Nat")

let%test _ =
  type_check empty_ctx
    (TmApp (filter (), p "{nil[Nat], nil[Nat]}"))
    (pt "List Nat")

*)
