open Etlc
open Lang
open Util
module E = Examples

(* The following tests depend on multistep (and thus cbv) *)
(************************ tp_opt, tm_some, tm_none ************************)
let%test _ =
  multistep (p (spf "(%s) some[Nat] 0" (up (E.unwrap ())))) IMap.empty
  = (p "0", IMap.empty)

let%test _ =
  multistep (p (spf "(%s) none[Nat]" (up (E.unwrap ())))) IMap.empty
  = (p "0", IMap.empty)

let%test _ =
  multistep (p (spf "(%s) some[Nat] (succ 0)" (up (E.unwrap ())))) IMap.empty
  = (p "succ 0", IMap.empty)


(************************ tp_exn ************************)
let%test "non-zero variants" =
  match tp_exn with TVariant vars -> not (List.is_empty vars) | _ -> false

(* The following tests depend on multistep (and thus cbv) *)
let%test _ =
  multistep (p (spf {|
try !null with
    λe:%s.0
|} (upt tp_exn))) IMap.empty
  = (p "0", IMap.empty)

let%test _ =
  multistep
    (p
       (spf
          {|
try (case blue = succ 0 as [red:Nat;green:Nat] of [
    red   s => succ s
  | green s => succ s
]) with
    λe:%s.0
|}
          (upt tp_exn)))
    IMap.empty
  = (p "0", IMap.empty)

let%test _ =
  multistep
    (TmTry (TmBang (TmLoc 0), p (spf "λe:%s.0" (upt tp_exn))))
    IMap.empty
  = (p "0", IMap.empty)

(* The following tests depend on type_check *)
let%test _ =
  type_infer empty_ctx empty_store
    (p (spf {|
try !null with
    λe:%s.0
|} (upt tp_exn)))
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store
    (p
       (spf
          {|
try !(if true then ref 0 else null) with
    λe:%s.0
|}
          (upt tp_exn)))
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p {|
  try !null with
    λe:[].0
|}) = None 


(************************ cbv ************************)

(* Some of the below tests, specifically those that test
   that you properly raise an exception (search for `is_err`),
   use `type_check` to make sure that what you raise is indeed
   an exception. As such, a working `type_check` implementation
   would be needed for those tests to pass (in addition to a working `cbv`
   implementation, of course). *)

let%test _ =
  let exn = up (example_value tp_exn) in
  cbv (p (spf "raise (raise %s)" exn)) IMap.empty
  = Some (p (spf "raise %s" exn), IMap.empty)

let%test _ =
  cbv (p "raise (if true then x else y)") IMap.empty
  = Some (p "raise x", IMap.empty)

let%test _ =
  let exn = example_value tp_exn in
  cbv (p (spf "(λx:Bool.0) (raise %s)" (up exn))) IMap.empty
  = Some (p (spf "raise %s" (up exn)), IMap.empty)

let%test _ =
  let exn = example_value tp_exn in
  cbv (p (spf "(raise %s) (λx:Bool.0) " (up exn))) IMap.empty
  = Some (p (spf "raise %s" (up exn)), IMap.empty)

let%test _ =
  match cbv (p "null 0") IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  cbv (p "if true then null else 0") IMap.empty = Some (p "null", IMap.empty)

let%test _ =
  match cbv (p "if null then true else false") IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  cbv (p "succ (if true then null else succ 0)") IMap.empty
  = Some (p "succ null", IMap.empty)

let%test _ =
  match cbv (p "succ null") IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  let exn = up (example_value tp_exn) in
  match cbv (p (spf "succ (raise %s)" exn)) IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  cbv (p "pred (if true then null else succ 0)") IMap.empty
  = Some (p "pred null", IMap.empty)

let%test _ =
  match cbv (p "pred null") IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  let exn = up (example_value tp_exn) in
  match cbv (p (spf "pred (raise %s)" exn)) IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false


(* what about `0?` and other constructs that try to
   consume the `null` ?... *)

let%test _ =
  cbv
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    IMap.empty
  = Some (p "0? succ succ succ 0", IMap.empty)

let%test _ =
  match
    cbv
      (p
         {|
    case null of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
      IMap.empty
  with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  match
    cbv
      (p
         {|
    case g = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | b s => 0? s
    ]
|})
      IMap.empty
  with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  cbv
    (p
       {|
    case g = null as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    IMap.empty
  = Some (p "0? null", IMap.empty)

let%test _ =
  let exn = up (example_value tp_exn) in
  cbv
    (p
       (spf
          {|
    case g = (raise %s) as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|}
          exn))
    IMap.empty
  = Some
      ( p
          (spf
             {|
    case raise %s of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|}
             exn),
        IMap.empty )

let%test _ =
  let exn = up (example_value tp_exn) in
  match
    cbv_n 2
      (p
         (spf
            {|
    case g = (raise %s) as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|}
            exn))
      IMap.empty
  with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  cbv (p "ref (if false then 0 else succ 0)") IMap.empty
  = Some (p "ref succ 0", IMap.empty)

let%test _ =
  match cbv (p "ref succ 0") IMap.empty with
  | Some (TmLoc i, mu) -> IMap.find i mu = p "succ 0"
  | _ -> false

let%test _ =
  match cbv (p "ref null") IMap.empty with
  | Some (TmLoc i, mu) -> IMap.find i mu = p "null"
  | _ -> false

let%test _ =
  let exn = up (example_value tp_exn) in
  match cbv (p (spf "ref (raise %s)" exn)) IMap.empty with
  | Some (t', _) -> is_err t'
  | _ -> false

let%test _ =
  match cbv_n 2 (p "!ref succ 0") IMap.empty with
  (* obviously `mu` (the wildcard below) should contain a new reference,
     but testing that should be captured above *)
  | Some (TmSucc TmZero, _) -> true
  | _ -> false

let%test _ =
  match cbv_n 2 (p "!ref null") IMap.empty with
  | Some (TmNull, _) -> true
  | _ -> false

let%test _ =
  match cbv_n 3 (p "!!ref null") IMap.empty with
  | Some (t', _) -> is_err t'
  | _ -> false

let%test _ =
  let exn = up (example_value tp_exn) in
  match cbv_n 2 (p (spf "!ref (raise %s)" exn)) IMap.empty with
  | Some (t', _) -> is_err t'
  | _ -> false

let%test _ =
  (* regular `Nat`s do not function as locations.
     (consult 11.10 (especially Single-Field Variants).
      Does that make you think of anything that could
      reduce the core of the language? Could we
      get away with not baking in the `Ref` type
      (i.e. having it as a derived form like `Option`)?) *)
  cbv (p "!0") (IMap.add 0 TmTrue IMap.empty) = None

let%test _ =
  let mu = IMap.add 0 TmTrue IMap.empty in
  (* There's no way to construct a location using the parser,
     since we want the language to construct them as `ref`s step *)
  cbv (TmBang (TmLoc 0)) mu = Some (TmTrue, mu)

let%test _ =
  let t', mu = cbv (p "ref true := false") IMap.empty |> Option.get in
  match t' with
  | TmAssn (TmLoc i, TmFalse) ->
      IMap.find i mu = p "true"
      &&
      let t'', mu' = cbv t' mu |> Option.get in
      t'' = p "unit" && IMap.find i mu' = p "false"
  | _ -> false (* ?? *)

let%test _ =
  let t', mu =
    cbv (TmApp (E.incc (), p "ref (succ 0)")) IMap.empty |> Option.get
  in
  match t' with
  | TmApp (_, TmLoc i) ->
      IMap.find i mu = p "succ 0"
      &&
      let t'', mu' = cbv_n 3 t' mu |> Option.get in
      t'' = p "unit" && IMap.find i mu' = p "succ succ 0"
  | _ -> false (* ?? *)

let%test _ =
  match cbv (p "null := false") IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

let%test _ =
  let t', mu = cbv (p "ref 0 := null") IMap.empty |> Option.get in
  match t' with
  | TmAssn (TmLoc i, TmNull) -> (
      IMap.find i mu = TmZero
      &&
      match cbv t' mu with
      | Some (TmUnit, mu') -> IMap.find i mu' = TmNull
      | _ -> false)
  | _ -> false

let%test _ =
  cbv (p (spf {|
try 0 with 
    λe:%s.succ 0
|} (upt tp_exn))) IMap.empty
  = Some (p "0", IMap.empty)

let%test _ =
  cbv (p (spf {|
try null with 
    λe:%s.succ 0
|} (upt tp_exn))) IMap.empty
  = Some (p "null", IMap.empty)

let%test _ =
  let exn = up (example_value tp_exn) in
  let tp_exn_str = upt tp_exn in
  cbv (p (spf {|
try raise %s with
    λe:%s.true
|} exn tp_exn_str)) IMap.empty
  = Some (p (spf "(λe:%s.true) %s" tp_exn_str exn), IMap.empty)

let%test _ =
  let exn = up (example_value tp_exn) in
  let tp_exn_str = upt tp_exn in
  cbv_n 2
    (p (spf {|
try raise %s with
    λe:%s.true
|} exn tp_exn_str))
    IMap.empty
  = Some (p "true", IMap.empty)

let%test _ = cbv (p "null?[Nat] null") IMap.empty = Some (p "true", IMap.empty)
let%test _ = cbv (p "null?[Nat] 0") IMap.empty = Some (p "false", IMap.empty)

let%test _ =
  let exn = up (example_value tp_exn) in
  match cbv (p (spf "null?[Nat] (raise %s)" exn)) IMap.empty with
  | Some (t', _) -> is_err t'
  | None -> false

(************************ multistep ************************)
let%test _ =
  let weekday = upt (E.weekday ()) in
  let day = spf "friday = unit as %s" weekday in
  (* we're applying the next_business_day function to the weekday friday *)
  multistep (p (spf "(%s) (%s)" (up (E.next_business_day ())) day)) IMap.empty
  = (p (spf "monday = unit as %s" weekday), IMap.empty)

let%test _ =
  let array = up (E.new_array_with [ 1; 5; 2 ]) in
  let lookup = up (E.lookup ()) in
  (* we're applying the lookup function to
     an ETLC array with Nats 1, 5, and 2 to extract
     the first (zeroth) element *)
  let t = p (spf "(%s) (%s) 0" lookup array) in
  let t, _ = multistep t IMap.empty in
  t = p "succ 0"

let%test _ =
  let array = up (E.new_array_with [ 1; 5; 2 ]) in
  let lookup = up (E.lookup ()) in
  let t = p (spf "(%s) (%s) (succ 0)" lookup array) in
  let t, _ = multistep t IMap.empty in
  t = p "succ succ succ succ succ 0"

let%test _ =
  let array = up (E.new_array_with [ 1; 5; 2 ]) in
  let lookup = up (E.lookup ()) in
  let t = p (spf "(%s) (%s) (succ succ 0)" lookup array) in
  let t, _ = multistep t IMap.empty in
  t = p "succ succ 0"

let%test _ =
  let array = up (E.new_array_with [ 1; 5; 2 ]) in
  let lookup = up (E.lookup ()) in
  let t = p (spf "(%s) (%s) (succ succ succ 0)" lookup array) in
  let t, _ = multistep t IMap.empty in
  t = p "0"


(************************ type_check ************************)
let%test _ =
  type_check (extend empty_ctx "x" TNat) empty_store (p "x") (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store (p "λx:Nat.true") (pt "Nat → Bool")

let%test _ = type_check empty_ctx empty_store (p "λx:Nat.x") (pt "Nat → Nat")

let%test _ =
  type_check empty_ctx empty_store (p "λx:Nat.ref x") (pt "Nat → Ref Nat")

let%test _ =
  not
    (type_check empty_ctx empty_store (p "λx:Nat.ref x") (pt "Nat → Ref Bool"))

let%test _ =
  type_check empty_ctx empty_store (p "λx:Nat.null") (pt "Nat → Ref Nat")

let%test _ =
  type_check empty_ctx empty_store (p "λx:Nat.null") (pt "Nat → Bool")

let%test _ = type_check empty_ctx empty_store (p "(λx:Nat.0) 0") (pt "Nat")
let%test _ = type_check empty_ctx empty_store (p "(λx:Nat.succ x) 0") (pt "Nat")
let%test _ = type_check empty_ctx empty_store (p "(λx:Nat.0? x) 0") (pt "Bool")

let%test _ =
  type_check empty_ctx empty_store (p "(λx:Nat.ref x) 0") (pt "Ref Nat")

let%test _ =
  type_check empty_ctx empty_store (p "(λx:Ref Bool.!x) ref true") (pt "Bool")

let%test _ =
  not
    (type_check empty_ctx empty_store (p "(λx:Ref Bool.!x) ref 0") (pt "Bool"))

let%test _ = type_check empty_ctx empty_store (p "(λx:Nat.0) null") (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store (p "(λx:Nat.ref x) null") (pt "Ref Nat")

let%test _ = type_check empty_ctx empty_store (p "null 0") (pt "Ref Nat")
let%test _ = type_check empty_ctx empty_store (p "null null") (pt "Ref Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "null (raise x)") (pt "Ref Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "null (raise x)") (pt "Bool")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "(raise x) (raise x)") (pt "Bool")

let%test _ = type_check empty_ctx empty_store (p "unit") (pt "Unit")

let%test _ =
  type_check empty_ctx empty_store (p "some[Unit] unit") (pt "Option Unit")

let%test _ = type_check empty_ctx empty_store (p "ref unit") (pt "Ref Unit")
let%test _ = type_check empty_ctx empty_store (p "!ref unit") (pt "Unit")
let%test _ = type_check empty_ctx empty_store (p "ref unit := unit") (pt "Unit")
let%test _ = type_check empty_ctx empty_store (p "ref 0 := succ 0") (pt "Unit")
let%test _ = type_check empty_ctx empty_store (p "true") (pt "Bool")
let%test _ = type_check empty_ctx empty_store (p "false") (pt "Bool")
let%test _ = type_check empty_ctx empty_store (p "ref false") (pt "Ref Bool")

let%test _ =
  type_check empty_ctx empty_store (p "some[Bool] false") (pt "Option Bool")

let%test _ =
  type_check empty_ctx empty_store (p "if true then 0 else succ 0") (pt "Nat")

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p "if some[Bool] true then 0 else succ 0")
       (pt "Nat"))

let%test _ =
  type_check empty_ctx empty_store (p "if null then 0 else succ 0") (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store
    (p "if null then null else succ 0")
    (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store
    (p "if null then null else succ null")
    (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store (p "if null then null else null") (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store
    (p "if null then null else null")
    (pt "Ref Nat")

let%test _ =
  type_check empty_ctx empty_store
    (p "if null then null else null")
    (pt "Option Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "if (raise x) then null else null")
    (pt "Option Nat")

let%test _ = type_check empty_ctx empty_store (p "pred null") (pt "Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "pred (raise x)") (pt "Nat")

let%test _ =
  not (type_check empty_ctx empty_store (p "pred none[Nat]") (pt "Option Nat"))

let%test _ =
  not (type_check empty_ctx empty_store (p "pred (ref 0)") (pt "Ref Nat"))

let%test _ = type_check empty_ctx empty_store (p "pred !(ref 0)") (pt "Nat")
let%test _ = type_check empty_ctx empty_store (p "0? 0") (pt "Bool")
let%test _ = type_check empty_ctx empty_store (p "0? succ 0") (pt "Bool")
let%test _ = not (type_check empty_ctx empty_store (p "0? ref 0") (pt "Bool"))

let%test _ =
  type_check empty_ctx empty_store
    (p "r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat]")
    (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  type_check empty_ctx empty_store
    (p "r = null as [r:Nat;g:Nat;b:Nat]")
    (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p "r = true as [r:Nat;g:Nat;b:Nat]")
       (pt "[r:Nat;g:Nat;b:Nat]"))

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "r = (raise x) as [r:Nat;g:Nat;b:Nat]")
    (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p "g = succ 0 as [r:Nat;b:Nat]")
       (pt "[r:Nat;g:Nat;b:Nat]"))

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p "g = succ 0 as [r:Nat;b:Nat]")
       (pt "[r:Nat;b:Nat]"))

let%test _ =
  type_check empty_ctx empty_store
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    (pt "Bool")

let%test _ =
  type_check empty_ctx empty_store
    (p
       {|
    case null of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    (pt "Bool")

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p
          {|
    case g = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | b s => 0? s
    ]
|})
       (pt "Bool"))

let%test _ =
  type_check empty_ctx empty_store
    (p
       {|
    case g = null as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    (pt "Bool")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p
       {|
    case g = (raise x) as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
    (pt "Bool")

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p
          {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => succ s
      | b s => 0? s
    ]
|})
       (pt "Bool"))

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p
          {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => succ s
      | b s => 0? s
    ]
|})
       (pt "Nat"))

let%test _ =
  not
    (type_check empty_ctx empty_store
       (p
          {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => succ s
      | b s => ref s
    ]
|})
       (pt "Nat"))

let%test _ =
  (* one of the cases is missing *)
  not
    (type_check empty_ctx empty_store
       (p
          {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | b s => 0? s
    ]
|})
       (pt "Bool"))

let%test _ =
  type_check empty_ctx empty_store
    (p "ref (if false then 0 else succ 0)")
    (pt "Ref Nat")

let%test _ = type_check empty_ctx empty_store (p "ref null") (pt "Ref Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "ref (raise x)") (pt "Ref Nat")

let%test _ = type_check empty_ctx empty_store (p "!ref null") (pt "Bool")
let%test _ = type_check empty_ctx empty_store (p "!!ref null") (pt "Bool")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "!ref (raise x)") (pt "Bool")

let%test _ = not (type_check empty_ctx empty_store (p "!0") (pt "Nat"))

let%test _ =
  not (type_check empty_ctx empty_store (TmBang (TmLoc 0)) (pt "Bool"))

let%test _ =
  let s = IMap.add 0 TBool IMap.empty in
  type_check empty_ctx s (TmBang (TmLoc 0)) (pt "Bool")

let%test _ =
  type_check empty_ctx empty_store
    (TmApp (E.incc (), p "ref (succ 0)"))
    (pt "Unit")

let%test _ = type_check empty_ctx empty_store (p "null := false") (pt "Unit")
let%test _ = type_check empty_ctx empty_store (p "ref 0 := null") (pt "Unit")

let%test _ =
  not (type_check empty_ctx empty_store (p "ref true := 0") (pt "Unit"))

let%test _ =
  type_check empty_ctx empty_store (p "ref null := false") (pt "Unit")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "ref (raise x) := false")
    (pt "Unit")

let%test _ =
  let array = E.new_array_with [ 1; 5; 2 ] in
  type_check empty_ctx empty_store array (E.nat_array ())

let%test _ =
  type_check empty_ctx empty_store
    (p (spf {|
try 0 with 
    λe:%s.succ 0
|} (upt tp_exn)))
    (pt "Nat")

let%test _ =
  type_check empty_ctx empty_store
    (p (spf {|
try null with 
    λe:%s.succ 0
|} (upt tp_exn)))
    (pt "Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p (spf {|
try raise x with
    λe:%s.true
|} (upt tp_exn)))
    (pt "Bool")

let%test _ =
  not
    (type_check
       (extend empty_ctx "x" tp_exn)
       empty_store
       (p {|
try raise x with
    true
|})
       (pt "Bool"))

let%test _ =
  not (type_check empty_ctx empty_store (p {|
try 0 with succ 0
|}) (pt "Nat"))

let%test _ = type_check empty_ctx empty_store (p "null") (pt "!")
let%test _ = type_check empty_ctx empty_store (p "null") (pt "Nat")
let%test _ = type_check empty_ctx empty_store (p "null") (pt "Option Ref Nat")

let%test _ =
  type_check (extend empty_ctx "x" tp_exn) empty_store (p "raise x") (pt "!")

let%test _ =
  type_check (extend empty_ctx "x" tp_exn) empty_store (p "raise x") (pt "Nat")

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "raise x") (pt "Option Ref Nat")

let%test _ = type_check empty_ctx empty_store (p "null?[Nat] null") (pt "Bool")
let%test _ = type_check empty_ctx empty_store (p "null?[Nat] 0") (pt "Bool")

let%test _ =
  not (type_check empty_ctx empty_store (p "null?[Bool] 0") (pt "Bool"))

let%test _ =
  type_check
    (extend empty_ctx "x" tp_exn)
    empty_store (p "null?[Nat] (raise x)") (pt "Bool")

(************************ type_infer ************************)
let%test _ =
  type_infer (extend empty_ctx "x" TNat) empty_store (p "x") = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "λx:Nat.true") = Some (pt "Nat → Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "λx:Nat.x") = Some (pt "Nat → Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "λx:Nat.ref x")
  = Some (pt "Nat → Ref Nat")

let%test _ =
  not
    (type_infer empty_ctx empty_store (p "λx:Nat.ref x")
    = Some (pt "Nat → Ref Bool"))

let%test _ =
  type_infer empty_ctx empty_store (p "λx:Nat.null") = Some (pt "Nat → !")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.0) 0") = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.succ x) 0") = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.0? x) 0") = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.ref x) 0") = Some (pt "Ref Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Ref Bool.!x) ref true")
  = Some (pt "Bool")

let%test _ =
  not
    (type_infer empty_ctx empty_store (p "(λx:Ref Bool.!x) ref 0")
    = Some (pt "Bool"))

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.0) null") = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "(λx:Nat.ref x) null")
  = Some (pt "Ref Nat")

let%test _ = type_infer empty_ctx empty_store (p "null 0") = Some (pt "!")
let%test _ = type_infer empty_ctx empty_store (p "null null") = Some (pt "!")

let%test _ =
  type_infer (extend empty_ctx "x" tp_exn) empty_store (p "(raise x) (raise x)")
  = Some (pt "!")

let%test _ = type_infer empty_ctx empty_store (p "unit") = Some (pt "Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "some[Unit] unit")
  = Some (pt "Option Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "ref unit") = Some (pt "Ref Unit")

let%test _ = type_infer empty_ctx empty_store (p "!ref unit") = Some (pt "Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "ref unit := unit") = Some (pt "Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "ref 0 := succ 0") = Some (pt "Unit")

let%test _ = type_infer empty_ctx empty_store (p "true") = Some (pt "Bool")
let%test _ = type_infer empty_ctx empty_store (p "false") = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "ref false") = Some (pt "Ref Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "some[Bool] false")
  = Some (pt "Option Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "if true then 0 else succ 0")
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "if some[Bool] true then 0 else succ 0")
  = None

let%test _ =
  type_infer empty_ctx empty_store (p "if null then 0 else succ 0")
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "if null then null else succ 0")
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "if null then null else succ null")
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store (p "if null then null else null")
  = Some (pt "!")

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "if (raise x) then null else null")
  = Some (pt "!")

let%test _ = type_infer empty_ctx empty_store (p "pred null") = Some (pt "Nat")

let%test _ =
  type_infer (extend empty_ctx "x" tp_exn) empty_store (p "pred (raise x)")
  = Some (pt "Nat")

let%test _ = type_infer empty_ctx empty_store (p "pred none[Nat]") = None
let%test _ = type_infer empty_ctx empty_store (p "pred (ref 0)") = None

let%test _ =
  type_infer empty_ctx empty_store (p "pred !(ref 0)") = Some (pt "Nat")

let%test _ = type_infer empty_ctx empty_store (p "0? 0") = Some (pt "Bool")
let%test _ = type_infer empty_ctx empty_store (p "0? succ 0") = Some (pt "Bool")
let%test _ = type_infer empty_ctx empty_store (p "0? ref 0") = None

let%test _ =
  type_infer empty_ctx empty_store
    (p "r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat]")
  = Some (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  type_infer empty_ctx empty_store (p "r = null as [r:Nat;g:Nat;b:Nat]")
  = Some (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  type_infer empty_ctx empty_store (p "r = true as [r:Nat;g:Nat;b:Nat]") = None

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "r = (raise x) as [r:Nat;g:Nat;b:Nat]")
  = Some (pt "[r:Nat;g:Nat;b:Nat]")

let%test _ =
  type_infer empty_ctx empty_store (p "g = succ 0 as [r:Nat;b:Nat]") = None

let%test _ =
  type_infer empty_ctx empty_store (p "g = succ 0 as [r:Nat;b:Nat]") = None

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
  = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case null of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
  = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case g = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | b s => 0? s
    ]
|})
  = None

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case g = null as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
  = Some (pt "Bool")

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p
       {|
    case g = (raise x) as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => 0? s
      | b s => 0? s
    ]
|})
  = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => succ s
      | b s => 0? s
    ]
|})
  = None

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | g s => succ s
      | b s => ref s
    ]
|})
  = None

let%test _ =
  type_infer empty_ctx empty_store
    (p
       {|
    case r = succ succ succ 0 as [r:Nat;g:Nat;b:Nat] of [
        r s => 0? s
      | b s => 0? s
    ]
|})
  = None

let%test _ =
  type_infer empty_ctx empty_store (p "ref (if false then 0 else succ 0)")
  = Some (pt "Ref Nat")

let%test _ = type_infer empty_ctx empty_store (p "ref null") = Some (pt "Ref !")

let%test _ =
  type_infer (extend empty_ctx "x" tp_exn) empty_store (p "ref (raise x)")
  = Some (pt "Ref !")

let%test _ = type_infer empty_ctx empty_store (p "!ref null") = Some (pt "!")
let%test _ = type_infer empty_ctx empty_store (p "!!ref null") = Some (pt "!")

let%test _ =
  type_infer (extend empty_ctx "x" tp_exn) empty_store (p "!ref (raise x)")
  = Some (pt "!")

let%test _ = type_infer empty_ctx empty_store (p "!0") = None
let%test _ = type_infer empty_ctx empty_store (TmBang (TmLoc 0)) = None

let%test _ =
  let s = IMap.add 0 TBool IMap.empty in
  type_infer empty_ctx s (TmBang (TmLoc 0)) = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store (TmApp (E.incc (), p "ref (succ 0)"))
  = Some (pt "Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "null := false") = Some (pt "Unit")

let%test _ =
  type_infer empty_ctx empty_store (p "ref 0 := null") = Some (pt "Unit")

let%test _ = type_infer empty_ctx empty_store (p "ref true := 0") = None

let%test _ =
  type_infer empty_ctx empty_store (p "ref null := false") = Some (pt "Unit")

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p "ref (raise x) := false")
  = Some (pt "Unit")

let%test _ =
  let array = E.new_array_with [ 1; 5; 2 ] in
  type_infer empty_ctx empty_store array = Some (E.nat_array ())

let%test _ =
  type_infer empty_ctx empty_store
    (p (spf {|
try 0 with 
    λe:%s.succ 0
|} (upt tp_exn)))
  = Some (pt "Nat")

let%test _ =
  type_infer empty_ctx empty_store
    (p (spf {|
try null with 
    λe:%s.succ 0
|} (upt tp_exn)))
  = Some (pt "Nat")

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p (spf {|
try raise x with
    λe:%s.true
|} (upt tp_exn)))
  = Some (pt "Bool")

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store
    (p {|
try raise x with
    true
|})
  = None

let%test _ = type_infer empty_ctx empty_store (p {|
try 0 with succ 0
|}) = None

let%test _ = type_infer empty_ctx empty_store (p "null") = Some (pt "!")

let%test _ =
  type_infer (extend empty_ctx "x" tp_exn) empty_store (p "raise x")
  = Some (pt "!")

let%test _ =
  type_infer empty_ctx empty_store (p "null?[Nat] null") = Some (pt "Bool")

let%test _ =
  type_infer empty_ctx empty_store (p "null?[Nat] 0") = Some (pt "Bool")

let%test _ = type_infer empty_ctx empty_store (p "null?[Bool] 0") = None

let%test _ =
  type_infer
    (extend empty_ctx "x" tp_exn)
    empty_store (p "null?[Nat] (raise x)")
  = Some (pt "Bool")


(************************ store_well_typed ************************)
let%test _ = store_well_typed empty_ctx IMap.empty IMap.empty

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Nat"))
    IMap.(empty |> add 0 (p "0"))

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Bool"))
    IMap.(empty |> add 0 (p "true"))

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Nat → Nat"))
    IMap.(empty |> add 0 (p "λx:Nat.x"))

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Nat → Nat"))
    IMap.(empty |> add 0 (p "null"))

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Nat") |> add 1 (pt "Ref Nat"))
    IMap.(empty |> add 0 (p "0") |> add 1 (TmLoc 0))

let%test _ =
  not
    (store_well_typed empty_ctx
       IMap.(empty |> add 0 (pt "Nat") |> add 1 (pt "Ref Nat"))
       IMap.(empty |> add 0 (p "0") |> add 1 (TmLoc 1)))

let%test _ =
  not
    (store_well_typed empty_ctx
       IMap.(empty |> add 0 (pt "Bool") |> add 1 (pt "Ref Nat"))
       IMap.(empty |> add 0 (p "true") |> add 1 (TmLoc 0)))

let%test _ =
  store_well_typed empty_ctx
    IMap.(empty |> add 0 (pt "Option Bool"))
    IMap.(empty |> add 0 (p "none[Bool]"))
