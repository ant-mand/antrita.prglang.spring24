open Util
open Etlc

(** This term unwraps an optional [Nat], returning [0] if 
    the [term] is `none\[Nat\]` *)
let unwrap () =
  p {|
λol:Option Nat.
  case ol of [
     some l => l
   | none _ => 0
  ]
|}

let weekday () =
  pt
    {|
  [monday:Unit; 
   tuesday:Unit;
   wednesday:Unit;
   thursday:Unit;
   friday:Unit;
   saturday:Unit;
   sunday:Unit]
|}

let next_business_day () =
  let weekday = upt (weekday ()) in
  p
    (spf
       {|λw:%s.
      case w of [
          monday    _ => tuesday   = unit as %s
        | tuesday   _ => wednesday = unit as %s
        | wednesday _ => thursday  = unit as %s
        | thursday  _ => friday    = unit as %s
        | friday    _ => monday    = unit as %s
        | saturday  _ => monday    = unit as %s
        | sunday    _ => monday    = unit as %s
      ]
    |}
       weekday weekday weekday weekday weekday weekday weekday weekday)

let incc () = p "λx:Ref Nat.(x := succ !x)"
let nat_array () = pt "Ref (Nat → Nat)"
let newarray () = p "λ_x:Unit.ref (λ_n:Nat.0)"
let lookup () = p (spf "λa:%s.λn:Nat.(!a n)" (upt (nat_array ())))

(* because we don't have general recursion,
   I cannot think of a way to define general
   Nat equality(?). We can use the host language however
   to generate checks up to a certain limit. *)
let update n =
  let nat_array = upt (nat_array ()) in
  let rec repeat n s = if n <= 0 then "" else s ^ repeat (n - 1) s in
  let minus n =
    p
      ("λu:Nat.λv:Nat."
      ^ String.concat ""
          (List.init n (fun i ->
               let preds = repeat i "pred " in
               spf "if (0? %s v) then %s u else " preds preds))
      ^ "0")
  in
  let equal n =
    let minus = up (minus n) in
    p
      (spf "λx:Nat.λy:Nat.if (0? ((%s) x y)) then (0? ((%s) y x)) else false"
         minus minus)
  in
  p
    (spf
       {|
    λa:%s.λm:Nat.λs:Nat.
      ((λoldf:Nat → Nat.(a := (λn:Nat. if (%s) m n then s else (oldf n)))) !a)
    |}
       nat_array
       (up (equal n)))

(** [new_array_with nats] generates a term that steps a constructed 
    array updated so that it refelects the OCaml list [nats]
    in terms of its indices and the values at those indices. *)
let new_array_with (nats : int list) : term =
  let rec to_term n = if n <= 0 then TmZero else TmSucc (to_term (n - 1)) in
  let update = up (update (List.length nats + 1)) in
  let newarray = up (newarray ()) in
  let nat_array = upt (nat_array ()) in
  p
    (spf "(λarray:%s.(%s)) ((%s) unit)" nat_array
       (List.fold_right
          (fun (n, i) acc ->
            spf "(λ_u%d:Unit.%s) ((%s) array %s %s)" i acc update
              (up (to_term i))
              (up (to_term n)))
          (List.mapi (fun i n -> (n, i)) nats)
          "array")
       newarray)
