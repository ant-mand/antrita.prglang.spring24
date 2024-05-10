open Lang
open Etlc

let p = Parse.parse
let pt = Parse.parse_typ
let up = Unparse.unparse
let upt = Unparse.unparse_typ
let spf = Printf.sprintf

exception NotPossible

(** [example_value typ] generates an example [term] of type [typ] *)
let rec example_value = function
  | TNat -> TmZero
  | TBool -> TmFalse
  | TUnit -> TmUnit
  | TFun (t1, t2) -> TmAbs ("x", t1, example_value t2)
  | TRef ty0 -> TmRef (example_value ty0)
  | TVariant vars as ty -> (
      match vars with
      | [] -> raise NotPossible
      | (lbl, ty0) :: _ -> TmVariant (lbl, example_value ty0, ty))
  | TBot -> raise NotPossible

(** [is_err term] return whether [term] is a valid error (i.e. a raised exception) *)
let is_err = function
  | TmRaise (TmVariant _ as t) -> type_check empty_ctx IMap.empty t tp_exn
  | _ -> false

(** [cbv_n n t mu] tries to step [t] [n] times, returning the resultant [term] and the modified [mu] *)
let rec cbv_n (n : int) (t : term) (mu : store) : (term * store) option =
  if n <= 0 then Some (t, mu)
  else
    match cbv t mu with Some (t', mu') -> cbv_n (n - 1) t' mu' | None -> None

(** [multistep_list t mu] steps the given [term] until it does not step, returning a list containing all the intermediate steps and the modified term store [mu'] *)
let rec multistep_list (t : term) (mu : store) : term list * store =
  match cbv t mu with
  | Some (t', mu') ->
      let tl, mu'' = multistep_list t' mu' in
      (t' :: tl, mu'')
  | None -> ([ t ], mu)
