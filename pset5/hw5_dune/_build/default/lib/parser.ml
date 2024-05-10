
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | ZERO
    | VAR of (
# 10 "lib/parser.mly"
       (string)
# 16 "lib/parser.ml"
  )
    | UNITT
    | UNIT
    | UNDERSCORE
    | TWO
    | TRUE
    | TIMES
    | THEN
    | TAIL
    | SUCC
    | SOME
    | SMALL_OPTION
    | SEMICOLON
    | RP
    | RCB
    | RB
    | PRED
    | PLUS
    | PIPE
    | OPTION
    | ONE
    | OF
    | NONE
    | NIL
    | NAT
    | LP
    | LIST
    | LETREC
    | LET
    | LCB
    | LB
    | LAM
    | ISZERO
    | ISNIL
    | INR
    | INL
    | IN
    | IF
    | HEAD
    | FIX
    | FATARROW
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DOT
    | CONS
    | COMMA
    | COLON
    | CASE
    | BOOL
    | AS
    | ARROW
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
open Etlc

# 79 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_prog) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState006 : (('s, _menhir_box_prog) _menhir_cell1_TAIL, _menhir_box_prog) _menhir_state
    (** State 006.
        Stack shape : TAIL.
        Start symbol: prog. *)

  | MenhirState008 : (('s, 'r) _menhir_cell1_OPTION, 'r) _menhir_state
    (** State 008.
        Stack shape : OPTION.
        Start symbol: <undetermined>. *)

  | MenhirState010 : (('s, 'r) _menhir_cell1_LP, 'r) _menhir_state
    (** State 010.
        Stack shape : LP.
        Start symbol: <undetermined>. *)

  | MenhirState011 : (('s, 'r) _menhir_cell1_LIST, 'r) _menhir_state
    (** State 011.
        Stack shape : LIST.
        Start symbol: <undetermined>. *)

  | MenhirState017 : (('s, 'r) _menhir_cell1_sum_typ, 'r) _menhir_state
    (** State 017.
        Stack shape : sum_typ.
        Start symbol: <undetermined>. *)

  | MenhirState019 : (('s, 'r) _menhir_cell1_prod_typ, 'r) _menhir_state
    (** State 019.
        Stack shape : prod_typ.
        Start symbol: <undetermined>. *)

  | MenhirState022 : (('s, 'r) _menhir_cell1_sum_typ, 'r) _menhir_state
    (** State 022.
        Stack shape : sum_typ.
        Start symbol: <undetermined>. *)

  | MenhirState028 : ((('s, _menhir_box_prog) _menhir_cell1_TAIL, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 028.
        Stack shape : TAIL typ.
        Start symbol: prog. *)

  | MenhirState029 : (('s, _menhir_box_prog) _menhir_cell1_SUCC, _menhir_box_prog) _menhir_state
    (** State 029.
        Stack shape : SUCC.
        Start symbol: prog. *)

  | MenhirState033 : (('s, _menhir_box_prog) _menhir_cell1_SOME, _menhir_box_prog) _menhir_state
    (** State 033.
        Stack shape : SOME.
        Start symbol: prog. *)

  | MenhirState034 : (('s, _menhir_box_prog) _menhir_cell1_PRED, _menhir_box_prog) _menhir_state
    (** State 034.
        Stack shape : PRED.
        Start symbol: prog. *)

  | MenhirState036 : (('s, _menhir_box_prog) _menhir_cell1_NONE, _menhir_box_prog) _menhir_state
    (** State 036.
        Stack shape : NONE.
        Start symbol: prog. *)

  | MenhirState040 : (('s, _menhir_box_prog) _menhir_cell1_NIL, _menhir_box_prog) _menhir_state
    (** State 040.
        Stack shape : NIL.
        Start symbol: prog. *)

  | MenhirState043 : (('s, _menhir_box_prog) _menhir_cell1_LP, _menhir_box_prog) _menhir_state
    (** State 043.
        Stack shape : LP.
        Start symbol: prog. *)

  | MenhirState045 : (('s, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_state
    (** State 045.
        Stack shape : SMALL_OPTION.
        Start symbol: prog. *)

  | MenhirState048 : (('s, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 048.
        Stack shape : LETREC VAR.
        Start symbol: prog. *)

  | MenhirState050 : ((('s, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 050.
        Stack shape : LETREC VAR typ.
        Start symbol: prog. *)

  | MenhirState053 : (('s, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 053.
        Stack shape : LET VAR.
        Start symbol: prog. *)

  | MenhirState055 : ((('s, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 055.
        Stack shape : LET VAR typ.
        Start symbol: prog. *)

  | MenhirState056 : (('s, _menhir_box_prog) _menhir_cell1_LCB, _menhir_box_prog) _menhir_state
    (** State 056.
        Stack shape : LCB.
        Start symbol: prog. *)

  | MenhirState059 : (('s, _menhir_box_prog) _menhir_cell1_LAM _menhir_cell0_VAR, _menhir_box_prog) _menhir_state
    (** State 059.
        Stack shape : LAM VAR.
        Start symbol: prog. *)

  | MenhirState061 : ((('s, _menhir_box_prog) _menhir_cell1_LAM _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 061.
        Stack shape : LAM VAR typ.
        Start symbol: prog. *)

  | MenhirState062 : (('s, _menhir_box_prog) _menhir_cell1_ISZERO, _menhir_box_prog) _menhir_state
    (** State 062.
        Stack shape : ISZERO.
        Start symbol: prog. *)

  | MenhirState064 : (('s, _menhir_box_prog) _menhir_cell1_ISNIL, _menhir_box_prog) _menhir_state
    (** State 064.
        Stack shape : ISNIL.
        Start symbol: prog. *)

  | MenhirState066 : ((('s, _menhir_box_prog) _menhir_cell1_ISNIL, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 066.
        Stack shape : ISNIL typ.
        Start symbol: prog. *)

  | MenhirState067 : (('s, _menhir_box_prog) _menhir_cell1_INR, _menhir_box_prog) _menhir_state
    (** State 067.
        Stack shape : INR.
        Start symbol: prog. *)

  | MenhirState068 : (('s, _menhir_box_prog) _menhir_cell1_INL, _menhir_box_prog) _menhir_state
    (** State 068.
        Stack shape : INL.
        Start symbol: prog. *)

  | MenhirState070 : (('s, _menhir_box_prog) _menhir_cell1_HEAD, _menhir_box_prog) _menhir_state
    (** State 070.
        Stack shape : HEAD.
        Start symbol: prog. *)

  | MenhirState072 : ((('s, _menhir_box_prog) _menhir_cell1_HEAD, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 072.
        Stack shape : HEAD typ.
        Start symbol: prog. *)

  | MenhirState073 : (('s, _menhir_box_prog) _menhir_cell1_FIX, _menhir_box_prog) _menhir_state
    (** State 073.
        Stack shape : FIX.
        Start symbol: prog. *)

  | MenhirState076 : (('s, _menhir_box_prog) _menhir_cell1_CONS, _menhir_box_prog) _menhir_state
    (** State 076.
        Stack shape : CONS.
        Start symbol: prog. *)

  | MenhirState078 : ((('s, _menhir_box_prog) _menhir_cell1_CONS, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_state
    (** State 078.
        Stack shape : CONS typ.
        Start symbol: prog. *)

  | MenhirState083 : (((('s, _menhir_box_prog) _menhir_cell1_CONS, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_cons_expr, _menhir_box_prog) _menhir_state
    (** State 083.
        Stack shape : CONS typ cons_expr.
        Start symbol: prog. *)

  | MenhirState089 : ((('s, _menhir_box_prog) _menhir_cell1_INL, _menhir_box_prog) _menhir_cell1_cons_expr, _menhir_box_prog) _menhir_state
    (** State 089.
        Stack shape : INL cons_expr.
        Start symbol: prog. *)

  | MenhirState092 : ((('s, _menhir_box_prog) _menhir_cell1_INR, _menhir_box_prog) _menhir_cell1_cons_expr, _menhir_box_prog) _menhir_state
    (** State 092.
        Stack shape : INR cons_expr.
        Start symbol: prog. *)

  | MenhirState096 : (('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_state
    (** State 096.
        Stack shape : IF.
        Start symbol: prog. *)

  | MenhirState097 : (('s, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_state
    (** State 097.
        Stack shape : CASE.
        Start symbol: prog. *)

  | MenhirState099 : (('s, _menhir_box_prog) _menhir_cell1_seq_expr, _menhir_box_prog) _menhir_state
    (** State 099.
        Stack shape : seq_expr.
        Start symbol: prog. *)

  | MenhirState102 : (('s, _menhir_box_prog) _menhir_cell1_ascr_expr, _menhir_box_prog) _menhir_state
    (** State 102.
        Stack shape : ascr_expr.
        Start symbol: prog. *)

  | MenhirState104 : (('s, _menhir_box_prog) _menhir_cell1_app_expr, _menhir_box_prog) _menhir_state
    (** State 104.
        Stack shape : app_expr.
        Start symbol: prog. *)

  | MenhirState109 : ((('s, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 109.
        Stack shape : CASE expr.
        Start symbol: prog. *)

  | MenhirState113 : (((('s, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_state
    (** State 113.
        Stack shape : CASE expr pat.
        Start symbol: prog. *)

  | MenhirState116 : ((((('s, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 116.
        Stack shape : CASE expr pat expr.
        Start symbol: prog. *)

  | MenhirState118 : (((((('s, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_state
    (** State 118.
        Stack shape : CASE expr pat expr pat.
        Start symbol: prog. *)

  | MenhirState123 : ((('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 123.
        Stack shape : IF expr.
        Start symbol: prog. *)

  | MenhirState125 : (((('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 125.
        Stack shape : IF expr expr.
        Start symbol: prog. *)

  | MenhirState129 : ((('s, _menhir_box_prog) _menhir_cell1_LCB, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 129.
        Stack shape : LCB expr.
        Start symbol: prog. *)

  | MenhirState133 : (((('s, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 133.
        Stack shape : LET VAR typ expr.
        Start symbol: prog. *)

  | MenhirState136 : (((('s, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 136.
        Stack shape : LETREC VAR typ expr.
        Start symbol: prog. *)

  | MenhirState140 : ((('s, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 140.
        Stack shape : SMALL_OPTION expr.
        Start symbol: prog. *)

  | MenhirState142 : (((('s, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_state
    (** State 142.
        Stack shape : SMALL_OPTION expr pat.
        Start symbol: prog. *)

  | MenhirState146 : ((((('s, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 146.
        Stack shape : SMALL_OPTION expr pat expr.
        Start symbol: prog. *)

  | MenhirState157 : ('s, _menhir_box_ptyp) _menhir_state
    (** State 157.
        Stack shape : .
        Start symbol: ptyp. *)


and ('s, 'r) _menhir_cell1_app_expr = 
  | MenhirCell1_app_expr of 's * ('s, 'r) _menhir_state * (Etlc.term)

and ('s, 'r) _menhir_cell1_ascr_expr = 
  | MenhirCell1_ascr_expr of 's * ('s, 'r) _menhir_state * (Etlc.term)

and ('s, 'r) _menhir_cell1_cons_expr = 
  | MenhirCell1_cons_expr of 's * ('s, 'r) _menhir_state * (Etlc.term)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Etlc.term)

and ('s, 'r) _menhir_cell1_pat = 
  | MenhirCell1_pat of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_prod_typ = 
  | MenhirCell1_prod_typ of 's * ('s, 'r) _menhir_state * (Etlc.typ)

and ('s, 'r) _menhir_cell1_seq_expr = 
  | MenhirCell1_seq_expr of 's * ('s, 'r) _menhir_state * (Etlc.term)

and ('s, 'r) _menhir_cell1_sum_typ = 
  | MenhirCell1_sum_typ of 's * ('s, 'r) _menhir_state * (Etlc.typ)

and ('s, 'r) _menhir_cell1_typ = 
  | MenhirCell1_typ of 's * ('s, 'r) _menhir_state * (Etlc.typ)

and ('s, 'r) _menhir_cell1_CASE = 
  | MenhirCell1_CASE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CONS = 
  | MenhirCell1_CONS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FIX = 
  | MenhirCell1_FIX of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_HEAD = 
  | MenhirCell1_HEAD of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INL = 
  | MenhirCell1_INL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INR = 
  | MenhirCell1_INR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ISNIL = 
  | MenhirCell1_ISNIL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ISZERO = 
  | MenhirCell1_ISZERO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LAM = 
  | MenhirCell1_LAM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LCB = 
  | MenhirCell1_LCB of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LETREC = 
  | MenhirCell1_LETREC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LIST = 
  | MenhirCell1_LIST of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LP = 
  | MenhirCell1_LP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NIL = 
  | MenhirCell1_NIL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NONE = 
  | MenhirCell1_NONE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OPTION = 
  | MenhirCell1_OPTION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRED = 
  | MenhirCell1_PRED of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SMALL_OPTION = 
  | MenhirCell1_SMALL_OPTION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SOME = 
  | MenhirCell1_SOME of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SUCC = 
  | MenhirCell1_SUCC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TAIL = 
  | MenhirCell1_TAIL of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_VAR = 
  | MenhirCell0_VAR of 's * (
# 10 "lib/parser.mly"
       (string)
# 453 "lib/parser.ml"
)

and _menhir_box_ptyp = 
  | MenhirBox_ptyp of (Etlc.typ) [@@unboxed]

and _menhir_box_prog = 
  | MenhirBox_prog of (Etlc.term) [@@unboxed]

let _menhir_action_02 =
  fun _1 ->
    (
# 124 "lib/parser.mly"
                ( _1 )
# 467 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_03 =
  fun e1 e2 ->
    (
# 126 "lib/parser.mly"
        ( TmApp (e1, e2) )
# 475 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_04 =
  fun _1 ->
    (
# 118 "lib/parser.mly"
               ( _1 )
# 483 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_05 =
  fun e tp ->
    (
# 120 "lib/parser.mly"
        ( tm_ascribe e tp )
# 491 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_06 =
  fun e ->
    (
# 158 "lib/parser.mly"
                       ( e )
# 499 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_07 =
  fun e1 e2 ->
    (
# 160 "lib/parser.mly"
        ( TmPair (e1, e2) )
# 507 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_08 =
  fun x ->
    (
# 161 "lib/parser.mly"
              ( TmVar x )
# 515 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_09 =
  fun () ->
    (
# 162 "lib/parser.mly"
           ( TmTrue )
# 523 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_10 =
  fun () ->
    (
# 163 "lib/parser.mly"
            ( TmFalse )
# 531 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_11 =
  fun () ->
    (
# 164 "lib/parser.mly"
           ( TmZero )
# 539 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_12 =
  fun () ->
    (
# 165 "lib/parser.mly"
           ( TmUnit )
# 547 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_13 =
  fun tp ->
    (
# 166 "lib/parser.mly"
                             ( tm_none (tp_opt tp) )
# 555 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_14 =
  fun tp ->
    (
# 167 "lib/parser.mly"
                            ( TmNil tp )
# 563 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_15 =
  fun () ->
    (
# 57 "lib/parser.mly"
            ( TUnit )
# 571 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_16 =
  fun () ->
    (
# 58 "lib/parser.mly"
           ( TBool )
# 579 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_17 =
  fun () ->
    (
# 59 "lib/parser.mly"
          ( TNat )
# 587 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_18 =
  fun t ->
    (
# 60 "lib/parser.mly"
                           ( TList t )
# 595 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_19 =
  fun t ->
    (
# 61 "lib/parser.mly"
                             ( tp_opt t )
# 603 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_20 =
  fun t ->
    (
# 62 "lib/parser.mly"
                      ( t )
# 611 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_21 =
  fun e1 e2 e3 ->
    (
# 103 "lib/parser.mly"
                                                      ( TmIf (e1, e2, e3) )
# 619 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_22 =
  fun e e1 e2 x1 x2 ->
    (
# 105 "lib/parser.mly"
        ( TmCase (e, x1, e1, x2, e2) )
# 627 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_23 =
  fun e e1 e2 x ->
    (
# 107 "lib/parser.mly"
        ( tm_opt_case e x e1 e2 )
# 635 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_24 =
  fun _1 ->
    (
# 108 "lib/parser.mly"
               ( _1 )
# 643 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_25 =
  fun _1 ->
    (
# 130 "lib/parser.mly"
                ( _1 )
# 651 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_26 =
  fun e ->
    (
# 131 "lib/parser.mly"
                          ( TmSucc e )
# 659 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_27 =
  fun e ->
    (
# 132 "lib/parser.mly"
                          ( TmPred e )
# 667 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_28 =
  fun e ->
    (
# 133 "lib/parser.mly"
                            ( TmIsZero e )
# 675 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_29 =
  fun e tp ->
    (
# 134 "lib/parser.mly"
                                       ( TmInl (tp, e) )
# 683 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_30 =
  fun e tp ->
    (
# 135 "lib/parser.mly"
                                       ( TmInr (tp, e) )
# 691 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_31 =
  fun e ->
    (
# 136 "lib/parser.mly"
                        ( TmFix e )
# 699 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_32 =
  fun e tp ->
    (
# 138 "lib/parser.mly"
        ( TmHead (tp, e) )
# 707 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_33 =
  fun e tp ->
    (
# 140 "lib/parser.mly"
        ( TmTail (tp, e) )
# 715 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_34 =
  fun e1 e2 tp ->
    (
# 142 "lib/parser.mly"
        ( TmCons (tp, e1, e2) )
# 723 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_35 =
  fun e tp ->
    (
# 144 "lib/parser.mly"
        ( TmIsNil (tp, e) )
# 731 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_36 =
  fun e ->
    (
# 146 "lib/parser.mly"
        ( tm_some e )
# 739 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_37 =
  fun _1 ->
    (
# 85 "lib/parser.mly"
                  ( _1 )
# 747 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_38 =
  fun e1 e2 tp x ->
    (
# 87 "lib/parser.mly"
        ( tm_let x tp e1 e2 )
# 755 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_39 =
  fun e1 e2 tp x ->
    (
# 89 "lib/parser.mly"
        ( tm_letrec x tp e1 e2 )
# 763 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_40 =
  fun _1 ->
    (
# 66 "lib/parser.mly"
              ( _1 )
# 771 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_41 =
  fun t1 t2 ->
    (
# 68 "lib/parser.mly"
        ( TFun (t1, t2) )
# 779 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_42 =
  fun _1 ->
    (
# 93 "lib/parser.mly"
                ( _1 )
# 787 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_43 =
  fun e t x ->
    (
# 94 "lib/parser.mly"
                                                         ( TmAbs (x, t, e) )
# 795 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_44 =
  fun _1 ->
    (
# 98 "lib/parser.mly"
          ( _1 )
# 803 "lib/parser.ml"
     : (string))

let _menhir_action_45 =
  fun () ->
    (
# 99 "lib/parser.mly"
                 ( "_" )
# 811 "lib/parser.ml"
     : (string))

let _menhir_action_46 =
  fun _1 ->
    (
# 78 "lib/parser.mly"
                 ( _1 )
# 819 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_47 =
  fun t1 t2 ->
    (
# 80 "lib/parser.mly"
        ( TProd (t1, t2) )
# 827 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_48 =
  fun e ->
    (
# 45 "lib/parser.mly"
                    ( e )
# 835 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_49 =
  fun _1 ->
    (
# 150 "lib/parser.mly"
                  ( _1 )
# 843 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_50 =
  fun e ->
    (
# 152 "lib/parser.mly"
        ( TmFst e )
# 851 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_51 =
  fun e ->
    (
# 154 "lib/parser.mly"
        ( TmSnd e )
# 859 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_52 =
  fun tp ->
    (
# 49 "lib/parser.mly"
                    ( tp )
# 867 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_53 =
  fun _1 ->
    (
# 112 "lib/parser.mly"
                ( _1 )
# 875 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_54 =
  fun e1 e2 ->
    (
# 114 "lib/parser.mly"
        ( tm_seq e1 e2 )
# 883 "lib/parser.ml"
     : (Etlc.term))

let _menhir_action_55 =
  fun _1 ->
    (
# 72 "lib/parser.mly"
               ( _1 )
# 891 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_56 =
  fun t1 t2 ->
    (
# 74 "lib/parser.mly"
        ( TSum (t1, t2) )
# 899 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_action_57 =
  fun _1 ->
    (
# 53 "lib/parser.mly"
              ( _1 )
# 907 "lib/parser.ml"
     : (Etlc.typ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARROW ->
        "ARROW"
    | AS ->
        "AS"
    | BOOL ->
        "BOOL"
    | CASE ->
        "CASE"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | CONS ->
        "CONS"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FATARROW ->
        "FATARROW"
    | FIX ->
        "FIX"
    | HEAD ->
        "HEAD"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INL ->
        "INL"
    | INR ->
        "INR"
    | ISNIL ->
        "ISNIL"
    | ISZERO ->
        "ISZERO"
    | LAM ->
        "LAM"
    | LB ->
        "LB"
    | LCB ->
        "LCB"
    | LET ->
        "LET"
    | LETREC ->
        "LETREC"
    | LIST ->
        "LIST"
    | LP ->
        "LP"
    | NAT ->
        "NAT"
    | NIL ->
        "NIL"
    | NONE ->
        "NONE"
    | OF ->
        "OF"
    | ONE ->
        "ONE"
    | OPTION ->
        "OPTION"
    | PIPE ->
        "PIPE"
    | PLUS ->
        "PLUS"
    | PRED ->
        "PRED"
    | RB ->
        "RB"
    | RCB ->
        "RCB"
    | RP ->
        "RP"
    | SEMICOLON ->
        "SEMICOLON"
    | SMALL_OPTION ->
        "SMALL_OPTION"
    | SOME ->
        "SOME"
    | SUCC ->
        "SUCC"
    | TAIL ->
        "TAIL"
    | THEN ->
        "THEN"
    | TIMES ->
        "TIMES"
    | TRUE ->
        "TRUE"
    | TWO ->
        "TWO"
    | UNDERSCORE ->
        "UNDERSCORE"
    | UNIT ->
        "UNIT"
    | UNITT ->
        "UNITT"
    | VAR _ ->
        "VAR"
    | ZERO ->
        "ZERO"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_158 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ptyp =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let tp = _v in
          let _v = _menhir_action_52 tp in
          MenhirBox_ptyp _v
      | _ ->
          _eRR ()
  
  let _menhir_run_155 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let e = _v in
          let _v = _menhir_action_48 e in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atomic_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_49 _1 in
      _menhir_goto_proj_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_proj_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TWO ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let e = _v in
              let _v = _menhir_action_51 e in
              _menhir_goto_proj_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | ONE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let e = _v in
              let _v = _menhir_action_50 e in
              _menhir_goto_proj_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | AS | COMMA | CONS | ELSE | EOF | FALSE | FIX | HEAD | IN | INL | INR | ISNIL | ISZERO | LCB | LP | NIL | NONE | OF | PIPE | PRED | RCB | RP | SEMICOLON | SOME | SUCC | TAIL | THEN | TRUE | UNIT | VAR _ | ZERO ->
          let _1 = _v in
          let _v = _menhir_action_25 _1 in
          _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_cons_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState028 ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState029 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState133 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState066 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState067 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState078 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_153 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TAIL, _menhir_box_prog) _menhir_cell1_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell1_TAIL (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_33 e tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_152 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_SUCC -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SUCC (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_26 e in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_151 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_SOME -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SOME (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_36 e in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_150 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_PRED -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PRED (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_27 e in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_app_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_app_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_03 e1 e2 in
      _menhir_goto_app_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_app_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | VAR _v_0 ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState104
      | UNIT ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | TRUE ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | TAIL ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | SUCC ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | SOME ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | PRED ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | NONE ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | NIL ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | LP ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | LCB ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | ISZERO ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | ISNIL ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | INR ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | INL ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | HEAD ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | FIX ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | FALSE ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | CONS ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | AS | COMMA | ELSE | EOF | IN | OF | PIPE | RCB | RP | SEMICOLON | THEN ->
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_goto_ascr_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_08 x in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_09 () in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TAIL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState006 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_15 () in
      _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atomic_typ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState008 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState157 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState011 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_026 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_OPTION -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_OPTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_19 t in
      _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_021 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_46 _1 in
      _menhir_goto_prod_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_prod_typ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState157 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_023 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_prod_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW | AS | COMMA | CONS | DOT | ELSE | EOF | EQ | FALSE | FIX | HEAD | IN | INL | INR | ISNIL | ISZERO | LCB | LP | NIL | NONE | OF | PIPE | PLUS | PRED | RB | RCB | RP | SEMICOLON | SOME | SUCC | TAIL | THEN | TRUE | UNIT | VAR _ | ZERO ->
          let _1 = _v in
          let _v = _menhir_action_55 _1 in
          _menhir_goto_sum_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_prod_typ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState019 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPTION (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState008 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_009 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_17 () in
      _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_010 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LP (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState010 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_011 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LIST (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState011 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_16 () in
      _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_sum_typ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_sum_typ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState017 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_stack = MenhirCell1_sum_typ (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState022 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AS | COMMA | CONS | DOT | ELSE | EOF | EQ | FALSE | FIX | HEAD | IN | INL | INR | ISNIL | ISZERO | LCB | LP | NIL | NONE | OF | PIPE | PRED | RB | RCB | RP | SEMICOLON | SOME | SUCC | TAIL | THEN | TRUE | UNIT | VAR _ | ZERO ->
          let _1 = _v in
          let _v = _menhir_action_40 _1 in
          _menhir_goto_fun_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_fun_typ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState157 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState102 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_025 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_57 _1 in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState157 ->
          _menhir_run_158 _menhir_stack _v _tok
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState036 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState006 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ascr_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ascr_expr (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let tp = _v in
      let _v = _menhir_action_05 e tp in
      _menhir_goto_ascr_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ascr_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState133 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_121 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AS ->
          let _menhir_stack = MenhirCell1_ascr_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | ELSE | EOF | IN | OF | PIPE | RCB | RP | SEMICOLON | THEN ->
          let _1 = _v in
          let _v = _menhir_action_53 _1 in
          _menhir_goto_seq_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ascr_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState102 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_seq_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_seq_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState099 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | ELSE | EOF | IN | OF | PIPE | RCB | RP | THEN ->
          let _1 = _v in
          let _v = _menhir_action_24 _1 in
          _menhir_goto_body_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SUCC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SOME (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NAT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RB ->
                  let _menhir_s = MenhirState033 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | ZERO ->
                      _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | UNIT ->
                      _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | TRUE ->
                      _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | TAIL ->
                      _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SUCC ->
                      _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SOME ->
                      _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRED ->
                      _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NONE ->
                      _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NIL ->
                      _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LP ->
                      _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LCB ->
                      _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | ISZERO ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | ISNIL ->
                      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INR ->
                      _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INL ->
                      _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | HEAD ->
                      _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FIX ->
                      _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FALSE ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CONS ->
                      _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRED (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState034 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NONE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState036 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NIL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState040 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LP (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState043 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SMALL_OPTION ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LETREC ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAM ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SMALL_OPTION (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | CASE ->
          let _menhir_s = MenhirState045 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LETREC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState048 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UNITT ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPTION ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NAT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LP ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LIST ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState053 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UNITT ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPTION ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NAT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LP ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LIST ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LCB (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState056 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SMALL_OPTION ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LETREC ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAM ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAM (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _menhir_stack = MenhirCell0_VAR (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState059 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UNITT ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | OPTION ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NAT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LP ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LIST ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BOOL ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ISZERO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState062 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ISNIL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState064 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState067 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState068 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_HEAD (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState070 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FIX (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_10 () in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CONS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LB ->
          let _menhir_s = MenhirState076 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState096 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SMALL_OPTION ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LETREC ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAM ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SMALL_OPTION ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LETREC ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAM ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_body_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_42 _1 in
      _menhir_goto_lambda_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_lambda_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState061 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState133 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_127 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LAM _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ (_menhir_stack, _, t) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, x) = _menhir_stack in
      let MenhirCell1_LAM (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_43 e t x in
      _menhir_goto_lambda_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_106 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_37 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_155 _menhir_stack _v _tok
      | MenhirState043 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState050 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState133 ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState056 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_148 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LP -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LP (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_06 e in
          _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_pat (_menhir_stack, _, x) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_SMALL_OPTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_23 e e1 e2 x in
      _menhir_goto_body_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_143 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NONE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | FATARROW ->
                  let _menhir_s = MenhirState146 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | ZERO ->
                      _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | VAR _v ->
                      _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | UNIT ->
                      _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | TRUE ->
                      _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | TAIL ->
                      _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SUCC ->
                      _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SOME ->
                      _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | SMALL_OPTION ->
                      _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | PRED ->
                      _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NONE ->
                      _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | NIL ->
                      _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LP ->
                      _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LETREC ->
                      _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LET ->
                      _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LCB ->
                      _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LAM ->
                      _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | ISZERO ->
                      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | ISNIL ->
                      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INR ->
                      _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INL ->
                      _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IF ->
                      _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | HEAD ->
                      _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FIX ->
                      _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | FALSE ->
                      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CONS ->
                      _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | CASE ->
                      _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SMALL_OPTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SOME ->
              let _menhir_s = MenhirState140 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNDERSCORE ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_44 _1 in
      _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_pat : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState140 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_141 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_SMALL_OPTION, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | FATARROW ->
          let _menhir_s = MenhirState142 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | FATARROW ->
          let _menhir_s = MenhirState118 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pat (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | FATARROW ->
          let _menhir_s = MenhirState113 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_45 () in
      _menhir_goto_pat _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_137 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, x) = _menhir_stack in
      let MenhirCell1_LETREC (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_39 e1 e2 tp x in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_135 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell0_VAR (_menhir_stack, x) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_38 e1 e2 tp x in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_132 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR, _menhir_box_prog) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState133 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LCB, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RCB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_LCB (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_07 e1 e2 in
          _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LCB as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState129 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, e2) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let e3 = _v in
      let _v = _menhir_action_21 e1 e2 e3 in
      _menhir_goto_body_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState125 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState123 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_119 : type  ttv_stack. (((((ttv_stack, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_pat (_menhir_stack, _, x2) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_pat (_menhir_stack, _, x1) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_CASE (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_22 e e1 e2 x1 x2 in
      _menhir_goto_body_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_114 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_CASE, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_cell1_pat as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INR ->
              let _menhir_s = MenhirState116 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNDERSCORE ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_CASE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INL ->
              let _menhir_s = MenhirState109 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | UNDERSCORE ->
                  _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_seq_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AS ->
          let _menhir_stack = MenhirCell1_ascr_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | ELSE | EOF | IN | OF | PIPE | RCB | RP | SEMICOLON | THEN ->
          let MenhirCell1_seq_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_54 e1 e2 in
          _menhir_goto_seq_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_INR, _menhir_box_prog) _menhir_cell1_cons_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_cons_expr (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_INR (_menhir_stack, _menhir_s) = _menhir_stack in
      let tp = _v in
      let _v = _menhir_action_30 e tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_INL, _menhir_box_prog) _menhir_cell1_cons_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_cons_expr (_menhir_stack, _, e) = _menhir_stack in
      let MenhirCell1_INL (_menhir_stack, _menhir_s) = _menhir_stack in
      let tp = _v in
      let _v = _menhir_action_29 e tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_077 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_CONS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _menhir_s = MenhirState078 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_HEAD as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _menhir_s = MenhirState072 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ISNIL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _menhir_s = MenhirState066 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LAM _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_s = MenhirState061 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState055 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LETREC _menhir_cell0_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState050 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SMALL_OPTION ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LETREC ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LAM ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_NIL -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_NIL (_menhir_stack, _menhir_s) = _menhir_stack in
          let tp = _v in
          let _v = _menhir_action_14 tp in
          _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_NONE -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_NONE (_menhir_stack, _menhir_s) = _menhir_stack in
          let tp = _v in
          let _v = _menhir_action_13 tp in
          _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TAIL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RB ->
          let _menhir_s = MenhirState028 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ZERO ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | UNIT ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TAIL ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUCC ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SOME ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PRED ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NONE ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NIL ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LCB ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISZERO ->
              _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ISNIL ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INR ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INL ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | HEAD ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIX ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONS ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LP -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LP (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_20 t in
          _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_sum_typ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_sum_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
      let t2 = _v in
      let _v = _menhir_action_41 t1 t2 in
      _menhir_goto_fun_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack ttv_result. ((ttv_stack, ttv_result) _menhir_cell1_sum_typ as 'stack) -> _ -> _ -> _ -> ('stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_prod_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW | AS | COMMA | CONS | DOT | ELSE | EOF | EQ | FALSE | FIX | HEAD | IN | INL | INR | ISNIL | ISZERO | LCB | LP | NIL | NONE | OF | PIPE | PLUS | PRED | RB | RCB | RP | SEMICOLON | SOME | SUCC | TAIL | THEN | TRUE | UNIT | VAR _ | ZERO ->
          let MenhirCell1_sum_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
          let t2 = _v in
          let _v = _menhir_action_56 t1 t2 in
          _menhir_goto_sum_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_prod_typ -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_prod_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
      let t2 = _v in
      let _v = _menhir_action_47 t1 t2 in
      _menhir_goto_prod_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_LIST -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_LIST (_menhir_stack, _menhir_s) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_18 t in
      _menhir_goto_atomic_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_100 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_goto_app_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_095 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_ISZERO -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ISZERO (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_28 e in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_094 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ISNIL, _menhir_box_prog) _menhir_cell1_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell1_ISNIL (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_35 e tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_091 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_INR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cons_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | AS ->
          let _menhir_s = MenhirState092 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_INL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cons_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | AS ->
          let _menhir_s = MenhirState089 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNITT ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPTION ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NAT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LP ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIST ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_087 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_HEAD, _menhir_box_prog) _menhir_cell1_typ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell1_HEAD (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_32 e tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_FIX -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_FIX (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_31 e in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_084 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_CONS, _menhir_box_prog) _menhir_cell1_typ, _menhir_box_prog) _menhir_cell1_cons_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_cons_expr (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell1_typ (_menhir_stack, _, tp) = _menhir_stack in
      let MenhirCell1_CONS (_menhir_stack, _menhir_s) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_34 e1 e2 tp in
      _menhir_goto_cons_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_083 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_CONS, _menhir_box_prog) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cons_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | VAR _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState083
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ZERO ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | UNIT ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TAIL ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUCC ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SOME ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SMALL_OPTION ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PRED ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NONE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NIL ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LETREC ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LCB ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LAM ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISZERO ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ISNIL ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INR ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INL ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | HEAD ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIX ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONS ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_157 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_ptyp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState157 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNITT ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPTION ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NAT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LP ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LIST ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let ptyp =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_ptyp v = _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
