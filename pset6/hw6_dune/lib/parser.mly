%{
open Etlc
%}


%token LP RP LB RB 
%token UNDERSCORE

%token LAM COLON DOT
%token <string> VAR
%token SEMICOLON

%token TRUE FALSE
%token ZERO
%token UNIT

%token IF THEN ELSE

%token SUCC PRED ISZERO
%token NULL ISNULL

%token CASE OF PIPE FATARROW
%token AS EQ
%token SOME NONE

%token RAISE TRY WITH

%token REF ASSN BANG

%token NAT BOOL UNITT REFT BOT
%token OPTION

%token ARROW

%token EOF

%start <term> prog 
%start <typ> ptyp

%%

prog: 
    | e = expr; EOF { e }
;

ptyp:
    | tp = typ; EOF { tp }
;

typ:
    | fun_typ { $1 }
;

fun_typ:
    | atomic_typ { $1 }
    | t1 = atomic_typ; ARROW; t2 = fun_typ
        { TFun (t1, t2) }
;

var:
   | SOME { "some" }
   | NONE { "none" }
   | VAR  { $1 }

var_typ_rule:
   | lbl = var; COLON; tp = typ
        { (lbl, tp) }
;

atomic_typ:
    | LP; tp = typ; RP { tp }
    | UNITT { TUnit }
    | BOOL { TBool }
    | NAT { TNat }
    | REFT; tp = atomic_typ
        { TRef tp }
    | OPTION; tp = atomic_typ
        { tp_opt tp }
    | BANG|BOT { TBot }
    | LB; vars = separated_list(SEMICOLON, var_typ_rule); RB
        { TVariant vars }
;


expr: 
    | lambda_expr { $1 }
;

lambda_expr:
    | body_expr { $1 }
    | LAM; x = var; COLON; t = typ; DOT; e = lambda_expr
        { TmAbs (x, t, e) }
;

pat:
    | var { $1 }
    | UNDERSCORE { "_" }
;

case_rule:
   lbl = var; x = pat; FATARROW; e = expr
        { (lbl, x, e) }
;

body_expr:
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { TmIf (e1, e2, e3) }
    | CASE; e = expr; OF; LB; lst = separated_list(PIPE, case_rule); RB 
        { TmCase (e, lst) }
    | TRY; e1 = expr; WITH; e2 = expr
        { TmTry (e1, e2) }
    | assn_expr { $1 }
;

assn_expr:
    | app_expr { $1 }
    | e1 = assn_expr; ASSN; e2 = app_expr
        { TmAssn (e1, e2) }
;

app_expr:
    | atomic_expr { $1 }
    | e1 = app_expr; e2 = atomic_expr
        { TmApp (e1, e2) }
;

atomic_expr:
    | LP; e = expr; RP { e }

    | x = var { TmVar x }

    | TRUE { TmTrue }
    | FALSE { TmFalse }
    | UNIT { TmUnit }

    | ZERO { TmZero }
    | SUCC; e = atomic_expr { TmSucc e }
    | PRED; e = atomic_expr { TmPred e }
    | ISZERO; e = atomic_expr { TmIsZero e }

    | lbl = var; EQ; e = expr; AS; tp = typ
        { TmVariant (lbl, e, tp) }

    | SOME; LB; tp = typ; RB; e = atomic_expr
        { tm_some e tp }
    | NONE; LB; tp = typ; RB
        { tm_none tp }

    | REF; e = atomic_expr { TmRef e }
    | BANG; e = atomic_expr { TmBang e }

    | NULL { TmNull }
    | ISNULL; LB; tp = typ; RB; e = atomic_expr
        { TmIsNull (tp, e) }

    | RAISE; e = atomic_expr { TmRaise e }
;
