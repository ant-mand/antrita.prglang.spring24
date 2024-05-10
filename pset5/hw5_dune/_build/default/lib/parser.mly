%{
open Etlc
%}


%token LP RP LB RB LCB RCB COMMA
%token UNDERSCORE

%token LAM COLON DOT
%token <string> VAR
%token SEMICOLON
%token LET IN LETREC EQ

%token TRUE FALSE
%token ZERO
%token UNIT
%token NIL
%token ONE TWO

%token IF THEN ELSE

%token SUCC PRED ISZERO
%token INL INR
%token SOME NONE
%token FIX HEAD TAIL CONS ISNIL

%token CASE OF PIPE FATARROW SMALL_OPTION

%token AS

%token NAT BOOL LIST OPTION UNITT

%token ARROW

%token TIMES PLUS

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

atomic_typ:
    | UNITT { TUnit }
    | BOOL { TBool }
    | NAT { TNat }
    | LIST; t = atomic_typ { TList t }
    | OPTION; t = atomic_typ { tp_opt t }
    | LP; t = typ; RP { t }
;

fun_typ:
    | sum_typ { $1 }
    | t1 = sum_typ; ARROW; t2 = fun_typ
        { TFun (t1, t2) }
;

sum_typ:
    | prod_typ { $1 }
    | t1 = sum_typ; PLUS; t2 = prod_typ
        { TSum (t1, t2) }
;

prod_typ:
    | atomic_typ { $1 }
    | t1 = prod_typ; TIMES; t2 = atomic_typ
        { TProd (t1, t2) }
;


expr: 
    | lambda_expr { $1 }
    | LET; x = VAR; COLON; tp = typ; EQ; e1 = expr; IN; e2 = expr 
        { tm_let x tp e1 e2 }
    | LETREC; x = VAR; COLON; tp = typ; EQ; e1 = expr; IN; e2 = expr 
        { tm_letrec x tp e1 e2 }
;

lambda_expr:
    | body_expr { $1 }
    | LAM; x = VAR; COLON; t = typ; DOT; e = lambda_expr { TmAbs (x, t, e) }
;

pat:
    | VAR { $1 }
    | UNDERSCORE { "_" }
;

body_expr:
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { TmIf (e1, e2, e3) }
    | CASE; e = expr; OF; INL; x1 = pat; FATARROW; e1 = expr; PIPE; INR; x2 = pat; FATARROW; e2 = expr 
        { TmCase (e, x1, e1, x2, e2) }
    | SMALL_OPTION; CASE; e = expr; OF; SOME; x = pat; FATARROW; e1 = expr; PIPE; NONE; FATARROW; e2 = expr
        { tm_opt_case e x e1 e2 }
    | seq_expr { $1 }
;

seq_expr:
    | ascr_expr { $1 }
    | e1 = seq_expr; SEMICOLON; e2 = ascr_expr
        { tm_seq e1 e2 }
;

ascr_expr:
    | app_expr { $1 }
    | e = ascr_expr; AS; tp = typ 
        { tm_ascribe e tp }
;

app_expr:
    | cons_expr { $1 }
    | e1 = app_expr; e2 = cons_expr
        { TmApp (e1, e2) }
;

cons_expr:
    | proj_expr { $1 }
    | SUCC; e = cons_expr { TmSucc e }
    | PRED; e = cons_expr { TmPred e }
    | ISZERO; e = cons_expr { TmIsZero e }
    | INL; e = cons_expr; AS; tp = typ { TmInl (tp, e) }
    | INR; e = cons_expr; AS; tp = typ { TmInr (tp, e) }
    | FIX e = cons_expr { TmFix e }
    | HEAD; LB; tp = typ; RB; e = cons_expr 
        { TmHead (tp, e) }
    | TAIL; LB; tp = typ; RB; e = cons_expr 
        { TmTail (tp, e) }
    | CONS; LB; tp = typ; RB; e1 = cons_expr; e2 = cons_expr
        { TmCons (tp, e1, e2) }
    | ISNIL; LB; tp = typ; RB; e = cons_expr 
        { TmIsNil (tp, e) }
    | SOME; LB; NAT; RB; e = cons_expr
        { tm_some e }
;

proj_expr:
    | atomic_expr { $1 }
    | e = proj_expr; DOT; ONE 
        { TmFst e }
    | e = proj_expr; DOT; TWO 
        { TmSnd e }
;

atomic_expr:
    | LP; e = expr; RP { e }
    | LCB; e1 = expr; COMMA; e2 = expr; RCB 
        { TmPair (e1, e2) }
    | x = VAR { TmVar x }
    | TRUE { TmTrue }
    | FALSE { TmFalse }
    | ZERO { TmZero }
    | UNIT { TmUnit }
    | NONE; LB; tp = typ; RB { tm_none (tp_opt tp) }
    | NIL; LB; tp = typ; RB { TmNil tp }
;
