%{
open Tlc
%}


%token LP
%token RP
%token LAM
%token DOT
%token <string> VAR
%token TRUE
%token FALSE
%token ZERO
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO

%token COLON
%token NAT
%token BOOL
%token ARROW
%right ARROW

%token EOF

%start <term> prog

%%

prog: 
    | e = expr; EOF { e }
;

typ:
    | BOOL { TBool } 
    | NAT { TNat } 
    | t1 = typ; ARROW; t2 = typ { TFun (t1, t2) }
    | LP; t = typ; RP { t }
;

expr: 
    | LP; e = expr; RP { e }
    | x = VAR { TmVar x }
    | LAM; x = VAR; COLON; t = typ; DOT; e = expr { TmAbs (x, t, e) }
    | LP; e1 = expr; e2 = expr; RP { TmApp (e1, e2) }
    | TRUE { TmTrue }
    | FALSE { TmFalse }
    | ZERO { TmZero }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { TmIf (e1, e2, e3) }
    | SUCC; e = expr { TmSucc e }
    | PRED; e = expr { TmPred e }
    | ISZERO; e = expr { TmIsZero e }
;
