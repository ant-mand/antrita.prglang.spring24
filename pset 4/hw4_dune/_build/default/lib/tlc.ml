type typ 
= TBool
| TNat 
| TFun of typ * typ

type term
= TmVar of string
| TmAbs of string * typ * term
| TmApp of term * term
| TmTrue 
| TmFalse 
| TmZero
| TmIf of term * term * term
| TmSucc of term
| TmPred of term
| TmIsZero of term
