{
open Parser
}

let white = [' ' '\t']+
let lletter = ['a'-'z']
let digit = ['0'-'9']
let id = lletter (digit | lletter)*
let lam = "&"|"λ"
let arrow = "->"|"→"


rule read = 
    parse
    | white { read lexbuf }
    | "(" { LP }
    | ")" { RP }
    | lam { LAM }
    | "." { DOT }
    | ":" { COLON }
    | arrow { ARROW }
    | "True" { TRUE }
    | "False" { FALSE }
    | ("Zero"|"0") {ZERO}
    | "If" { IF }
    | "Then" { THEN }
    | "Else" { ELSE }
    | "Succ" { SUCC }
    | "Pred" { PRED }
    | ("IsZero"|"0?") {ISZERO}
    | "Nat" { NAT }
    | "Bool" { BOOL }
    | id { VAR (Lexing.lexeme lexbuf) }
    | eof { EOF }
