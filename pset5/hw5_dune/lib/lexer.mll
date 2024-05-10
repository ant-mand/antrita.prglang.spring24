{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let lletter = ['a'-'z']
let digit = ['0'-'9']
let id = lletter (digit | lletter)*
let lam = "&"|"λ"
let arrow = "->"|"→"
let times = "*"|"×"


rule read = 
    parse
    | white { read lexbuf }
    | "(" { LP }
    | ")" { RP }
    | "[" { LB }
    | "]" { RB }
    | "{" { LCB }
    | "}" { RCB }
    | lam { LAM }
    | "." { DOT }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "," { COMMA }
    | "let" { LET }
    | "in" { IN }
    | "letrec" { LETREC }
    | "=" { EQ }
    | "true" { TRUE }
    | "false" { FALSE }
    | ("zero"|"0") { ZERO }
    | "unit" { UNIT }
    | "none" { NONE }
    | "nil" { NIL }
    | "1" { ONE }
    | "2" { TWO }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "succ" { SUCC }
    | "pred" { PRED }
    | ("iszero"|"0?") { ISZERO }
    | "inl" { INL }
    | "inr" { INR }
    | "fix" { FIX }
    | "head" { HEAD }
    | "tail" { TAIL }
    | "cons" { CONS }
    | ("isnil"|"nil?") { ISNIL }
    | "some" { SOME }
    | "none" { NONE }
    | "case" { CASE }
    | "of" { OF }
    | "|" { PIPE }
    | "_" { UNDERSCORE }
    | "=>" { FATARROW }
    | "option" { SMALL_OPTION }
    | "as" { AS }
    | "Nat" { NAT }
    | "Bool" { BOOL }
    | "List" { LIST }
    | "Option" { OPTION }
    | "Unit" { UNITT }
    | arrow { ARROW }
    | times { TIMES }
    | "+" { PLUS }
    | id { VAR (Lexing.lexeme lexbuf) }
    | eof { EOF }
