{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let lletter = ['a'-'z']
let digit = ['0'-'9']
let id = ("_" digit*)? lletter (digit | lletter | "_")*
let lam = "&"|"λ"
let arrow = "->"|"→"

rule read = 
    parse
    | white { read lexbuf }
    | "(" { LP }
    | ")" { RP }
    | "[" { LB }
    | "]" { RB }
    | lam { LAM }
    | "." { DOT }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "true" { TRUE }
    | "false" { FALSE }
    | ("zero"|"0") { ZERO }
    | "unit" { UNIT }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "succ" { SUCC }
    | "pred" { PRED }
    | ("iszero"|"0?") { ISZERO }
    | "case" { CASE }
    | "of" { OF }
    | "|" { PIPE }
    | "_" { UNDERSCORE }
    | "=>" { FATARROW }
    | "as" { AS }
    | "some" { SOME }
    | "none" { NONE }
    | "=" { EQ }
    | "null" { NULL }
    | ("null?"|"isnull") { ISNULL }
    | "raise" { RAISE }
    | "try" { TRY }
    | "with" { WITH }
    | "ref" { REF }
    | ":=" { ASSN }
    | "!" { BANG }
    | "Nat" { NAT }
    | "Bool" { BOOL }
    | "Unit" { UNITT }
    | "Ref" { REFT }
    | "Bot" { BOT }
    | "Option" { OPTION }
    | arrow { ARROW }
    | id { VAR (Lexing.lexeme lexbuf) }
    | eof { EOF }
