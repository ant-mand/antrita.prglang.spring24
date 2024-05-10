let parse (s : string) : Etlc.term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_typ (s : string) : Etlc.typ =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.ptyp Lexer.read lexbuf in
  ast

let parse_file (file_name : string) : Etlc.term =
  let ic = open_in_bin file_name in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  parse content
