open Hw6
open Etlc
open Lang

(* Interpreter *)

(* This is for you to debug your code. *)

let interpret (ast : term) : unit =
  let term_str = Unparse.unparse ast in
  let steps, _ = Util.multistep_list ast IMap.empty in
  let typ =
    Option.value
      (Option.map Unparse.unparse_typ (type_infer empty_ctx empty_store ast))
      ~default:""
  in
  print_endline "----- Call by Value Multistep Evaluation -----";
  print_endline ("      " ^ term_str);
  List.iter (fun step -> print_endline ("->    " ^ Unparse.unparse step)) steps;
  print_endline " : ";
  print_endline ("      " ^ typ)
;;

let arg1 = Sys.argv.(1) in
if arg1 = "-f" then
  let ast = Parse.parse_file Sys.argv.(2) in
  interpret ast
else
  let ast = Parse.parse arg1 in
  interpret ast
