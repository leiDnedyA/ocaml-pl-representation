(* open Z3 *)
include Lib_types
include Lib_evaluation

(*
------------------------------------------------------
       function for printing user-defined types
------------------------------------------------------
*)

let rec print_expr env expr =
  match expr with
  | Int x -> Printf.printf "%d\n" x
  | False -> print_endline "False"
  | True -> print_endline "True"
  | _ -> print_expr env (evaluate_expression env expr)


