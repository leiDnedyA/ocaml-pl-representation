open Z3
open Lib_types

let mk = mk_context [];;

let bool1 = Boolean.mk_val mk true;;
let bool2 = Boolean.mk_val mk false;;

let expr1 = Boolean.mk_or mk [bool1;bool2];;
let expr2 = Boolean.mk_and mk [bool1;bool2];;

let bool_of_z3 e = Z3.Boolean.is_true e;;

let solve_z3_bool_expr expr mk =
  let solver = (Solver.mk_simple_solver mk) in
    let model = Option.get (Solver.get_model solver) in
    bool_of_z3 (Option.get (Model.eval model expr true));;

let rec z3_expr_of_expr expr mk =
  match expr with
  | True -> Z3.Boolean.mk_true mk
  | False -> Z3.Boolean.mk_false mk
  | And (x, y) -> Z3.Boolean.mk_and mk [
    (z3_expr_of_expr x mk);
    (z3_expr_of_expr y mk)]
  | Or (x, y) -> Z3.Boolean.mk_or mk [
    (z3_expr_of_expr x mk);
    (z3_expr_of_expr y mk)]
  | Not x -> Z3.Boolean.mk_not mk (z3_expr_of_expr x mk)
  | _ -> failwith("invalid boolean expression");;

(*
  Can evaluate boolean expressions including And, Or, and Not expressions,
  but can't do inequalities yet
 *)
let eval_pure_bool_expr expr =
  let mk = mk_context [] in
    let bool_result = solve_z3_bool_expr (z3_expr_of_expr expr mk) mk in
      if bool_result == true then True else False;;

let z3_expr1 = z3_expr_of_expr (Not False) mk;;
let result = solve_z3_bool_expr z3_expr1 mk;;


