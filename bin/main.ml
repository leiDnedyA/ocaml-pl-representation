open Z3

let mk = mk_context [];;


let bool1 = Boolean.mk_val mk true;;
let bool2 = Boolean.mk_val mk false;;

let expr1 = Boolean.mk_or mk [bool1;bool2];;

let bool_of_z3 e = Z3.Boolean.is_true e;;

let solve_bool_expression expr mk =
  let solver = (Solver.mk_simple_solver mk) in
    let model = Option.get (Solver.get_model solver) in
    bool_of_z3 (Option.get (Model.eval model expr true));;

Printf.printf "%b\n" (solve_bool_expression expr1 mk);;
