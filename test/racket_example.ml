open Lib;;

(*
------------------------------------------------------
       Test code
------------------------------------------------------
*)

let vars = Hashtbl.create 25;;
Hashtbl.add vars "x" (Int (1));;

let test_var = Var("x");;

let test_program = [
  If(True, 3);
  Assign("x", Int(1));
  Goto(4);
  Assign("x", Int(3))
  ];;

evaluate_program vars 0 test_program;;

print_expr vars (evaluate_expression vars (Gt(Int(1), Int(0)))) ;;

let test_number = 3;;

print_expr vars (Hashtbl.find vars "x")
