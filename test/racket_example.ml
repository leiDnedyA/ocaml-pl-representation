open Lib;;

let test_passed label = Printf.printf "Test %s passed 👍\n" label
let test_failed label = Printf.printf "Test %s failed 🟥\n" label

(*
------------------------------------------------------
       Test code
------------------------------------------------------
*)


(* Expected result env: [x = 3] *)
let env_1 = Hashtbl.create 1;;
let integration_1 = [
  If(Gt(Int(1), Int(0)), 3);
  Assign("x", Int(1));
  Goto(4);
  Assign("x", Int(3))
  ];;
evaluate_program env_1 0 integration_1;;
if ((Hashtbl.find env_1 "x") == (Int(3))) then
  test_passed "Test 1"
else
  test_failed "Test 1"

(* Expected result env: [] *)
let env_2 = Hashtbl.create 3;;
let integration_2 = [
  Assign("x", True);
  Assign("y", False);
  If(Or(Var("x"), Var("y")), 5);
  Assign("result", Int(-1));
  Goto(999);
  Assign("result", Int(1));
  ];;
evaluate_program env_2 0 integration_2;;
if ((Hashtbl.find env_2 "result") == (Int (1))) then
  test_passed "Test 2"
else
  test_failed "Test 2"
