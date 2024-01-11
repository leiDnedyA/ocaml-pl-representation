open Lib_types

(*
---------------------------
        evaluation funcs
---------------------------
*)

let rec evaluate_arithmetic env expr =
  match expr with
  | Int n -> n
  | Var x -> evaluate_arithmetic env (Hashtbl.find env x)
  | Add (x, y) -> 
      (evaluate_arithmetic env x) + (evaluate_arithmetic env y)
  | Subtract(x, y) ->
      (evaluate_arithmetic env x) - (evaluate_arithmetic env y)
  | Multiply(x, y) ->
      (evaluate_arithmetic env x) * (evaluate_arithmetic env y)
  | Divide(x, y) -> 
      (evaluate_arithmetic env x) / (evaluate_arithmetic env y)
  | _ -> failwith("Invalid arithmetical expression")

let expression_of_bool b =
  match b with
  | true -> True
  | false -> False

let rec evaluate_expression env expr =
  match expr with
  | True|False -> expr
  | Int i -> Int i
  | Var x -> evaluate_expression env (Hashtbl.find env x)
  | Add (_, _) | Subtract (_, _) | Multiply (_, _) | Divide (_, _)->
      Int (evaluate_arithmetic env expr)
  | Eq (_, _) | Gt (_, _) | Lt (_, _) | Gte (_, _) | Lte (_, _) ->
      evaluate_inequality env expr
  | And (_, _) | Or (_, _) | Not _ -> evaluate_boolean env expr
and evaluate_inequality env expr =
  match expr with
  | Eq (x, y) ->
      expression_of_bool ((evaluate_expression env x) == (evaluate_expression env y))
  | Gt (x, y) ->
      expression_of_bool ((evaluate_expression env x) > (evaluate_expression env y))
  | Lt (x, y) ->
      expression_of_bool ((evaluate_expression env x) < (evaluate_expression env y))
  | Gte (x, y) ->
      expression_of_bool ((evaluate_expression env x) >= (evaluate_expression env y))
  | Lte (x, y) ->
      expression_of_bool ((evaluate_expression env x) <= (evaluate_expression env y))
  | _ -> failwith("Invalid inequality expression")
and evaluate_boolean env expr =
  match expr with
  (*Base cases / Lowest level boolean algebra*)
  | True | False -> expr
  | Not (True) -> False
  | Not (False) -> True
  | And (False, False) -> False
  | And (True, False) -> False
  | And (False, True) -> False
  | And (True, True) -> True
  | Or (True, True) -> True
  | Or (False, True) -> True
  | Or (True, False) -> True
  | Or (False, False) -> False
  (*Cases with variables*)
  | Not (x) ->
      evaluate_boolean env (Not (evaluate_expression env x))
  | And (x, y) ->
      evaluate_boolean env (And ((evaluate_expression env x), (evaluate_expression env y)))
  | Or (x, y) ->
      evaluate_boolean env (Or ((evaluate_expression env x), (evaluate_expression env y)))
  | _ -> failwith("Invalid boolean expression")

let evaluate_statement env pc stmt = (*env -> hashtable, pc -> program counter (line number), stmt -> statement*)
  (*evaluate a statement -> return an updated env and updated pc*)
  match stmt with
  | Assign (str, expr) ->
      Hashtbl.replace env str expr; pc + 1
  | If (expr, if_pc) ->
      (
      match (evaluate_expression env expr) with
      | True -> if_pc
      | False -> pc + 1
      | _ -> failwith("Invalid boolean result")
      )
  | Goto (goto_pc) -> goto_pc

let rec evaluate_program env pc (program:statement list) = 
  if (pc >= List.length program) then
    0
  else
    evaluate_program env (evaluate_statement env pc (List.nth program pc)) program
