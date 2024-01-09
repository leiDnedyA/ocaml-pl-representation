(*internal representation -> evaluate -> print result*)
(*addition, susbtraction, multiplication*)

(* (define x 100) *)
(* hash table : ["x":1] ["y":200] *)
(* (define x 1) *)

(*
(* Notes from Jan 4 meeting w Ramsey *)

type expression =
  | int
  | var
  | True
  | False
  | Add # <and all other arithmetical expressions
  | And # <and all other boolean operators>

type statement =
  | Assign string * expression
  | If expression * int
  | Goto int # the purpose of a goto is express a while loop as an if statement

let rec eval_statement env pc stmt = # env -> hashtable, pc -> program counter (line number), stmt -> statement
  (*evaluate a statement -> return an updated env and updated pc*)
  match stmt with
  | Assign(str, expr) ->
      (*Update the env*)
      (env, pc + 1)
  | If (expr, pc) -> (*Make sure expr is boolean (2 < 5) (true && false)*)
      (*If expr evaluates to true, jump to pc*)
      (*Else, increment pc by 1*)
  | Goto(pc) -> pc

  let evaluate_program (program:statement list) = 

 *)


(*
---------------------------
        datatypes
---------------------------
*)

type expression =
  | Int of int
  | Var of string
  | True
  | False
  (* Arithmetic operations *)
  | Add of expression * expression
  | Subtract of expression * expression
  | Multiply of expression * expression
  | Divide of expression * expression
  (* Mathematical inequalities *)
  | Eq of expression * expression (* Equal to *)
  | Gt of expression * expression (* Greater than *)
  | Lt of expression * expression (* Less than *)
  | Gte of expression * expression (*Greater than or equal to *)
  | Lte of expression * expression (* Less than or equal to *)
  (* Boolean operations *)
  | And of expression * expression
  | Or of expression * expression
  | Not of expression

type statement =
  | Assign of string * expression
  | If of expression * int
  | Goto of int 


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

let rec evaluate_boolean env expr =
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
      evaluate_boolean env (Not (evaluate_boolean env x))
  | And (x, y) ->
      evaluate_boolean env (And ((evaluate_boolean env x), (evaluate_boolean env y)))
  | Or (x, y) ->
      evaluate_boolean env (Or ((evaluate_boolean env x), (evaluate_boolean env y)))
  | _ -> failwith("Invalid boolean expression")

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
  | Eq (Int x, Int y) -> if x == y then True else False
  | Gt (Int x, Int y) -> if x > y then True else False
  | Lt (Int x, Int y) -> if x < y then True else False
  | Gte (Int x, Int y) -> if x >= y then True else False
  | Lte (Int x, Int y) -> if x <= y then True else False
  | Eq (x, y) ->
      evaluate_inequality env (Eq ((evaluate_expression env x), (evaluate_expression env y)))
  | Gt (x, y) ->
      evaluate_inequality env (Gt ((evaluate_expression env x), (evaluate_expression env y)))
  | Lt (x, y) ->
      evaluate_inequality env (Lt ((evaluate_expression env x), (evaluate_expression env y)))
  | Gte (x, y) ->
      evaluate_inequality env (Gte ((evaluate_expression env x), (evaluate_expression env y)))
  | Lte (x, y) ->
      evaluate_inequality env (Lte ((evaluate_expression env x), (evaluate_expression env y)))
  | _ -> failwith("Invalid inequality expression")


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


