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
  (* Boolean operations *)
  | And of expression * expression
  | Or of expression * expression
  | Not of expression

type statement =
  | Assign of string * expression
  | If of expression * int
  | Goto of int (*the purpose of a goto is express a while loop as an if statement*)


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
  | Add (_, _) -> Int (evaluate_arithmetic env expr)
  | Subtract (_, _) -> Int (evaluate_arithmetic env expr)
  | Multiply (_, _) -> Int (evaluate_arithmetic env expr)
  | Divide (_, _) -> Int (evaluate_arithmetic env expr)
  | _ -> failwith("not yet implemented")

let rec evaluate_statement env pc stmt = (*env -> hashtable, pc -> program counter (line number), stmt -> statement*)
  (*evaluate a statement -> return an updated env and updated pc*)
  match stmt with
  | _ -> 0
  (*
  | Assign(str, expr) -> str;expr;0
      (*Update the env*)
      (*(env, pc + 1)*)
  | If (expr, pc) -> expr;pc;1(*Make sure expr is boolean (2 < 5) (true && false)*)
      (*If expr evaluates to true, jump to pc*)
      (*Else, increment pc by 1*)
  | Goto(pc) -> pc + 1
   *)

let evaluate_program (program:statement list) = 0


(*
------------------------------------------------------
       funcs for printing user-defined types
------------------------------------------------------
*)

let rec print_expr env expr =
  match expr with
  | Int x -> Printf.printf "%d\n" x
  | False -> print_endline "False"
  | True -> print_endline "True"
  | _ -> print_expr env (evaluate_expression env expr)


(*
------------------------------------------------------
       Test code
------------------------------------------------------
*)

let vars = Hashtbl.create 25;;
Hashtbl.add vars "x" (Int (1));;

let test_var = Var("x");;

let test_add = Add(Int(20), test_var);;
let test_subtract = Subtract(Int(20), test_var);;
let test_multiply = Multiply(Int(20), test_var);;
let test_divide = Divide(Int(20), test_var);;

print_expr vars (Add (Int (1), test_var));;

(*
Printf.printf "%d\n" (evaluate_arithmetic vars_hashtable test_add);;
Printf.printf "%d\n" (evaluate_arithmetic vars_hashtable test_subtract);;
Printf.printf "%d\n" (evaluate_arithmetic vars_hashtable test_multiply);;
Printf.printf "%d\n" (evaluate_arithmetic vars_hashtable test_divide);;
 *)

(* (* Reference for how to use hashtables *)
let vars_hashtable = Hashtbl.create hashtable_size;;
let lookup_var name = Hashtbl.find vars_hashtable name;;
let create_var name value = Hashtbl.add vars_hashtable name value;;
*)
