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
        constants
---------------------------
*)

let hashtable_size = 25;;


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

let rec evaluate_expression env expr =
  match expr with
  | Int n -> n
  | Var x -> Hashtbl.find env x
  | Add (x, y) -> 
    (evaluate_expression env x) + (evaluate_expression env y)
  | Subtract (x, y) -> 
    (evaluate_expression env x) - (evaluate_expression env y)
  | Multiply (x, y) -> 
    (evaluate_expression env x) * (evaluate_expression env y)
  | Divide (x, y) -> 
    (evaluate_expression env x) / (evaluate_expression env y)
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

let vars_hashtable = Hashtbl.create hashtable_size;;
Hashtbl.add vars_hashtable "x" 3;;

let test_var = Var("x");;

let test_add = Add(Int(20), test_var);;
let test_subtract = Subtract(Int(20), test_var);;
let test_multiply = Multiply(Int(20), test_var);;
let test_divide = Divide(Int(20), test_var);;

Printf.printf "%d\n" (evaluate_expression vars_hashtable test_add);;
Printf.printf "%d\n" (evaluate_expression vars_hashtable test_subtract);;
Printf.printf "%d\n" (evaluate_expression vars_hashtable test_multiply);;
Printf.printf "%d\n" (evaluate_expression vars_hashtable test_divide);;

(* (* Reference for how to use hashtables *)
let vars_hashtable = Hashtbl.create hashtable_size;;
let lookup_var name = Hashtbl.find vars_hashtable name;;
let create_var name value = Hashtbl.add vars_hashtable name value;;
*)
