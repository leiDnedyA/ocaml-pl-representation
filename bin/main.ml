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

let hashtable_size = 25;;

type value =
  |Int of int
  |Var of string
;;
      
type operation =
  |Multiply of value * value
  |Subtract of value * value
  |Add of value * value
  |Define of value * value
;;


let vars_hashtable = Hashtbl.create hashtable_size;;
let lookup_var name = Hashtbl.find vars_hashtable name;;
let create_var name value = Hashtbl.add vars_hashtable name value;;


let eval_add = function
  | Add (Int a, Int b) -> a + b
  | Add (Var a, Int b) -> (lookup_var a) + b
  | Add (Int a, Var b) -> (lookup_var b) + a
  | Add (Var a, Var b) -> (lookup_var a) + (lookup_var b)
  | Multiply (_, _) -> failwith("Invalid operation type for eval_add")
  | Subtract (_, _) -> failwith("Invalid operation type for eval_add")
  | Define (_, _) -> failwith("Invalid operation type for eval_add")
;;

let eval_operation expr = 
  match expr with
  | Add (a, b) -> eval_add (Add (a, b))
  | Define (Var var_name, Int var_value) ->
      create_var var_name var_value; 0
  | Subtract (_, _) -> failwith("Not yet implemented")
  | Multiply (_, _) -> failwith("Not yet implemented")
  | Define (_, _) -> failwith("Invalid define operation")
;;


let define_test = Define (Var "x", Int 2);;
let add_test = Add (Int 2, Var "x";);;

eval_operation define_test;;
Printf.printf "%d\n" (lookup_var "x");;

Printf.printf "%d\n" (eval_operation add_test);;

Multiply (Int 1, Int 2);;
Subtract (Int 1, Int 2);;
