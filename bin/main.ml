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
  | Add
  | Subtract
  | Multiply
  | Divide
  (* Boolean operations *)
  | And
  | Or
  | Not

type statement =
  | Assign of string * expression
  | If of expression * int
  | Goto of int (*the purpose of a goto is express a while loop as an if statement*)

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

(* (* Reference for how to use hashtables *)
let vars_hashtable = Hashtbl.create hashtable_size;;
let lookup_var name = Hashtbl.find vars_hashtable name;;
let create_var name value = Hashtbl.add vars_hashtable name value;;
*)
