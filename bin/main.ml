(*internal representation -> evaluate -> print result*)
(*addition, susbtraction, multiplication*)

(* (define x 100) *)
(* hash table : ["x":1] ["y":200] *)
(* (define x 1) *)

let hashtable_size = 25;;

type value =
  |Int of int
  |Var of string
;;
      
type operation =
  |Add of value list
  |Define of value * value
;;


let vars_hashtable = Hashtbl.create hashtable_size;;
let lookup_var name = Hashtbl.find vars_hashtable name;;
let create_var name value = Hashtbl.add vars_hashtable name value;;


let rec eval_add = function
  | [] -> 0
  | [Int a] -> a
  | [Var a] -> lookup_var a
  | Int first :: rest -> first + (eval_add rest)
  | Var first :: rest -> (lookup_var first) + (eval_add rest)

let eval_operation expr = 
  match expr with
  | Add a -> eval_add a
  | Define (Var var_name, Int var_value) ->
      create_var var_name var_value; 0
  | Define (_, _) -> failwith("Invalid define operation")
;;


let define_test = Define (Var "x", Int 2);;
let add_test = Add [Int 2; Var "x";];;

eval_operation define_test;;
Printf.printf "%d\n" (lookup_var "x");;

Printf.printf "%d\n" (eval_operation add_test);;


