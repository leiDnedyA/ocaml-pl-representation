
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

