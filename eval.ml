open Dwt.Infix
open Types

exception ExpectedUnit
exception ExpectedBool
exception ExpectedInt
exception ExpectedFunction
exception ExpectedHandle
exception ExpectedPromise
exception ExpectedRef
exception ExpectedList
exception ExpectedString
exception UnboundVariable
exception InexhaustivePatterns
exception IncompatibleTypes
exception ArgumentMismatch
exception LetMismatch
exception AwaitMismatch

(* [eval env exp] is [v], where [v] is the result of evaluating [exp] under the
 * environment [env]
 * side effects: produces the applicable i/o side effects *)
let rec eval (env : env) (exp : exp) : value =
  failwith "Implement me!"

(* [bind_pat p v] is [None] where [v] does not match [p], and [Some b]
 * where [v] matches [p] producing new bindings [b] *)
and bind_pat (p : pat) (v : value) : env option =
  failwith "Implement me!"

(* You may use the following utility functions in your implementation.
 * Example usage: [eval env exp |> assert_unit] *)

and assert_unit = function
  | VUnit -> ()
  | v -> raise ExpectedUnit

and assert_bool = function
  | VBool b -> b
  | _ -> raise ExpectedBool

and assert_int = function
  | VInt i -> i
  | _ -> raise ExpectedInt

and assert_fun = function
  | VFun (f, b, env) -> (f, b, env)
  | _ -> raise ExpectedFunction

and assert_handle = function
  | VHandle h -> h
  | _ -> raise ExpectedHandle

and assert_dwt = function
  | VDwt dwt -> dwt
  | _ -> raise ExpectedPromise

and assert_ref = function
  | VRef ref -> ref
  | _ -> raise ExpectedRef

and assert_list vs =
  match vs with
  | VNil | VCons _ -> vs
  | _ -> raise ExpectedList

and assert_string = function
  | VStr s -> s
  | _ -> raise ExpectedString

(* Converts a list into a VList. *)
and vlist_of_list l =
  let rec loop acc = function
  | [] -> acc
  | h::t -> loop (VCons(h,acc)) t in
  loop VNil (List.rev l)

