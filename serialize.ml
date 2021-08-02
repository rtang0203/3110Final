open Types

(** Serializable values *)
type svalue =
| SUnit                             [@name "q"]
| SBool of bool                     [@name "r"]
| SInt of int                       [@name "s"]
| SStr of string                    [@name "t"]
| SFun of pat * exp * senv          [@name "u"]
(* When serializing a recursive function, we omit the recursive function from
   it's defining environment to prevent stack overflow. (The recursive function
   will be added back to its own environment during deserialization. Which is also
   why we hold on to the function name.) *)
| SFunRec of var * pat * exp * senv [@name "v"]
| SPair of svalue * svalue          [@name "w"]
| SRef of svalue                    [@name "x"]
| SNil                              [@name "y"]
| SCons of svalue * svalue          [@name "z"]
| SHandle of int                    [@name "@"]
[@@deriving yojson]

and senv = (var * svalue) list
[@@deriving yojson]

(** [unwrap error opt] is [v] if [opt] is [Some v], or else raises [error]. *)
let unwrap error = function
| Ok v -> v
| Error _ -> failwith error

(** [is_serializable v] is [true] if [v] can be safely
  * sent to another process, or [false] otherwise. *)
let rec is_serializable = function
| VUnit
| VHandle _
| VBool _
| VInt _
| VStr _
| VRef _
| VFun (_, _, _)
| VFunRec _ -> true
| VPair (l, r) -> is_serializable l && is_serializable r
| VNil -> true
| VCons (h, vs) -> is_serializable h && is_serializable vs
| VDwt _ -> false

(** Let [k] be the variable name of the unique recursive function [f] holding [env_ref].
  * Let [env] be the environment [env_ref] points to. Let [env'] be the same environment
  * but with the binding (k, f) removed.
  * 
  * [remove_rec env_ref] finds [k] and returns (k, env_ref') where [env_ref'] is a
  * new reference to [env'].
  * 
  * (For the purpose of serializing a recursive function, we omit the recursive function from
  * it's defining environment to prevent stack overflow. The recursive function
  * will be added back to its own environment during deserialization.) *)
let remove_rec env_ref =
  let rec remove_kv env =
    match env with
    | [] -> failwith "Recursion serialization error (see writeup troubleshooting \
    section). Recusive function was not found in its own environemnt."
    | ((k, VFunRec (p, b, env_ref')) as h)::t -> begin
      if env_ref == env_ref' then (k, t)
      else let (k', tail) = remove_kv t in (k', h::tail) end
    | h::t -> begin
      let (k, tail) = (remove_kv t) in (k, h::tail) end in
  remove_kv !env_ref

module Free = Set.Make (struct type t = var let compare = compare end)

(** [free_exp e] is the set of free variables of expression [e]. *)
let rec free_exp e =
  let rec free_exp_tail acc ~e = match e with
  | Unit
  | Bool _
  | Int _
  | Str _ -> acc
  | Var x -> Free.add x acc
  | Fun (p, e) ->
    e |> free_exp
      |> fun acc' -> Free.diff acc' (kill_pat p)
      |> Free.union acc
  | Await (p, e, b)
  | Let (p, e, b) ->
    acc |> free_exp_tail ~e
        |> free_exp_tail ~e:(Fun (p, b))
  | LetRec (x, e, b) ->
    acc |> free_exp_tail ~e
        |> free_exp_tail ~e:(Fun (PVar x, b))
  | Nil -> acc
  | Pair (l, r)
  | App (l, r)
  | Bin (_, l, r)
  | Cons (l, r)
  | Assign (l, r)
  | Seq (l, r)
  | Spawn (l, r)
  | Send (l, r) ->
    acc |> free_exp_tail ~e:l
        |> free_exp_tail ~e:r
  | Una (_, e)
  | Ref e
  | Deref e 
  | Recv e
  | Join e
  | Pick e
  | Return e ->
    acc |> free_exp_tail ~e
  | IfThen (b, t, f) ->
    acc |> free_exp_tail ~e:b
        |> free_exp_tail ~e:t
        |> free_exp_tail ~e:f
  | Match (e, ps) ->
    ps |> List.map (fun (p, b) -> free_exp (Fun (p, b)))
       |> List.fold_left Free.union acc
       |> free_exp_tail ~e
  in free_exp_tail Free.empty ~e

(** [kill_pat acc p] is the set of variable bindings in pattern [p], with
  * initial value [acc]. *)
and kill_pat p =
  let rec kill_pat_tail acc ~p = match p with
  | PNil
  | PUnit
  | PWild
  | PBool _
  | PInt _
  | PStr _ -> acc
  | PVar x -> Free.add x acc
  | PPair (l, r)
  | PCons (l, r) ->
    acc |> kill_pat_tail ~p:l
        |> kill_pat_tail ~p:r
  in
  kill_pat_tail Free.empty ~p

(** [svalue_of_value v] is a fallible conversion from value [v] to
  * svalue [sv]. Raises an exception if [v] cannot be safely converted. *)
let rec svalue_of_value = function
| VUnit -> SUnit
| VHandle h -> SHandle h
| VBool b -> SBool b
| VInt i -> SInt i
| VStr s -> SStr s
| VRef v -> SRef (svalue_of_value !v)
| VPair (l, r) -> SPair (svalue_of_value l, svalue_of_value r)
| VNil -> SNil
| VCons (v, vs) -> SCons (svalue_of_value v, svalue_of_value vs)
| VFun (p, b, env) ->
  let fvs = Free.diff (free_exp b) (kill_pat p) in
  SFun (p, b, senv_of_env env fvs)
| VFunRec (p, b, env_ref) -> begin
  let var, env' = remove_rec env_ref in
  let fvs = Free.diff (free_exp b) (kill_pat p) in
  SFunRec (var, p, b, senv_of_env env' fvs) end
| _ -> failwith "Invalid SValue"

and senv_of_env env fvs =
  env |> List.filter (fun (x, _) -> Free.mem x fvs)
      |> List.filter (fun (_, v) -> is_serializable v)
      |> List.map (fun (x, v) -> (x, svalue_of_value v))

(** [string_of_value v] is a fallible conversion from value [v] to string.
  * Raises an exception if [v] cannot be serialized.  *)
let string_of_value v =
  v |> svalue_of_value
    |> svalue_to_yojson
    |> Yojson.Safe.to_string

(** [value_of_svalue sv] is an infallible conversion from svalue [sv] to value [v]. *)
let rec value_of_svalue = function
| SUnit -> VUnit
| SHandle h -> VHandle h
| SBool b -> VBool b
| SInt i -> VInt i
| SStr s -> VStr s
| SRef v -> VRef (ref (value_of_svalue v))
| SPair (l, r) -> VPair (value_of_svalue l, value_of_svalue r)
| SNil -> VNil
| SCons (s, ss) -> VCons (value_of_svalue s, value_of_svalue ss)
| SFun (v, e, senv) -> VFun (v, e, env_of_senv senv)
| SFunRec (f, p, b, senv) -> begin
  let env = env_of_senv senv in
  let env_ref = ref env in
  let v = VFunRec (p, b, env_ref) in
  env_ref := (f, v)::env; v
  end

and env_of_senv senv =
  List.map (fun (x, v) -> (x, value_of_svalue v)) senv

(** [value_of_string s] is an infallible conversion from string [s] to value [v]. *)
let value_of_string s =
  s |> Yojson.Safe.from_string
    |> svalue_of_yojson
    |> unwrap "Invalid JSON value"
    |> value_of_svalue
