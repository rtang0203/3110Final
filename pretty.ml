open Types

let sprintf = Printf.sprintf

let tab = "  "

(** [indent lvl] Creates an indent of size [lvl] *)
let rec indent lvl = if lvl = 0 then "" else tab ^ indent (lvl - 1)

(** [lst_of_vlst] converts a [VList] to a [value list] *)
let rec lst_of_vlst = function 
  | VNil -> []
  | VCons (v, vs) -> v::(lst_of_vlst vs)
  | _ -> failwith "Pretty Printer: Expected List"

(** [lst_of_elst] converts a [List] to an [exp list] *)
let rec lst_of_elst = function 
  | Nil -> []
  | Cons (e, es) -> e::(lst_of_elst es)
  | _ -> failwith "Pretty Printer: Expected List"

(** [pretty_lst_general] converts an ['a list] to a string, given a 
 * current indentation level,  an ['a] printer, the list, and wether a new line
 * should be made. *)
let pretty_lst_general (lvl : int) (print_a : int -> 'a -> string) (alst : 'a list) (is_endline : bool) = 
  let endline = if is_endline then "\n" ^ (indent (lvl+1)) else "" in
  let rec pretty_inner = function 
    | [] -> ""
    | [a] -> endline^print_a (lvl + 1) a
    | a::t -> endline^(print_a (lvl+1) a)^"; "^(pretty_inner t) in 
  "["^(pretty_inner alst)^"]"

(** [pretty_lst_endline] is like [pretty_lst_general] but with an endline. *)
let pretty_lst_endline (lvl : int) (print_a : int -> 'a -> string) (alst : 'a list) = 
  pretty_lst_general lvl print_a alst true

(** [pretty_lst] is like [pretty_lst_general] but without an endline. *)
let pretty_lst (print_a : 'a -> string) (alst : 'a list) = 
  pretty_lst_general (-10000)(fun int -> print_a) alst false

(** [pretty_print] converts a value to a string, but given function printer, 
 * a current indentation level, and the value. *)
let rec pretty_print_general (pf : int -> value -> string) (lvl : int) (v : value) = 
  match v with 
  | VUnit -> "()"
  | VBool b -> if b then "true" else "false" 
  | VInt n -> string_of_int n 
  | VStr s -> "\""^s^"\"" 
  | VFun _ -> pf lvl v
  | VFunRec _ -> pf lvl v
  | VDwt p -> pretty_dwt lvl pf p
  | VRef r -> sprintf "<reference : (%s)>" (pretty_print_general pf lvl !r)
  | VPair (v1, v2) -> sprintf "(%s, %s)" 
                        (pretty_print_general pf lvl v1) (pretty_print_general pf lvl v2)
  | VNil | VCons _ -> pretty_lst_endline lvl (pretty_print_general pf) (lst_of_vlst v)
  | VHandle h -> sprintf "<handle : %d>" h 

(** [pretty_dwt] converts a [Dwt.t] promise to a string, given a current indentation 
 * level and a function printer and the promise. *)
and pretty_dwt (lvl : int) (pf : int -> value -> string) (p : value Dwt.t) = 
  let open Lwt in
  match state (Dwt.danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a8 p) with 
  | Return v -> 
    sprintf "<promise resolved : %s>" (pretty_print_general pf lvl v)
  | Sleep -> "<promise pending>" 
  | Fail _ -> "<promise rejected>" 

(** [pretty_print] converts a value to a string (per RML's `print` specification.) *)
let pretty_print (lvl : int) (v : value) = 
  pretty_print_general (fun lvl v -> "<function>") lvl v

(** [pretty_pat] converts a pattern to a string. *)
let rec pretty_pat (p : pat) = match p with 
  | PUnit -> "()"
  | PWild -> "_"
  | PBool b -> if b then "true" else "false"
  | PInt n -> string_of_int n 
  | PStr s -> "\""^s^"\"" 
  | PVar v -> v
  | PPair (p1, p2) -> sprintf "(%s, %s)" (pretty_pat p1) (pretty_pat p2)
  | PCons (p1, p2) -> sprintf "(%s::%s)" (pretty_pat p1) (pretty_pat p2)
  | PNil -> "[]"

(** [pretty_binop] converts a binop to a string. *)
let pretty_binop (op : bin) = match op with 
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " % "
  | And -> " && "
  | Or -> " || "
  | Lt -> " < "
  | Le -> " <= "
  | Gt -> " > "
  | Ge -> " >= "
  | Eq -> " = "
  | Ne -> " <> "
  | Cat -> " ^ "

(** [pretty_unop] converts a unop to a string. *)
let pretty_unop (op : una) = match op with 
  | Neg -> "-"
  | Not -> "not "

(** [paren] encloses a string with parents. *)
let paren (s : string) = "("^s^")"

(** [pretty_exp] converts an [exp] to a string, given a current indentation level 
 * and the expression.  *)
let rec pretty_exp (lvl : int) (e : exp) = match e with 
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Int n -> string_of_int n 
  | Str s -> s 
  | Var v -> v 
  | Una (op, e) -> pretty_unop op ^ pretty_exp lvl e |> paren
  | Bin (op, e1, e2) -> pretty_exp lvl e1 ^ pretty_binop op ^ pretty_exp lvl e2 |> paren
  | Pair (e1, e2) -> sprintf "(%s, %s)" (pretty_exp lvl e1) (pretty_exp lvl e2)
  | Nil | Cons _ -> pretty_lst_endline lvl (pretty_exp) (lst_of_elst e)
  | Fun (p, b) -> sprintf "(fun %s -> \n%s%s)" (pretty_pat p) (indent (lvl + 1)) (pretty_exp (lvl + 1) b)
  | App (e1, e2) -> pretty_exp lvl e1 ^ " " ^ pretty_exp lvl e2 |> paren
  | Let (p, b, e) -> sprintf "let %s = %s in \n%s%s" 
                       (pretty_pat p) (pretty_exp lvl b) (indent lvl) (pretty_exp lvl e) |> paren
  | LetRec (v, b, e) -> sprintf "let rec %s = %s in \n%s" 
                          v (pretty_exp lvl b) (pretty_exp lvl e) |> paren
  | Seq (e1, e2) -> pretty_exp lvl e1 ^ "; \n" ^ pretty_exp lvl e2 |> paren
  | IfThen (b, t, f) -> sprintf "if %s \n%sthen %s \n%selse %s" 
                          (pretty_exp lvl b) (indent (lvl+1)) (pretty_exp (lvl+1) t) 
                          (indent (lvl+1)) (pretty_exp (lvl+1) f) |> paren
  | Match (e, cases) -> sprintf "match %s with %s" 
                          (pretty_exp lvl e) (pretty_cases lvl cases) |> paren
  | Assign (l, r) -> pretty_exp lvl l ^ " := " ^ pretty_exp lvl r |> paren
  | Ref e -> "ref " ^ pretty_exp lvl e |> paren
  | Deref e -> "!" ^ pretty_exp lvl e |> paren
  | Await (p, b, e) -> sprintf "await %s = %s in \n%s%s" 
                         (pretty_pat p) (pretty_exp lvl b) (indent lvl) (pretty_exp lvl e) |> paren
  | Spawn (f, a) -> sprintf "spawn %s with %s"
                      (pretty_exp lvl f) (pretty_exp lvl a) |> paren 
  | Send (e, h) -> sprintf "send %s to %s"
                     (pretty_exp lvl e) (pretty_exp lvl h) |> paren 
  | Recv e -> "recv " ^ pretty_exp lvl e |> paren 
  | Join e -> "join " ^ pretty_exp lvl e |> paren 
  | Pick e -> "pick " ^ pretty_exp lvl e |> paren 
  | Return e -> "return " ^ pretty_exp lvl e |> paren

(** [pretty_cases] converts match statement cases to a string. *)
and pretty_cases (lvl : int) (cases : (pat * exp) list) = match cases with 
  | [] -> sprintf "\n%send" (indent lvl)
  | (p, e)::t -> (sprintf "\n%s| %s -> %s " (indent (lvl + 1)) (pretty_pat p) (pretty_exp (lvl+1) e))
                 ^ pretty_cases lvl t

(** [remove_overshadowed_vars env] removes overshadowed duplicate entries in [env] *)
let rec remove_overshadowed_vars env = 
  let rec remove_key_all k = function 
    | [] -> [] 
    | ((k', _) as h)::t -> begin 
        let tail = remove_key_all k t in 
        if k = k' then tail else h::tail end in
  match env with 
  | [] -> [] 
  | ((k, v) as h)::t -> begin
      let tail = remove_key_all k t in 
      h::(remove_overshadowed_vars tail) end

(** [pretty_env] converts a functions environment to a string, given a value 
 * printer and the current indentation level and the environment. *)
let pretty_env (pv : int -> value -> string) (lvl : int) (env : env) =  
  let env = remove_overshadowed_vars env in
  let pretty_env_binding lvl (var, value) = 
    sprintf "(%s, %s)" var (pv lvl value) in 
  pretty_lst_endline lvl pretty_env_binding env

(** [print_function_general] converts a function closure to a string given an environment 
 * printer and the current indentation level and the function value. *)
let rec print_function_general (pe : int -> env -> string) (lvl : int) (v : value) = match v with 
  | VFun (p, b, env) -> sprintf "<function : {\n%spattern : %s; \n%sbody : %s; \n%senv: %s} >" 
                          (indent (lvl+1)) 
                          (pretty_pat p) (indent (lvl+1)) (pretty_exp (lvl+2) b) (indent (lvl+1)) (pe (lvl+2) env)
  | VFunRec (p, b, env_ref) -> begin let (var, env) = Serialize.remove_rec env_ref in 
      sprintf "<recursive function : {\n%sname : %s; \n%spattern : %s; \n%sbody : %s; \n%senv : %s}>" 
        (indent (lvl+1)) var (indent (lvl+1)) 
        (pretty_pat p) (indent (lvl+1)) (pretty_exp (lvl+2) b) (indent (lvl+1)) (pe (lvl+2) env) end
  | _ -> failwith "Impossible (passed non-function to print_function)"

(** [pretty_print_detail1] converts a value to a string, per RML's [print_detail1]
 * specification. *)
let pretty_print_detail1 (lvl : int) (v : value) = 
  pretty_print_general (print_function_general (fun lvl env -> "<env>")) lvl v

(** [pretty_print_detail2] converts a value to a string, per RML's [print_detail2]
 * specification. *)
let pretty_print_detail2 (lvl : int) (v : value) = 
  pretty_print_general (print_function_general (pretty_env pretty_print)) lvl v

(** [pretty_print_detail3] converts a value to a string, per RML's [print_detail3]
 * specification. *)
let rec pretty_print_detail3 (lvl : int) (v : value) = 
  pretty_print_general (print_function_general (pretty_env pretty_print_detail3)) lvl v

(** [print v] returns the string representation of [v]. 
 * Special representations:
 * 
 * Function closures: <function>
 * References: <reference : v> 
 * Promises: <promise resolved : v> or <promise pending>
 * Handles: <handle : n>
 * *)
let print (v : value) = pretty_print 0 v 

(** [print_detail1 v] is the same as [print v] except function closures are 
 * represented as:
 * 
 * <function : {
 *     pattern : p; 
 *     body : e; 
 *     env : env } >  
 * 
 * and recursive function closures are representd as
 *
 * <recursive function : {
 *     name : x;
 *     pattern : p; 
 *     body : e; 
 *     env : env } >  
 * 
 * Where [env] is represented as <env> *)
let print_detail1 (v : value) = pretty_print_detail1 0 v 

(** [print_detail2 v] is the same as [print_detail1 v] except function environments 
 * are represented as: 
 * 
 * env : [
 *     (x1, v1);
 *     ...     ;
 *     (xn, vn)]
 *
 * Where any [vi] in [env] that is a function closure is represented as <function>. *)
let print_detail2 (v : value) = pretty_print_detail2 0 v 

(** [print_detail3 v] is the same as [print_detail1 v] except function closures 
 * in environments are fully recursively printed out.  *)
let print_detail3 (v : value) = pretty_print_detail3 0 v 
