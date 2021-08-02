open Types

(** [print v] returns the string representation of [v]. 
 * Special representations:
 * 
 * Function closures: <function>
 * References: <reference : v> 
 * Promises: <promise resolved : v> or <promise pending>
 * Handles: <handle : n>
 * *)
val print : value -> string

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
val print_detail1 : value -> string

(** [print_detail2 v] is the same as [print_detail1 v] except function environments 
 * are represented as: 
 * 
 * env : [
 *     (x1, v1);
 *     ...     ;
 *     (xn, vn)]
 *
 * Where any [vi] in [env] that is a function closure is represented as <function>. *)
val print_detail2 : value -> string

(** [print_detail3 v] is the same as [print_detail1 v] except function closures 
 * in environments are fully recursively printed out.  *)
val print_detail3 : value -> string