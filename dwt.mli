type 'a t

(** [return v] returns a promise that is already resolved with [v]. *)
val return : 'a -> 'a t

(** Exactly the same specification as Lwt.bind. (You may use the infix operator [>>=].)
  * If [p] is already resolved, then [f] is run immediately on the contents of [p].
  * If [p] is pending, then [bind] registers [f] as a callback to eventually be run
  * when (or if) the promise is resolved. A new promise is immediately returned that
  * will in the future resolve to the result of the callback. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [join [p1; ...; pn]] returns a single [t] that resolves to a list of
  * values [v1; ...; vn], where each [pi] resolves to [vi]. *)
val join : 'a t list -> 'a list t

(** [pick [p1; ...; pn]] returns a single [t] that resolves to the first
  * value [vi] whose promise [pi] resolves. *)
val pick : 'a t list -> 'a t

(** [send v h] sends [v] asynchronously to the server with handle [h]. The value 
  * represented by [v] should be serialized to string via [Serialize.string_of_value] 
  * before being passed to [send]. ([h] can be extracted from a [VHandle].)
  * 
  * Requires:
  * - [h] is a valid handle received from [spawn], or equal to [SELF] or [SERVER]. *)
val send : string -> int -> unit

(** [recv h] is a promise representing the next value sent from the server with handle [h].
  * The promise will resolve to a string representation of the value, which should 
  * be deserialized back to a value via [Serialize.value_of_string]. 
  * 
  * Requires:
  * - [h] is a valid handle received from [spawn], or equal to [SELF] or [SERVER]. *)
val recv : int -> string t

(** [spawn f a] is the port number [h] of a new robot running program [f] with argument [a].
  * The function represented by [f] and the argument represented by [a] should 
  * be serialized to string via [Serialize.string_of_value] before being 
  * passed to [spawn]. The returned integer should be wrapped into a [VHandle]
  *
  * This call is blocking, guaranteeing that [send] and [recv] calls to [h] will
  * be valid after it returns. *)
val spawn : string -> string -> int

(** [sleep ms] is a promise that will resolve to unit after [ms] milliseconds. *)
val sleep : int -> unit t 

(** Convenience module for providing infix syntax. *)
module Infix : sig
  (** Infix operator for [bind]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
end

(** Staff use only *)
val danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a8 : 'a t -> 'a Lwt.t