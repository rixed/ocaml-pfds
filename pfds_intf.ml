exception Empty
exception Out_of_bound

module type STACK =
sig
	type 'a t

	val empty    : 'a t
	val is_empty : 'a t -> bool
	val cons     : 'a -> 'a t -> 'a t (* args should be the other way around *)
	val head     : 'a t -> 'a   (* raises Empty if the stack is empty *)
	val tail     : 'a t -> 'a t (* raises Empty if the stack is empty *)
end

module type STACK_OPS =
sig
	module Stack : STACK
	val suffixes : 'a Stack.t -> 'a Stack.t Stack.t
	val append   : 'a Stack.t -> 'a Stack.t -> 'a Stack.t
	val update   : 'a Stack.t -> int -> 'a -> 'a Stack.t
end

module type ORDERED =
sig
	type t
	val compare : t -> t -> int
end

module type SET =
sig
	type t
	type elmt

	val empty     : t
	val is_empty  : t -> bool
	val singleton : elmt -> t
	val insert    : t -> elmt -> t
	val member    : t -> elmt -> bool
	val complete  : elmt -> int -> t
	val create    : elmt -> int -> t
	val iter      : t -> (elmt -> unit) -> unit (* FIXME: include iterable *)
	(* TODO : delete... *)
end

module type FINITE_MAP =
sig
	type 'a t
	type key

	val empty  : 'a t
	val bind   : 'a t -> key -> 'a -> 'a t
	val lookup : 'a t -> key -> 'a (* raises Not_found if key is unbound *)
	val iter   : 'a t -> (key -> 'a -> unit) -> unit
end

module type HEAP =
sig
	type t
	type elmt

	val empty      : t
	val is_empty   : t -> bool
	val singleton  : elmt -> t
	val insert     : t -> elmt -> t
	val merge      : t -> t -> t
	val min        : t -> elmt (* raise Empty if t is empty *)
	val delete_min : t -> t    (* raise Empty if t is empty *)
end

module type HEAP_OPS =
sig
	module Heap : HEAP

	val of_list : Heap.elmt list -> Heap.t
	val length  : Heap.t -> int
end

(* QUEUE, DEQUEUE, CATLIST... *)
