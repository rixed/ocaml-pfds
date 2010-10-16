exception Empty
exception Out_of_bound

module type ITERABLE =
sig
	type t
	type e

	val empty      : t
	val is_empty   : t -> bool
	val singleton  : e -> t
	val length     : t -> int
	val iter       : (e -> unit) -> t -> unit
	val iteri      : (int -> e -> unit) -> t -> unit
	val fold_left  : ('a -> e -> 'a) -> 'a -> t -> 'a
	val fold_right : (e -> 'a -> 'a) -> t -> 'a -> 'a
end

module type ITERABLE_GEN =
sig
	type 'a t

	val empty      : 'a t
	val is_empty   : 'a t -> bool
	val singleton  : 'a -> 'a t
	val length     : 'a t -> int
	val iter       : ('a -> unit) -> 'a t -> unit
	val iteri      : (int -> 'a -> unit) -> 'a t -> unit
	val map        : ('a -> 'b) -> 'a t -> 'b t
	val mapi       : (int -> 'a -> 'b) -> 'a t -> 'b t
	val fold_left  : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
	val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type STACK =
sig
	include ITERABLE_GEN

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
	include ITERABLE

	val insert : t -> e -> t
	val member : t -> e -> bool
	(* TODO : delete... *)
end

module type SET_GEN =
sig
	include ITERABLE_GEN

	val insert : 'a t -> 'a -> 'a t
	val member : 'a t -> 'a -> bool
end

module type FINITE_MAP =
sig
	(* FIXME : use ITERABLE *)
	type 'a t
	type key

	val empty  : 'a t
	val bind   : 'a t -> key -> 'a -> 'a t
	val lookup : 'a t -> key -> 'a (* raises Not_found if key is unbound *)
	val iter   : 'a t -> (key -> 'a -> unit) -> unit
end

module type HEAP =
sig
	(* FIXME : use ITERABLE *)
	type t
	type e

	val empty      : t
	val is_empty   : t -> bool
	val singleton  : e -> t
	val insert     : t -> e -> t
	val merge      : t -> t -> t
	val min        : t -> e (* raises Empty if t is empty *)
	val delete_min : t -> t    (* raises Empty if t is empty *)
end

module type HEAP_OPS =
sig
	module Heap : HEAP

	val of_list : Heap.e list -> Heap.t
	val length  : Heap.t -> int
end

module type QUEUE =
sig
	(* FIXME : use ITERABLE *)
	type 'a t

	val empty     : 'a t
	val is_empty  : 'a t -> bool
	val singleton : 'a -> 'a t
	val snoc      : 'a t -> 'a -> 'a t
	val head      : 'a t -> 'a    (* raises Empty if t is empty *)
	val tail      : 'a t -> 'a t  (* raises Empty if t is empty *)
end

module type DEQUEUE =
sig
	(* FIXME : use ITERABLE *)
	type 'a t

	val empty     : 'a t
	val is_empty  : 'a t -> bool
	val singleton : 'a -> 'a t

	val cons      : 'a t -> 'a -> 'a t
	val head      : 'a t -> 'a   (* raises Empty if t is empty *)
	val tail      : 'a t -> 'a t (* raises Empty if t is empty *)

	val snoc      : 'a t -> 'a -> 'a t
	val last      : 'a t -> 'a    (* raises Empty if t is empty *)
	val init      : 'a t -> 'a t  (* raises Empty if t is empty *)
end	

module type RING_GEN =
sig
	include ITERABLE_GEN

	val get           : 'a t -> 'a
	val next          : 'a t -> 'a t
	val prev          : 'a t -> 'a t
	val insert_before : 'a t -> 'a -> 'a t
	val insert_after  : 'a t -> 'a -> 'a t
	val remove        : 'a t -> 'a t
	(* Same as iter, fold_left, etc, but call function with whole ring instead of the single element *)
	val iterr         : ('a t -> unit) -> 'a t -> unit
	val iterir        : (int -> 'a t -> unit) -> 'a t -> unit
	val fold_leftr    : ('b -> 'a t -> 'b) -> 'b -> 'a t -> 'b
	val fold_rightr   : ('a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type SORTLIST_GEN =
sig
	include ITERABLE_GEN

	val insert : 'a t -> 'a -> 'a t
	val remove : 'a t -> 'a -> 'a t
end

(* CATLIST... *)
