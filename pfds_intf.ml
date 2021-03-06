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
    val fold_right : (e -> 'a -> 'a) -> t -> 'a -> 'a   (* FIXME: must iterate from right to left! *)
    val find_first : (e -> bool) -> t -> e (* may raise [Not_found] *)
    val exists     : (e -> bool) -> t -> bool

    (* Converters *)

    val to_list   : t -> e list
    val to_array  : t -> e array
end

module type ITERABLE_GEN =
sig
    type 'a t

    (* TODO: Move here index/rindex... *)

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
    val find_first : ('a -> bool) -> 'a t -> 'a (* may raise [Not_found] *)
    val exists     : ('a -> bool) -> 'a t -> bool

    (* Converter to various sources *)

    val to_list   : 'a t -> 'a list
    val to_array  : 'a t -> 'a array
    val to_string : char t -> string
    val to_file   : string -> char t -> unit
end

module type STACK_OPS =
sig
    type 'a t
    val suffixes : 'a t -> 'a t t
    val append   : 'a t -> 'a t -> 'a t
    val update   : 'a t -> int -> 'a -> 'a t
end

module type STACK_BASE =
sig
    include ITERABLE_GEN

    val cons     : 'a -> 'a t -> 'a t (* args should be the other way around *)
    val head     : 'a t -> 'a   (* raises Empty if the stack is empty *)
    val tail     : 'a t -> 'a t (* raises Empty if the stack is empty *)
end

(* Instead of letting the user build his own Stack_ops, we'd rather include it directly in the Stack module
   so that some stack implementations can overwrite functions of STACK_OPS *)
module type STACK =
sig
    include STACK_BASE
    include STACK_OPS with type 'a t := 'a t
end

module type ORDERED =
sig
    type t
    val compare : t -> t -> int
end

module type SET_OPS =
sig
    type t
    type e
    val merge : t -> t -> t
    val filter : (e -> bool) -> t -> t
end

module type SET_BASE =
sig
    include ITERABLE

    val insert : t -> e -> t
    val member : t -> e -> bool
    (* find_best as in red_black_tree? *)
    val delete : t -> e -> t
    (** [delete t x] deletes one value that compare equal with x from t, or raise [Not_found] *)
end

module type SET =
sig
    include SET_BASE
    include SET_OPS with type t := t and type e := e
end

module type SET_GEN =
sig
    include ITERABLE_GEN

    val insert : 'a t -> 'a -> 'a t
    val member : 'a t -> 'a -> bool
end

module type FINITE_MAP =
sig
    type 'a t
    type key

    val empty      : 'a t
    val is_empty   : 'a t -> bool
    val length     : 'a t -> int
    val bind       : 'a t -> key -> 'a -> 'a t
    (** [bind m k x] returns the same map as [m] but with a new binding of [k] to [x].
        Previous binding of [k] in [m] is lost. *)

    val unbind_exn : 'a t -> key -> 'a t
    (** raises [Not_found] if key is unbound. *)

    val unbind     : 'a t -> key -> 'a t
    val lookup     : 'a t -> key -> 'a (* raises [Not_found] if key is unbound *)
    val iter       : 'a t -> (key -> 'a -> unit) -> unit
    val fold_left  : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
    (** [fold_left f v m] will return [f (... (f (f (f v k1 x1) k2 x2) k3 x3) ...) kmax xmax], ie. it will call f for all values
        in ascending order. *)

    val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold_right f m v] will proceed in descending order *)

    val update     : 'a t -> key -> ('a -> 'a) -> 'a t
    (** [update m k f] returns a map which value x for k is replaced with [f x]
        (or same map if k is unbound) *)

    val update_exn : 'a t -> key -> ('a -> 'a) -> 'a t
    (** same as [update], but raise [Not_found] if key is unbound. *)

    val update_with_default
                   : 'a -> 'a t -> key -> ('a -> 'a) -> 'a t
    (** [update_with_default d m k f] is the same as [update m k f] if [k] is bound in [m], but a new mapping from [k] to [d] is added
        is k is unbound. *)

    val update_with_default_delayed
                   : (unit -> 'a) -> 'a t -> key -> ('a -> 'a) -> 'a t
    (** [update_with_default_delayed d m k f] is the same as [update_with_default d m k f] but d is now a function returning the default value.
        Useful if this default value is expensive to compute. *)

    val map        : (key -> 'a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same keys than [m] but values transformed through [f]. *)

    val filter     : (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter f m] returns the same map as [m] but with all keys dissatisfying [f] removed. *)

    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    (** action of filter and map combined. *)
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
    val delete_min : t -> t (* raises Empty if t is empty *)
    (*val member     : t -> e -> bool*)
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
    (* FIXME: make these (and possibly others) 'a -> 'a t -> 'a t so that it's easier to queue operations *)
    (* Insert the element before the current one, and focus it *)
    val insert_before : 'a t -> 'a -> 'a t
    (* Insert the element after the current one, and focus it *)
    val insert_after  : 'a t -> 'a -> 'a t
    (* Remove the focused element, and focus on the one after it *)
    val remove        : 'a t -> 'a t
    (* Same as iter, fold_left, etc, but call function with whole ring instead of the single element *)
    val iterr         : ('a t -> unit) -> 'a t -> unit
    val iterir        : (int -> 'a t -> unit) -> 'a t -> unit
    val iteri         : (int -> 'a -> unit) -> 'a t -> unit
    val fold_leftr    : ('b -> 'a t -> 'b) -> 'b -> 'a t -> 'b
    val fold_rightr   : ('a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type SORTLIST_GEN =
sig
    include ITERABLE_GEN

    val insert : ('a -> 'a -> bool) -> 'a t -> 'a -> 'a t (* comp function is less-than *)
    (* Remove the first entry for which the given function returns true.
     * Will do nothing of the entry can't be found. *)
    val remove : ('a -> bool) -> 'a t -> 'a t
end

module type ROPE_GEN =
sig
    include ITERABLE_GEN

    val cat       : 'a t -> 'a t -> 'a t
    val cut       : 'a t -> int -> int -> 'a t
    val insert    : 'a t -> int -> 'a t -> 'a t
    val sub       : 'a t -> int -> int -> 'a t
    val nth       : 'a t -> int -> 'a
    val count     : 'a t -> 'a -> int
    val index     : 'a t -> 'a -> int
    val rindex    : 'a t -> 'a -> int

    val of_list   : 'a list -> 'a t
    val of_array  : 'a array -> 'a t
    val of_string : string -> char t
    val of_func   : int -> (int -> 'a) -> 'a t
    val of_file   : string -> char t
end

