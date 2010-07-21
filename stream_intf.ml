module type STREAM =
sig
	type 'a t = 'a cell Lazy.t
	and 'a cell = Nil | Cons of 'a * 'a t

	val empty        : 'a t
	val is_empty     : 'a t -> bool

	(* Convertion / Creation *)
	val singleton    : 'a -> 'a t
	val to_list_rev  : 'a t -> 'a list
	val to_list      : 'a t -> 'a list
	val of_list      : 'a list -> 'a t
	val of_succ      : ('a -> 'a) -> 'a -> 'a t

	(* List like interface *)
	val head         : 'a t -> 'a
	val tail         : 'a t -> 'a t
	val cons         : 'a -> 'a t -> 'a t
	val nth          : 'a t -> int -> 'a
	val length       : 'a t -> int
	val cat          : 'a t -> 'a t -> 'a t
	val map          : 'a t -> ('a -> 'b) -> 'b t
	val map2         : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
	val mapi         : 'a t -> (int -> 'a -> 'b) -> 'b t
	val iter         : 'a t -> ('a -> unit) -> unit
	val fold         : 'a t -> 'b -> ('a -> 'b -> 'b) -> 'b
	val filter       : 'a t -> ('a -> bool) -> 'a t
	
	(* Generators *)
	val cycle        : 'a t -> 'a t
	val repeat       : ?count:int -> 'a -> 'a t
	val stammer      : int -> 'a t -> 'a t
	val dropwhile    : ('a -> bool) -> 'a t -> 'a t
	val takewhile    : ('a -> bool) -> 'a t -> 'a t
	val skip         : int -> 'a t -> 'a t
	val one_every    : int -> 'a t -> 'a t
	val slice        : ?step:int -> ?dropwhile:('a -> bool) -> ?takewhile:('a -> bool) -> 'a t -> 'a t
	val zip2         : 'a t -> 'b t -> ('a * 'b) t
	val zip2_longest : 'a t -> 'b t -> 'a -> 'b -> ('a * 'b) t
	val altern2      : 'a t -> 'a t -> 'a t
	val firsts       : int -> 'a t -> 'a t
	val lasts        : int -> 'a t -> 'a t
	val groupby      : 'a t -> ('a -> 'b) -> ('b * 'a t) t (* group consecutive similar values *)
	val uniq         : 'a t -> ('a -> 'b) -> 'a t (* filter out consecutive similar values *)
	
	(* Combinatoric generators *)
	val zip          : 'a t t -> 'a t t (* {{a,b,..},{c,d,..},{e,f,..}} -> {{a,c,e},{b,d,f},...} *)
	val product      : 'a t t -> 'a t t (* {{1,2,3},{3,4}} -> {{1,3},{1,4},{2,3},{2,4},{3,3},..} *)
	val permutations : ?len:int -> 'a t -> 'a t t (* {1,2,3} -> {{1,2,3},{1,3,2},{2,1,3},..} *)
	val combinations : int -> 'a t -> 'a t t (* {1,2,3},2 -> {{1,2},{1,3},{2,3}} *)
end

