type 'a t = 'a cell Lazy.t
and 'a cell = Nil | Cons of 'a * 'a t

val empty        : 'a t
val is_empty     : 'a t -> bool
val is_singleton : 'a t -> bool

(* Conversion / Creation *)
val singleton    : 'a -> 'a t
val to_list_rev  : 'a t -> 'a list
val to_list      : 'a t -> 'a list
val to_string    : char t -> string
val to_array     : 'a t -> 'a array
val unfold       : ('b -> 'a * 'b) -> 'b -> 'a t
val of_list      : 'a list -> 'a t
val of_succ      : ('a -> 'a) -> 'a -> 'a t
val of_nth       : ?limit:int -> (int -> 'a) -> 'a t
val of_string    : string -> char t
val of_array     : 'a array -> 'a t
val of_file      : in_channel -> string t
val of_directory : string -> string t
val of_fun       : (unit -> 'a) -> 'a t (* until f raises Exit *)
val nat          : int t
val range        : int -> int -> int t

(* List like interface *)
val head         : 'a t -> 'a
val tail         : 'a t -> 'a t
val cons         : 'a -> 'a t -> 'a t
val nth          : 'a t -> int -> 'a
val length       : 'a t -> int
val cat          : 'a t -> 'a t -> 'a t
val append       : 'a t -> 'a -> 'a t
val prepend      : 'a -> 'a t -> 'a t
val map          : 'a t -> ('a -> 'b) -> 'b t
val map2         : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
val mapi         : 'a t -> (int -> 'a -> 'b) -> 'b t
val map_opt      : 'a t -> ('a -> 'b option) -> 'b t
val iter         : 'a t -> ('a -> unit) -> unit
val iteri        : 'a t -> (int -> 'a -> unit) -> unit
val fold         : 'a t -> 'b -> ('a -> 'b -> 'b) -> 'b
val reduce       : 'a t -> ('a -> 'a -> 'a) -> 'a
val filter       : 'a t -> ('a -> bool) -> 'a t
val mask         : 'a t -> ('a -> bool) -> 'a -> 'a t
val exists       : 'a t -> ('a -> bool) -> bool
val mem          : 'a t -> 'a -> bool

(* Comparison *)
val cmp          : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val cmp_shortest : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(* same as above but stop comparing elements as soon as one of the stream is empty *)
val merge        : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
(* [merge cmp a b] returns the stream composed of elements of a and b interleaved such that
 * the cmp function yield >= 0 for any two successive elements of the result stream *)

(* Generators *)
val cycle        : 'a t -> 'a t
val repeat       : ?count:int -> 'a -> 'a t
val stammer      : int -> 'a t -> 'a t
val dropwhile    : ('a -> bool) -> 'a t -> 'a t
val takewhile    : ('a -> bool) -> 'a t -> 'a t
val skip         : int -> 'a t -> 'a t
val one_every    : int -> 'a t -> 'a t
val slice        : ?step:int -> ?dropwhile:('a -> bool) -> ?takewhile:('a -> bool) -> 'a t -> 'a t
val zip2         : 'a t -> 'b t -> ('a * 'b) t (* stops as soon as one stream stops *)
val zip2_longest : 'a t -> 'b t -> 'a -> 'b -> ('a * 'b) t (* use default values whenever a stream stops, until both stop *)
val altern2      : 'a t -> 'a t -> 'a t
val firsts       : int -> 'a t -> 'a t
val lasts        : int -> 'a t -> 'a t
val groupby      : 'a t -> ('a -> 'b) -> ('b * 'a t) t (* group consecutive similar values *)
val uniq         : 'a t -> ('a -> 'b) -> 'a t (* filter out consecutive similar values *)
val choose       : int -> 'a t -> 'a t (* choose n items randomly *)

(* Combinatoric generators *)
val zip          : 'a t t -> 'a t t (* {{a,b,..},{c,d,..},{e,f,..}} -> {{a,c,e},{b,d,f},...} *)
val product      : 'a t t -> 'a t t (* {{1,2,3},{3,4}} -> {{1,3},{1,4},{2,3},{2,4},{3,3},..} *)
val power        : 'a t -> int -> 'a t t
val permutations : ?len:int -> 'a t -> 'a t t (* {1,2,3} -> {{1,2,3},{1,3,2},{2,1,3},..} *)
val combinations : int -> 'a t -> 'a t t (* {1,2,3},2 -> {{1,2},{1,3},{2,3}} *)
val flatten      : 'a t t -> 'a t (* {{1,2,3},{4,5},...} -> {1,2,3,4,5,...} *)

(* Utilities *)
val (--)         : int -> int -> int t  (* Same as range *)
val (//)         : 'a t -> ('a -> bool) -> 'a t (* Same as filter *)
val (++)         : 'a t -> 'a t -> 'a t (* Same as cat *)
val (/@)         : 'a t -> ('a -> 'b) -> 'b t (* Same as map *)
val (//@)        : 'a t -> ('a -> 'b option) -> 'b t (* Same as map_opt *)
val (+@+)        : 'a t -> ('a -> 'b t) -> 'b t (* Same as map |> flatten *)
