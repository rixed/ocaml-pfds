open Pfds_intf

module Leftist_heap_raw (Ord : ORDERED) =
struct
	type elmt = Ord.t
	type t = E | T of int * elmt * t * t

	let empty = E
	let is_empty = function E -> true | _ -> false
	let singleton x = T (1, x, E, E)

	let rank = function
		| E -> 0
		| T (r, _, _, _) -> r
	
	(* For some reasons, this merge function is exponential... *)
	let rec merge a b = match a with
		| E -> b
		| T (_, x, a_l, a_r) -> (match b with
			| E -> a
			| T (_, y, b_l, b_r) ->
				let makeT v l r =
					let rank_l, rank_r = rank l, rank r in
					if rank_l >= rank_r then T (rank_l + 1, v, l, r)
					else T (rank_r + 1, v, r, l) in
				if Ord.compare x y <= 0 then makeT x a_l (merge a_r b)
				else makeT y b_l (merge a b_r))
	
	let insert a x = merge a (singleton x)
	let min = function E -> raise Empty | T (_, x, _, _) -> x
	let delete_min = function E -> raise Empty | T (_, _, l, r) -> merge l r
end

module Leftist_heap (Ord : ORDERED) :
	HEAP with type elmt = Ord.t = Leftist_heap_raw(Ord)

