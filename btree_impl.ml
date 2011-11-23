open Pfds_intf

module Unbalanced_set_raw (Ord : ORDERED) =
struct
	(* FIXME: use a finite_map, with 'a = unit *)
	type e = Ord.t
	type t = E | T of (t * e * t)

	let empty = E
	let is_empty = function
		| E -> true
		| _ -> false
	let singleton x = T (E, x, E)

	let insert set x =
		let rec aux last = function
			| E ->
				if last = Some x then raise Exit else singleton x
			| T (l, y, r) ->
				let cmp = Ord.compare x y in
				if cmp < 0 then T (aux last l, y, r)
				else T (l, y, aux (Some y) r) in
		try aux None set with Exit -> set
	
	let member set x =
		let rec aux last = function
			| E -> last = Some x
			| T (l, y, r) ->
				let cmp = Ord.compare x y in
				if cmp < 0 then aux last l
				else aux (Some y) r in
		aux None set

	let delete set x =
		(* return the min and the tree without the min *)
		let rec del_min = function
			| E -> failwith "Should not happen"
			| T (E, y, r) -> y, r
			| T (l, y, r) ->
				let m, l' = del_min l in
				m, T (l', y, r) in
		(* delete the root of the given tree *)
		let del_root = function
			| E -> failwith "Should not happen"
			| T (l, _, E) -> l
			| T (E, _, r) -> r
			| T (l, _, r) ->
			(* TODO: alternatively, instead of replacing the root with its successor
			   replace it by its predecessor (mirroring del_min).
			   See Knuth TAOCPv3, p435, to know why *)
				let m, r' = del_min r in
				T (l, m, r') in
		(* locate the node to be deleted then delete it *)
		let rec find_del = function
			| E -> raise Not_found
			| T (l, y, r) as n ->
				let cmp = Ord.compare x y in
				if cmp < 0 then T (find_del l, y, r)
				else if cmp > 0 then T (l, y, find_del r)
				else del_root n in
		find_del set
	
	let rec complete x depth =
		if depth = 0 then E
		else let s = complete x (depth-1) in T (s, x, s)
	
	(* return a set of arbitrary size, filled with x, with max sharing *)
	let create x n =
		let rec create2 n =
			if n = 0 then E, singleton x
			else let s, s' = create2 ((n-1)/2) in
				if n land 1 = 1 then T (s, x, s), T (s, x, s')
				else T (s, x, s'), T (s',x, s') in
		fst (create2 n)

	let rec iter f = function
		| E -> ()
		| T (l, x, r) ->
			iter f l ;
			f x ;
			iter f r

	include Iterable_impl.Iterable_from_iter (struct type t' = t type e' = e let iter = iter end)
end

module Unbalanced_set (Ord : ORDERED) :
	SET with type e = Ord.t = Set_ops_impl.Make (Unbalanced_set_raw (Ord))

