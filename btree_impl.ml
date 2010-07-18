open Pfds_intf

module Unbalanced_set_raw (Ord : ORDERED) =
struct
	(* FIXME: use a finite_map, with 'a = unit *)
	type elmt = Ord.t
	type t = E | T of (t * elmt * t)

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
	SET with type elmt = Ord.t = Unbalanced_set_raw (Ord)

