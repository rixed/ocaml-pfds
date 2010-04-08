open Pfds_intf

module Unbalanced_set_raw (Ord : ORDERED) =
struct
	(* FIXME: use a finite_map, with 'a = unit *)
	type elmt = Ord.t
	type t = E | T of (t * elmt * t)

	let empty = E

	let insert set x =
		let rec aux last = function
			| E ->
				if last = Some x then raise Exit else T (E, x, E)
			| T (l, y, r) ->
				let cmp = Ord.compare x y in
				if cmp < 0 then T (aux None l, y, r)
				else T (l, y, aux last r) in
		try aux None set with Exit -> set
	
	let rec member set x = match set with
		| E -> false
		| T (l, y, r) ->
			let cmp = Ord.compare x y in
			if cmp < 0 then member l x
			else if cmp > 0 then member r x
			else true
	
	let rec complete x depth =
		if depth = 0 then E
		else let s = complete x (depth-1) in T (s, x, s)
	
	(* return a set of arbitrary size, filled with x, with max sharing *)
	let create x n =
		let rec create2 n =
			let singleton = T (E, x, E) in
			if n = 0 then E, singleton
			else let s, s' = create2 ((n-1)/2) in
				if n land 1 = 1 then T (s, x, s), T (s, x, s')
				else T (s, x, s'), T (s',x, s') in
		fst (create2 n)

	let rec iter t f = match t with
		| E -> ()
		| T (l, x, r) ->
			iter l f ;
			f x ;
			iter r f
end

module Unbalanced_set (Ord : ORDERED) :
	SET with type elmt = Ord.t = Unbalanced_set_raw (Ord)

