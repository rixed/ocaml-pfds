open Pfds_intf

module Red_black_tree_raw (Ord : ORDERED) =
struct
	type elmt = Ord.t
	type color = R | B
	type t = E | T of (color * t * elmt * t)

	let empty = E
	let is_empty = function E -> true | _ -> false
	let singleton x = T (B, E, x, E)

	let rec member t x = match t with
		| E -> false
		| T (_, l, y, r) ->
			let cmp = Ord.compare x y in
			if cmp < 0 then member l x
			else if cmp > 0 then member r x
			else true
	
	let check_invariants t =
		let rec chk prec_col tot_B = function
			| E -> tot_B
			| T (R, l, _, r) ->
				if prec_col = R then failwith "2 successive Reds" ;
				let depth_l = chk R tot_B l
				and depth_r = chk R tot_B r in
				if depth_l <> depth_r then failwith "Not same Black count (R)" ;
				depth_l
			| T (B, l, _, r) ->
				let depth_l = chk B (tot_B+1) l
				and depth_r = chk B (tot_B+1) r in
				if depth_l <> depth_r then failwith "Not same Black count (B)" ;
				depth_l in
		ignore (chk B 0 t)

	let balance_l = function
		| B, T (R, a, x, T (R, b, y, c)), z, d
		| B, T (R, T (R, a, x, b), y, c), z, d ->
			T (R, T (B, a, x, b), y, T (B, c, z, d))
		| body -> T body
	
	let balance_r = function
		| B, a, x, T (R, b, y, T (R, c, z, d))
		| B, a, x, T (R, T (R, b, y, c), z, d) ->
			T (R, T (B, a, x, b), y, T (B, c, z, d))
		| body -> T body
	
	let insert t x =
		let rec ins = function
			| E -> T (R, E, x, E)
			| T (color, l, y, r) ->
				let cmp = Ord.compare x y in
				if cmp < 0 then balance_l (color, ins l, y, r)
				else if cmp > 0 then balance_r (color, l, y, ins r)
				else raise Exit in
		try match ins t with
			| T (_, l, y, r) -> T (B, l, y, r)
			| _ -> failwith "Someone messed up the compiler"
		with Exit -> t
	
	let insert' t x =
		let t' = insert t x in
		check_invariants t' ;
		t'
	
	let rec iter f = function
		| E -> ()
		| T (_, l, x, r) ->
			iter f l ;
			f x ;
			iter f r
	
	include Iterable_impl.Iterable_from_iter (struct type t' = t type e' = e let iter = iter end)
end

module Red_black_tree (Ord : ORDERED) :
	SET with type elmt = Ord.t = Red_black_tree_raw (Ord)
