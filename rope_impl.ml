open Pfds_intf

module Make_raw =
struct
	(* ITERABLE_GEN *)

	type 'a t =
		| Leaf of 'a array
		| Cat of (int (* total length *) * 'a t (* left part *) * 'a t (* right part *))

	let empty = Leaf [||]

	let is_empty = function
		| Leaf [||] -> true
		| _ -> false
	
	let singleton e = Leaf [| e |]

	let length = function
		| Leaf a -> Array.length a
		| Cat (n, _, _) -> n
	
	let iteri f t =
		let rec aux i = function
			| Leaf a -> Array.iteri (fun ii e -> f (ii+i) e) a
			| Cat (_, l, r) -> aux 0 l ; aux (length l) r in
		aux 0 t

	let rec iter f = function
		| Leaf a -> Array.iter f a
		| Cat (_, l, r) -> iter f l ; iter f r
	
	let mapi f t =
		let rec aux i = function
			| Leaf a -> Leaf (Array.mapi (fun ii e -> f (ii+i) e) a)
			| Cat (n, l, r) -> Cat (n, aux 0 l, aux (length l) r) in
		aux 0 t
	
	let rec map f = function
		| Leaf a -> Leaf (Array.map f a)
		| Cat (n, l, r) -> Cat (n, map f l, map f r)
	
	let rec fold_left f s = function
		| Leaf a -> Array.fold_left f s a
		| Cat (_, l, r) ->
			let s' = fold_left f s l in
			fold_left f s' r
	
	let rec fold_right f t s = match t with
		| Leaf a -> Array.fold_right f a s
		| Cat (_, l, r) ->
			let s' = fold_right f r s in
			fold_right f l s'

	(* ROPE_GEN *)

	let cat l r = (* TODO: if l or r is small, append to left/rightmost array *)
		Cat (length l + length r, l, r)

	let rec sub t start stop = match t with
		| Leaf a -> Leaf (Array.sub a start (stop-start))
		| Cat (_, l, r) ->
			let nl = length l in
			if stop <= nl then sub l start stop
			else if start >= nl then sub r (start-nl) (stop-nl)
			else Cat (stop-start, sub l start nl, sub r 0 (stop-nl))

	let rec nth t i = match t with
		| Leaf a -> a.(i)
		| Cat (_, l, r) ->
			let nl = length l in
			if i < nl then nth l i
			else nth r (i-nl)

	let rec cut t start stop =
		let n' = length t - (stop-start) in
		match t with
		| Leaf a -> Leaf (Array.init n' (fun i -> if i < start then a.(i) else a.(i-start+stop)))
		| Cat (_, l, r) ->
			let nl = length l in
			if stop <= nl then Cat (n', cut l start stop, r)
			else if start >= nl then Cat (n', l, cut r (start-nl) (stop-nl))
			else Cat (n', cut l start nl, cut r 0 (stop-nl))
	
	let insert t i t' =
		cat (cat (sub t 0 i) t') (sub t i (length t))

	let to_string t =
		let str = String.create (length t) in
		iteri (String.set str) t ;
		str
	
	let of_string str =
		let l = String.length str in
		let array_of_string str = Array.init l (fun i -> str.[i]) in
		Leaf (array_of_string str)

	let to_array t =
		let l = length t in
		Array.init l (fun i -> nth t i)	(* FIXME: copy array by array *)
	
	let of_array arr = Leaf arr

	let to_list t =
		let lst = fold_left (fun lst e -> e :: lst) [] t in
		List.rev lst

	let of_list lst = of_array (Array.of_list lst)
end

module Make : ROPE_GEN = Make_raw
