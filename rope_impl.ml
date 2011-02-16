open Pfds_intf
open Bricabrac

module Make_raw =
struct
	(* ITERABLE_GEN *)

	type 'a t =
		| Leaf of (int (* length *) * int (* offset *) * (int -> 'a) (* getter *))
		| Cat of (int (* total length *) * 'a t (* left part *) * 'a t (* right part *))

	let empty = Leaf (0, 0, fun _ -> assert false)

	let is_empty = function
		| Leaf (0, _, _) -> true
		| _ -> false
	
	let singleton e = Leaf (1, 0, fun x -> assert (x=0) ; e)

	let length = function
		| Leaf (n, _, _) -> n
		| Cat (n, _, _) -> n
	
	let iteri f t =
		let rec aux i = function
			| Leaf (n, o, get) -> for ii = 0 to n-1 do f (ii+i) (get (ii+o)) done
			| Cat (_, l, r) -> aux 0 l ; aux (length l) r in
		aux 0 t

	let rec iter f = function
		| Leaf (n, o, get) -> for i = 0 to n-1 do f (get (i+o)) done
		| Cat (_, l, r) -> iter f l ; iter f r
	
	let mapi f t =
		let rec aux i = function
			| Leaf (n, o, get) -> Leaf (n, 0, fun ii -> f (i+ii) (get (o+ii)))
			| Cat (n, l, r) -> Cat (n, aux 0 l, aux (length l) r) in
		aux 0 t
	
	let rec map f = function
		| Leaf (n, o, get) -> Leaf (n, 0, fun ii -> f (get (o+ii)))
		| Cat (n, l, r) -> Cat (n, map f l, map f r)
	
	let rec fold_left f s = function
		| Leaf (n, o, get) ->
			let rec aux i s =
				if i >= n then s
				else aux (i+1) (f s (get (o+i))) in
			aux 0 s
		| Cat (_, l, r) ->
			let s' = fold_left f s l in
			fold_left f s' r
	
	let rec fold_right f t s = match t with
		| Leaf (n, o, get) ->
			let rec aux i s =
				if i < 0 then s
				else aux (i-1) (f (get (o+i)) s) in
			aux (n-1) s
		| Cat (_, l, r) ->
			let s' = fold_right f r s in
			fold_right f l s'

	(* ROPE_GEN *)

	let cat l r = (* TODO: if l or r is small, append to left/rightmost array *)
		Cat (length l + length r, l, r)

	let rec sub t start stop = match t with
		| Leaf (n, o, get) -> assert (stop-start <= n) ; Leaf (stop-start, o+start, get)
		| Cat (_, l, r) ->
			let nl = length l in
			if stop <= nl then sub l start stop
			else if start >= nl then sub r (start-nl) (stop-nl)
			else Cat (stop-start, sub l start nl, sub r 0 (stop-nl))

	let rec nth t i = match t with
		| Leaf (n, o, get) -> assert (i+o < n) ; get (i+o)
		| Cat (_, l, r) ->
			let nl = length l in
			if i < nl then nth l i
			else nth r (i-nl)

	let rec cut t start stop =
		let n' = length t - (stop-start) in
		match t with
		| Leaf (n, o, get) -> assert (n' <= n) ; Leaf (n', o+start, get)
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
		let n = String.length str in
		Leaf (n, 0, String.get str)

	let to_array t =
		let l = length t in
		Array.init l (fun i -> nth t i)	(* FIXME: copy array by array *)
	
	let of_array arr =
		let n = Array.length arr in
		Leaf (n, 0, Array.get arr)

	let to_list t =
		let lst = fold_left (fun lst e -> e :: lst) [] t in
		List.rev lst

	let of_list lst = of_array (Array.of_list lst)

	let of_func n f = Leaf (n, 0, f)

	let to_file fname t =
		let ochn = open_out fname in
		try_finalize
			(iter (output_char ochn)) t
			close_out ochn

	let of_file fname =
		let ichn = open_in fname in
		let n = in_channel_length ichn in
		let read_byte i =
			seek_in ichn i ;
			input_char ichn in
		Gc.finalise close_in ichn ;
		of_func n read_byte
end

module Make : ROPE_GEN = Make_raw
