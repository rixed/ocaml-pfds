open Pfds_intf	(* for exceptions *)
open Stream_intf

module Stream_raw =
struct
	type 'a t = 'a cell Lazy.t
	and 'a cell = Nil | Cons of 'a * 'a t

	(* Variables named "x" are items of a lazy list,
	 * variables named "f" are functions,
	 * variables named "n" are integer counters while "l" are integer lengths,
	 * variables named "t" are lazy lists,
	 * variables named "tt" are lazy lists of lazy lists.
	 * There are no such monstruosities as lazy lists of lazy lists of lazy lists. *)
	
	let empty = lazy Nil
	let is_empty t = t = lazy Nil

	(* Convertion / Creation *)

	let singleton x = lazy (Cons (x, empty))

	let to_list_rev t =
		let rec aux prev = function
			| lazy Nil -> prev
			| lazy (Cons (x, s')) -> aux (x::prev) s' in
		aux [] t

	let to_list t = List.rev (to_list_rev t)

	let rec of_list = function
		| [] -> empty
		| h :: t -> lazy (Cons (h, of_list t))

	let rec of_succ succ first = lazy (Cons (first, of_succ succ (succ first)))

	(* List like interface *)
	
	let head = function
		| lazy Nil -> raise Empty
		| lazy (Cons (x, _)) -> x
	
	let tail = function
		| lazy Nil as nil -> nil
		| lazy (Cons (_, t)) -> t
	
	let rec nth t n =
		if n = 0 then head t
		else nth (tail t) (n-1)
	
	let length t =
		let rec aux n = function
			| lazy Nil -> n
			| lazy (Cons (_, t')) -> aux (n+1) t' in
		aux 0 t

	let rec cat t1 t2 = match t1 with
		| lazy Nil -> t2
		| lazy (Cons (x, t1')) -> lazy (Cons (x, cat t1' t2))
	
	let rec map t f = match t with
		| lazy Nil -> lazy Nil
		| lazy (Cons (x, t')) -> lazy (Cons (f x, map t' f))
	
	let mapi t f =
		let rec aux i t = match t with
			| lazy Nil -> lazy Nil
			| lazy (Cons (x, t')) -> lazy (Cons (f i x, aux (i+1) t')) in
		aux 0 t

	let rec fold t b f = match t with
		| lazy Nil -> b
		| lazy (Cons (x, t')) -> fold t' (f x b) f

	let rec filter t f = match t with
		| lazy Nil as nil -> nil
		| lazy (Cons (x, t')) -> if f x then lazy (Cons (x, filter t' f)) else filter t' f
	
	(* Generators *)
	
	let cycle t =
		if is_empty t then raise Empty ;
		let rec aux = function
			| lazy Nil -> aux t
			| lazy (Cons (x, t')) -> lazy (Cons (x, aux t')) in
		aux t
	
	let rec repeat ?count x = match count with
		| None -> lazy (Cons (x, repeat x))
		| Some n -> if n <= 0 then empty else lazy (Cons (x, repeat ~count:(n-1) x))
	
	let rec stammer n = function
		| lazy Nil as nil -> nil
		| lazy (Cons (x, t')) -> lazy (Cons (x, cat (repeat ~count:(n-1) x) (stammer n t')))

	let rec dropwhile f t = match t with
		| lazy Nil as nil -> nil
		| lazy (Cons (x, t')) -> if f x then dropwhile f t' else t
	
	let rec takewhile f t = match t with
		| lazy Nil as nil -> nil
		| lazy (Cons (x, t')) -> if f x then lazy (Cons (x, takewhile f t')) else empty

	let rec skip n t = if n = 0 then t else skip (n-1) (tail t)

	let one_every n t =
		let rec aux n' = function
			| lazy Nil as nil -> nil
			| lazy (Cons (x, t')) ->
				if n' = 1 then lazy (Cons (x, aux n t')) else aux (n'-1) t' in
		aux 1 t

	let slice ?(step=1) ?dropwhile:du ?takewhile:da t =
		let cut_head = match du with
			| None -> t
			| Some f -> dropwhile f t in
		let chopped = match da with
			| None -> cut_head
			| Some f -> takewhile f cut_head in
		one_every step chopped

	let rec zip2 t1 t2 = match t1 with
		| lazy Nil -> lazy Nil
		| lazy (Cons (h1, t1')) -> (match t2 with
			| lazy Nil -> lazy Nil
			| lazy (Cons (h2, t2')) -> lazy (Cons ((h1, h2), zip2 t1' t2')))

	let rec zip2_longest t1 t2 x1 x2 =
		if is_empty t1 && is_empty t2 then empty else
			let h1 = if is_empty t1 then x1 else head t1
			and h2 = if is_empty t2 then x2 else head t2 in
			lazy (Cons ((h1, h2), zip2_longest (tail t1) (tail t2) x1 x2))

	let rec firsts n = function
		| lazy Nil as nil -> nil
		| lazy (Cons (x, t')) -> if n <= 0 then empty else lazy (Cons (x, firsts (n-1) t'))
	
	let lasts n t =
		let rec aux h' = function
			| lazy Nil -> h'
			| lazy (Cons (_, t')) -> aux (tail h') t' in
		aux t (skip n t)

	let groupby t f_key =
		(* prep_key {1,2,3} = (f_key 1),{1,2,3} *)
		let prep_key t =
			assert (not (is_empty t)) ;
			(f_key (head t)), t in
		(* headtail k3,{3,2,1} k4,{4,5,6} = {(k4,{4,3,2,1}), (k5,{5,6})} *)
		let rec headtail p1 p2 =
			match p2 with
			| _, lazy Nil -> singleton p1
			| kb, lazy (Cons (b, t2')) ->
				let ka, t1 = p1 in
				if kb = ka then (
					let p1' = kb, cat t1 (singleton b) in
					if is_empty t2' then singleton p1'
					else headtail p1' (prep_key t2')
				) else lazy (Cons (p1, groupby' p2))
		and groupby' = function
			| _, lazy Nil -> empty
			| k, t -> headtail (k,empty) (prep_key t) in
		if is_empty t then empty else groupby' (prep_key t)

	let uniq t f_key =
		let rec aux prev_k = function
			| lazy Nil as nil -> nil
			| lazy (Cons (x, t')) -> let k_x = f_key x in
				if k_x = prev_k then aux prev_k t'
				else lazy (Cons (x, aux k_x t')) in
		if is_empty t then empty else
			let h = head t in lazy (Cons (h, aux (f_key h) t))
	
	(* Combinatoric generators *)

	(* ziphead {{1,2},{3,4,5},{6,7}} = {1,3,6} *)
	let rec ziphead = function
		| lazy Nil -> empty
		| lazy (Cons (t, tt')) -> lazy (Cons (head t, ziphead tt'))
	
	(* ziptail {{1,2},{3,4,5},{6,7}} = {{2},{4,5},{7}} *)
	let rec ziptail = function
		| lazy Nil -> empty
		| lazy (Cons (t, tt')) -> lazy (Cons (tail t, ziptail tt'))
	
	(* zip {{1,2},{3,4,5},{6,7}} = {{1,3,6},{2,4,7}} *)
	let zip tt =
		let rec aux tt' =
			let some_empty = fold tt' false (fun t e -> if e then true else is_empty t) in
			if some_empty then empty
			else lazy (Cons (ziphead tt', aux (ziptail tt'))) in
		if is_empty tt then empty else aux tt

	(* product {{1,2,3},{3,4}} = {{1,3},{1,4},{2,3},{2,4},{3,3},..}
	 * actually the other way around : {{1,2},{2,3},{3,3},{1,4},...} *)
	let product tt =
		let req_len, stt =
			fold tt (1, empty) (fun t (next_stam, new_tt) ->
				let next_t = stammer next_stam t in
				let l = length t in
				next_stam * l, cat new_tt (singleton (cycle next_t))) in
		firsts req_len (zip stt)
	
	let power t n =
		let rec rep n =
			if n = 0 then empty
			else lazy (Cons (t, rep (n-1))) in
		product (rep n)

	let rec permutations ?len t =
		if is_empty t then empty else
		let li = length t in
		let lo = match len with
			| None -> li
			| Some n -> assert (n<=li) ; n in
		let prepend_all x tt =
			if is_empty tt then singleton (singleton x) else
			map tt (fun t -> cat (singleton x) t) in
		let rec perms' n li lo t =
			if n = li then empty else
			cat (prepend_all (head t) (perms (li-1) (lo-1) (firsts (li-1) (tail t))))
			    (perms' (n+1) li lo (tail t))
		and perms li lo t =
			if lo = 0 then empty else
			let t' = cycle t in
			perms' 0 li lo t' in
		perms li lo t

end

module Stream : STREAM = Stream_raw

