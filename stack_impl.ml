open Pfds_intf

module Stack_raw =
struct
	type 'a t = Nil | Cons of ('a * 'a t)

	let empty = Nil
	
	let is_empty = function
		| Nil -> true
		| _ -> false
	
	let singleton x = Cons (x, Nil)

	let length t =
		let rec aux n = function
			| Nil -> n
			| Cons (_, t') -> aux (n+1) t' in
		aux 0 t
	
	let rec iter f t = match t with
		| Nil -> ()
		| Cons (x, t') -> f x ; iter f t'
	
	let iteri f t =
		let c = ref 0 in
		iter (fun x -> f !c x ; incr c) t
	
	let rev t =
		let rec aux prevs = function
			| Nil -> prevs
			| Cons (x, t') -> aux (Cons (x, prevs)) t' in
		aux Nil t

	let map f t =
		let rec aux prevs = function
			| Nil -> prevs
			| Cons (x, t') -> aux (Cons (f x, prevs)) t' in
		rev (aux Nil t)
	
	let mapi f t =
		let c = ref (-1) in
		map (fun x -> incr c ; f !c x) t
	
	let rec fold_left f b t = match t with
		| Nil -> b
		| Cons (x, t') -> fold_left f (f b x) t'
	
	let rec fold_right f t b = fold_left (fun x b -> f b x) b t

	let cons x t = Cons (x, t)

	let head = function
		| Nil -> raise Empty
		| Cons (x, _) -> x
	
	let tail = function
		| Nil -> raise Empty
		| Cons (_, t') -> t'
end

module Stack : STACK = Stack_raw
