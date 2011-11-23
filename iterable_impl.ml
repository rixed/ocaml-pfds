module type ITER =
sig
	type t'
	type e'
	val iter : (e' -> unit) -> t' -> unit
end

module Iterable_from_iter (I:ITER) =
struct
	include I

	let iteri f t =
		let c = ref (-1) in
		iter (fun x -> incr c ; f !c x) t
	
	let fold_left f b t =
		let b' = ref b in
		let f' x = b' := f !b' x in
		iter f' t ;
		!b'
	
	let fold_right f t b = fold_left (fun x b -> f b x) b t

	let length t = fold_left (fun c _ -> c+1) 0 t

	exception Found of e'
	let find_first f t =
		try iter (fun x -> if f x then raise (Found x)) t ; raise Not_found
		with Found x -> x

	let exists f t =
		try ignore (find_first f t) ; true
		with Not_found -> false
end

