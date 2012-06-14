open Pfds_intf

module Stack_base =
struct
    type 'a t = Nil | Cons of ('a * 'a t)

   	let rec iter f t = match t with
		| Nil -> ()
		| Cons (x, t') -> f x ; iter f t'

    include Iterable_impl.Of_gen (struct
        type 'a t' = 'a t
        let iter = iter
    end)

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


    let rec to_list = function
        | Nil -> []
        | Cons (x, t') -> x :: to_list t'

end

module Stack : STACK = Stack_ops_impl.Make (Stack_base)
