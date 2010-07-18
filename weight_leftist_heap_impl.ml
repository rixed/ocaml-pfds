open Pfds_intf

(* Same as Leftist_heap but with size instead of rank *)
module Weight_leftist_heap_raw (Ord : ORDERED) =
struct
	type e = Ord.t
	type t = E | T of int * e * t * t

	let empty = E
	let is_empty = function E -> true | _ -> false
	let singleton x = T (1, x, E, E)

	let size = function
		| E -> 0
		| T (s, _, _, _) -> s
	
	let rec merge a b = match a with
		| E -> b
		| T (_, x, a_l, a_r) -> (match b with
			| E -> a
			| T (_, y, b_l, b_r) ->
				let size_tot = size a + size b in
				(* Contrary to Leftist_heap, we know before merging the resulting size,
				 * so we know before calling merge if we must merge at left or at right.
				 * I don't think it makes such a big difference, though. *)
				if Ord.compare x y <= 0 then (
					let size_merge = size a_r + size b in
					if size a_l >= size_merge then
						T (size_tot, x, a_l, merge a_r b)
					else
						T (size_tot, x, merge a_r b, a_l)
				) else (
					let size_merge = size a + size b_r in
					if size b_l >= size_merge then
						T (size_tot, y, b_l, merge a b_r)
					else
						T (size_tot, y, merge a b_r, b_l)))
	
	let insert a x = merge a (singleton x)
	let min = function E -> raise Empty | T (_, x, _, _) -> x
	let delete_min = function E -> raise Empty | T (_, _, l, r) -> merge l r
end

module Weight_leftist_heap (Ord : ORDERED) :
	HEAP with type e = Ord.t = Weight_leftist_heap_raw (Ord)

