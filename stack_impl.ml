open Pfds_intf

module Stack : STACK =
struct
	type 'a t = NIL | CONS of ('a * 'a t)
	exception Empty

	let empty = NIL
	
	let is_empty = function
		| NIL -> true
		| _ -> false
	
	let cons v stack = CONS (v, stack)

	let head = function
		| NIL -> raise Empty
		| CONS (head, _) -> head
	
	let tail = function
		| NIL -> raise Empty
		| CONS (_, tail) -> tail
end
