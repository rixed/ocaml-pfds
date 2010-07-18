open Pfds_intf

module Batched_queue_raw =
struct
	type 'a t = 'a list * 'a list	(* front list, rear list *)

	let empty = [], []
	let is_empty (f, _) = f = []
	let singleton x = [x], []

	let checkf = function
		| [], r -> List.rev r, []
		| t -> t
	
	let snoc (f, r) x = checkf (f, x::r)
	
	let head = function
		| [], _ -> raise Empty
		| x::_, _ -> x
	
	let tail = function
		| [], _ -> raise Empty
		| _::f, r -> checkf (f, r)
end

module Batched_queue : QUEUE = Batched_queue_raw

