open Pfds_intf

module Queue_test (Queue : QUEUE) =
struct
	open Queue

	let () =
		assert (is_empty empty) ;
		let s = singleton 0 in
		assert (not (is_empty s)) ;
		assert (head s = 0) ;
		assert (is_empty (tail s)) ;
		try ignore (head empty) ; assert false
		with Empty -> () ;
		try ignore (tail empty) ; assert false
		with Empty -> () ;
		assert (head (snoc s 1) = 0) ;
		assert (head (snoc empty 1) = 1)
end

module Batched_queue_test = Queue_test (Batched_queue_impl.Batched_queue_raw)

