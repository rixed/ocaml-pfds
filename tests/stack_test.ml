open Pfds_intf

module Stack_test (Stack : STACK) =
struct
	open Stack

	let () =
		assert (is_empty empty) ;
		let singleton = cons empty empty in
		assert (not (is_empty singleton)) ;
		assert (head singleton = empty) ;
		assert (is_empty (tail singleton)) ;
		try
			ignore (head empty) ; assert false
		with Empty -> () ;
		try
			ignore (tail empty) ; assert false
		with Empty -> () ;
		let s = cons 1 (cons 2 (cons 3 empty)) in
		let suff_s =
			cons s
				(cons (cons 2 (cons 3 empty))
					(cons (cons 3 empty)
						(cons empty empty))) in
		assert (suffixes s = suff_s)
end

module Stack_test1 = Stack_test (Stack_impl.Stack)

