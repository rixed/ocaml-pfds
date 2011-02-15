open Pfds_intf

module Rope_test (Rope : ROPE_GEN) =
struct
	open Rope

	(* TODO: test the iterable part separately *)
	let () =
		assert (is_empty empty = true) ;
		assert (length empty = 0) ;
		(* convertions preserve emptiness *)
		assert (to_string empty = "") ;
		assert (to_array empty = [||]) ;
		assert (to_list empty = []) ;
		assert (is_empty (of_list [])) ;
		assert (is_empty (of_string "")) ;
		assert (is_empty (of_array [||])) ;

		(* convertions preserve length *)
		assert (length (of_string "bla") = 3) ;
		assert (length (of_list [ 1 ; 2 ; 3 ]) = 3) ;
		assert (length (of_array [| 1 ; 2 ; 3 |]) = 3) ;

		(* manipulations *)
		let (+^) a b = cat a (of_string b) in
		let complex = (of_string "the quick") +^ " brown fox" +^ " jumps over" +^ " the lazy dog" in
		assert (to_string complex = "the quick brown fox jumps over the lazy dog") ;
		assert (length complex = 43) ;
		assert (nth complex 2 = 'e') ;
		assert (nth complex 40 = 'd') ;
		assert (to_string (sub complex 7 21) = "ck brown fox j") ;

		(* map *)
		let complex_up = map Char.uppercase complex in
		assert (to_string complex_up = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG") ;
end

module Rope_test1 = Rope_test (Rope_impl.Make_raw)
