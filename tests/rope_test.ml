open Pfds_intf

module Rope_test (Rope : ROPE_GEN) =
struct
    open Rope

    let () =
        assert (is_empty (cat empty empty)) ;
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

        (* iterators *)
        let complex_up = map Char.uppercase complex in
        assert (to_string complex_up = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG") ;
        assert (fold_left  (fun n _ -> n+1) 0 complex_up = length complex_up) ;
        assert (fold_right (fun _ n -> n+1) complex_up 0 = length complex_up) ;

        (* count, index, rindex *)
        assert (count complex 't' = 2) ;
        assert (count complex 'h' = 2) ;
        assert (count complex 'e' = 3) ;
        assert (count (singleton 'a') 'a' = 1) ;
        assert (count (singleton 'a') 'b' = 0) ;
        assert (index complex 'e' = 2) ;
        assert (rindex complex 'e' = 33) ;
        assert (index (singleton 'a') 'a' = 0) ;
        assert (rindex (singleton 'a') 'a' = 0) ;

        (* cut *)
        let r = Rope.of_string "foobar" in
        let r = Rope.cut r 2 3 in
        let r = Rope.cut r 2 3 in
        assert (Rope.to_string r = "foar")
end

module Iterable_test1 = Iterable_test.Of_gen (Rope_impl.Make_raw)
module Rope_test1 = Rope_test (Rope_impl.Make_raw)
