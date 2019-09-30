open Pfds_intf

module Heap_test (Heap : HEAP with type e = String.t) =
struct
    open Heap [@@ocaml.warning "-44"]

    let s = singleton "one"
    let s' = insert s "two"

    let test_emptiness () =
        assert (is_empty empty) ;
        assert (not (is_empty s))

    let test_min () =
        assert (min s = "one") ;
        assert (min s'= "one") ;
        assert (min (delete_min s') = "two")

    let test_singleton () =
        assert (is_empty (delete_min s))

    let test_insert () =
        (* Check we can remove as many elmt than distinct elmts we put in *)
        let rec add_one h n =
            if n <= 0 then h
            else add_one (insert h "foo") (n-1) in
        let rec del_one h n =
            if n <= 0 then h
            else del_one (delete_min h) (n-1) in
        let h = add_one empty 10 in
        let h'= del_one h 10 in
        assert (is_empty h')

    let test_exceptions () =
        try (
            ignore (min empty) ;
            assert (false)
        ) with Empty -> () ;
        try (
            ignore (delete_min empty) ;
            assert (false)
        ) with Empty -> ()

    let test_ops () =
        let module Op = Heap_ops_impl.Heap_ops_raw (Heap) in
        assert (is_empty (Op.of_list [])) ;
        assert (is_empty (delete_min (Op.of_list ["toto"]))) ;
        assert (singleton "A" = Op.of_list ["A"]) ;
        assert (Op.length empty = 0) ;
        assert (Op.length s = 1) ;
        let l = [ "A" ; "B" ; "C" ; "D" ] in
        assert (List.length l = Op.length (Op.of_list l))

    let () =
        test_emptiness () ;
        test_min () ;
        test_singleton () ;
        test_insert () ;
        test_exceptions () ;
        test_ops ()
end

module Heap_test1 = Heap_test (Leftist_heap_impl.Leftist_heap_raw (String))
module Heap_test2 = Heap_test (Weight_leftist_heap_impl.Weight_leftist_heap_raw (String))
module Heap_test3 = Heap_test (Binomial_heap_impl.Binomial_heap_raw (String))

