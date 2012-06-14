open Pfds_intf

module DQ_test (Dequeue : DEQUEUE) =
struct
    open Dequeue

    let () =
        assert (is_empty empty) ;
        let s = singleton 0 in
        assert (not (is_empty s)) ;
        assert (head s = 0) ;
        assert (last s = 0) ;
        assert (is_empty (tail s)) ;
        assert (is_empty (init s)) ;

        try ignore (head empty) ; assert false
        with Empty -> () ;
        try ignore (tail empty) ; assert false
        with Empty -> () ;
        try ignore (last empty) ; assert false
        with Empty -> () ;
        try ignore (init empty) ; assert false
        with Empty -> () ;

        assert (head (snoc s 1) = 0) ;
        assert (last (snoc s 1) = 1) ;
        assert (head (snoc empty 1) = 1) ;
        assert (last (snoc empty 1) = 1)
end

module Dequeue_test = DQ_test (Dequeue_impl.Dequeue_raw)

