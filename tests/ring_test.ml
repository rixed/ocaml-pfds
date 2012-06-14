open Pfds_intf

module Ring_test (Ring : RING_GEN) =
struct
    open Ring

    let () =
        assert (is_empty empty = true) ;
        assert (length empty = 0) ;
        assert (next empty = empty) ;
        assert (prev empty = empty) ;
        try (ignore (get empty) ; assert false) with Not_found -> () ;
        assert (get (insert_after empty 1) = 1) ;
        assert (get (insert_before empty 1) = 1) ;
        try (ignore (remove empty) ; assert false) with Not_found -> ()
end

module Ring_test1 = Ring_test (Ring_impl.Ring_raw)

