open Pfds_intf

module Map_test (Map : FINITE_MAP with type key = string) =
struct
    open Map

    let s = bind empty "single" 1

    let test_emptiness () =
        assert (is_empty empty) ;
        assert (not (is_empty s))

    let rec add_one s n =
        if n <= 0 then s
        else let str = string_of_int n in
            add_one (bind s str n) (n-1)

    let test_insert () =
        let nb = 10 in
        let m = add_one empty nb in
        assert (length m = nb) ;
        let m = add_one m (1 + nb) in (* should add only one entry *)
        assert (length m = 1 + nb) ;
        assert (length s = 1)

    let test_lookup () =
        assert (lookup s "single" = 1) ;
        assert (
            try ignore (lookup s "not there") ;
                false
            with Not_found -> true) ;
        let ss = bind s "second" 2 in
        assert (lookup ss "single" = 1) ;
        assert (lookup ss "second" = 2)

    let test_update () =
        let m = add_one empty 4 in
        let keys = fold_left (fun p k _x -> p ^ k) "" in
        let values = fold_left (fun p _k x -> p ^ string_of_int x) "" in
        assert (keys m = "1234") ;
        assert (values m = "1234") ;
        let m = update m "2" ((+) 1) in
        let m = update m "x" ((+) 1) in
        assert (keys m = "1234") ;
        assert (values m = "1334") ;
        let m = update_with_default 0 m "2" ((+) 1) in
        let m = update_with_default 0 m "5" ((+) 1) in
        assert (keys m = "12345") ;
        assert (values m = "14340")

    let () =
        test_emptiness () ;
        test_insert () ;
        test_lookup () ;
        test_update ()
end

module Met_test1 = Map_test (Finite_map_impl.Finite_map (String))
