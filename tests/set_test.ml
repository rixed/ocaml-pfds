open Pfds_intf

module Set_test (Set : SET with type e = String.t) =
struct
    open Set

    let s = singleton "one"
    let s' = insert s "two"

    let test_emptiness () =
        assert (is_empty empty) ;
        assert (not (is_empty s))

    let test_insert () =
        let rec add_one s n =
            if n <= 0 then s
            else let str = string_of_int n in
                add_one (insert s str) (n-1) in
        let nb = 10 in
        let set = add_one empty nb in
        assert (length set = nb) ;
        let set = insert s "one" in
        assert (length set = 1)

    let test_member () =
        assert (member s "one") ;
        let ss' = insert s "two" in
        assert (member ss' "one") ;
        assert (member ss' "two")

    let test_delete () =
        assert (is_empty (delete s "one")) ;
        try ignore (delete s "pas glop") ; assert false
        with Not_found -> () ;
        let set = List.fold_left insert empty
            [ "capricorn" ; "aquarius" ; "pisces" ;
              "aries" ; "taurus" ; "gemini" ; "cancer" ;
              "leo" ; "virgo" ; "libra" ; "scorpio" ] in
        assert (length set = 11) ;
        let t str =
            let set' = delete set str in
            assert (length set' = 10) ;
            assert (not (member set' str)) in
        t "aquarius" ;
        t "cancer" ;
        t "capricorn"

    let () =
        test_emptiness () ;
        test_insert () ;
        test_member () ;
        test_delete ()
end

module Set_test1 = Set_test (Btree_impl.Unbalanced_set (String))
module Set_test2 = Set_test (Red_black_tree_impl.Red_black_tree (String))
