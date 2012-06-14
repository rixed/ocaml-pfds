open Pfds_intf

module Sortlist_test (SortList : SORTLIST_GEN) =
struct
    open SortList

    let () =
        let t = List.fold_left (fun t i ->
            insert (<) t i) empty [ 5;1;2;4;3;0;3 ] in
        assert (length t = 7) ;
        assert (to_list t = [ 0;1;2;3;3;4;5 ]) ;
        let t = remove ((=) 3) t in (* remove only one of the 3 *)
        assert (to_list t = [ 0;1;2;3;4;5 ]) ;
        (* remove does not change anything if the item is not found *)
        assert (is_empty (remove (fun _ -> true) empty)) ;
        assert (to_list (remove (fun _ -> false) t) = [ 0;1;2;3;4;5 ])

end

module Iterable_test1 = Iterable_test.Of_gen (Sortlist_impl.Gen)
module Sortlist_test1 = Sortlist_test (Sortlist_impl.Gen)
