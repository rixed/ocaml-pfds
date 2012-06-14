open Pfds_intf

module Of (Iter : ITERABLE) =
struct
    open Iter

    (* Not much for now since we can't create content (until from_list and friends join us... *)
    let () =
        assert (is_empty empty = true) ;
        assert (length empty = 0) ;
end

module Of_gen (Iter : ITERABLE_GEN) =
struct
    open Iter

    (* Not much for now since we can't create content (until from_list and friends join us... *)
    let () =
        assert (is_empty empty = true) ;
        assert (length empty = 0) ;
end
