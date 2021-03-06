open Pfds_intf

module Make (Set : SET_BASE) :
    SET with type t = Set.t and type e = Set.e =
struct
    include Set
    let merge a b = fold_left insert a b
    let filter f s = fold_left (fun s x -> if f x then s else delete s x) s s
end
