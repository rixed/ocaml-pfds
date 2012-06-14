open Pfds_intf

module Make (Set : SET_BASE) =
struct
    include Set
    let merge a b = fold_left insert a b
    let filter s f = fold_left (fun s x -> if f x then s else delete s x) s s
end
