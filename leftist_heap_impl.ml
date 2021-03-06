open Pfds_intf

module Leftist_heap_raw (Ord : ORDERED) =
struct
    type e = Ord.t
    type t = E | T of int * e * t * t

    let empty = E
    let is_empty = function E -> true | _ -> false
    let singleton x = T (1, x, E, E)

    let rank = function
        | E -> 0
        | T (r, _, _, _) -> r

    let rec merge a b = match a with
        | E -> b
        | T (_, x, a_l, a_r) -> (match b with
            | E -> a
            | T (_, y, b_l, b_r) ->
                let makeT v l r =
                    let rank_l, rank_r = rank l, rank r in
                    if rank_l >= rank_r then T (rank_r + 1, v, l, r)
                    else T (rank_l + 1, v, r, l) in
                if Ord.compare x y <= 0 then makeT x a_l (merge a_r b)
                else makeT y b_l (merge a b_r))

    let insert a x = merge a (singleton x)
    let min = function E -> raise Empty | T (_, x, _, _) -> x
    let delete_min = function E -> raise Empty | T (_, _, l, r) -> merge l r

    let rec member t x = match t with
        | E -> false
        | T (_, y, l, r) ->
            let cmp = Ord.compare x y in
            if cmp = 0 then true else
            if cmp < 0 then member l x else
            member r x
end

module Leftist_heap (Ord : ORDERED) :
    HEAP with type e = Ord.t = Leftist_heap_raw(Ord)

