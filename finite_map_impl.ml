open Pfds_intf

module Finite_map_raw (Ord : ORDERED) =
struct
    type key = Ord.t
    type 'a t = E | T of ('a t * key * 'a * 'a t)

    let empty = E

    let bind map k x =
        let rec aux last = function
            | E ->
                if last = Some k then raise Exit else T (E, k, x, E)
            | T (l, k', y, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux None l, k', y, r)
                else T (l, k', y, aux last r) in
        try aux None map with Exit -> map

    let rec lookup map k = match map with
        | E -> raise Not_found
        | T (l, k', x, r) ->
            let cmp = Ord.compare k k' in
            if cmp < 0 then lookup l k
            else if cmp > 0 then lookup r k
            else x

    let rec iter map f = match map with
        | E -> ()
        | T (l, k, x, r) ->
            iter l f ;
            f k x ;
            iter r f
end

module Finite_map (Ord : ORDERED) :
    FINITE_MAP with type key = Ord.t = Finite_map_raw (Ord)
