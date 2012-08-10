open Pfds_intf

module Finite_map_raw (Ord : ORDERED) =
struct
    type key = Ord.t
    type 'a t = E | T of ('a t * key * 'a * 'a t)

    let empty = E

    let is_empty = function E -> true | _ -> false

    let rec length = function
        | E -> 0
        | T (l, _, _, r) -> length l + length r + 1

    let bind m k x =
        let rec aux = function
            | E -> T (E, k, x, E)
            | T (l, k', y, r) ->
                let cmp = Ord.compare k k' in
                if cmp = 0 then (
                    if x == y then raise Exit
                    else T (l, k', x, r)
                ) else if cmp < 0 then T (aux l, k', y, r)
                else (* cmp > 0 *) T (l, k', y, aux r) in
        try aux m with Exit -> m

    let rec lookup m k = match m with
        | E -> raise Not_found
        | T (l, k', x, r) ->
            let cmp = Ord.compare k k' in
            if cmp < 0 then lookup l k
            else if cmp > 0 then lookup r k
            else x

    let rec iter m f = match m with
        | E -> ()
        | T (l, k, x, r) ->
            iter l f ;
            f k x ;
            iter r f

    let rec fold_left f i = function
        | E -> i
        | T (l, k, x, r) ->
            let i = fold_left f i l in
            let i = f i k x in
            fold_left f i r

    let rec fold_right f m i = match m with
        | E -> i
        | T (l, k, x, r) ->
            let i = fold_right f r i in
            let i = f k x i in
            fold_right f l i

    let update m k f =
        let rec aux = function
            | E -> raise Exit
            | T (l, k', x, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* cmp = 0 *) T (l, k', f x, r) in
        try aux m with Exit -> m

    let update_with_default d m k f =
        let rec aux = function
            | E -> T (E, k, d, E)
            | T (l, k', x, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* cmp = 0 *) T (l, k', f x, r) in
        aux m

end

module Finite_map (Ord : ORDERED) :
    FINITE_MAP with type key = Ord.t = Finite_map_raw (Ord)
