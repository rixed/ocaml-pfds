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

    (* Deletion is not trivial. Here are two helpers: *)

    (* returns the min key, it's value and the tree without the min *)
    let rec del_min = function
        | E -> invalid_arg "map"
        | T (E, k, x, r) -> k, x, r
        | T (l, k, x, r) ->
            let km, xm, l' = del_min l in
            km, xm, T (l', k, x, r)

    (* deletes the root of the given tree *)
    let del_root = function
        | E -> invalid_arg "map"
        | T (l, _, _, E) -> l
        | T (E, _, _, r) -> r
        | T (l, _, _, r) ->
        (* TODO: alternatively, instead of replacing the root with its successor
           replace it by its predecessor (mirroring del_min).
           See Knuth TAOCPv3, p435, to know why *)
            let km, xm, r' = del_min r in
            T (l, km, xm, r')

    (* Now we are ready to delete any element: *)

    let unbind_exn m k =
        let rec aux = function
            | E -> raise Not_found
            | T (l, k', x, r) as n ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* found you! *) del_root n in
        aux m

    let unbind m k =
        try unbind_exn m k with Not_found -> m

    (* Note: we suppose we won't suppress many elements. *)
    let rec filter f = function
        | E -> E
        | T (l, k, x, r) as n ->
            if f k x then T (filter f l, k, x, filter f r)
            else filter f (del_root n)

    let rec filter_map f = function
        | E -> E
        | T (l, k, x, r) as n ->
            match f k x with
                | Some y -> T (filter_map f l, k, y, filter_map f r)
                | None   -> filter_map f (del_root n)

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

    let update_exn m k f =
        let rec aux = function
            | E -> raise Not_found
            | T (l, k', x, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* cmp = 0 *) T (l, k', f x, r) in
        aux m

    let update m k f =
        try update_exn m k f with Not_found -> m

    let update_with_default d m k f =
        let rec aux = function
            | E -> T (E, k, d, E)
            | T (l, k', x, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* cmp = 0 *) T (l, k', f x, r) in
        aux m

    let update_with_default_delayed d m k f =
        let rec aux = function
            | E -> T (E, k, d (), E)
            | T (l, k', x, r) ->
                let cmp = Ord.compare k k' in
                if cmp < 0 then T (aux l, k', x, r)
                else if cmp > 0 then T (l, k', x, aux r)
                else (* cmp = 0 *) T (l, k', f x, r) in
        aux m

    let rec map f = function
        | E -> E
        | T (l, k, x, r) -> T (map f l, k, f k x, map f r)

end

module Finite_map (Ord : ORDERED) :
    FINITE_MAP with type key = Ord.t = Finite_map_raw (Ord)
