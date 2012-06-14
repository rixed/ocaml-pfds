open Pfds_intf
open Bricabrac

module Raw =
struct
    (** the list of items before the cursor, and those after, then length *)
    type 'a t = 'a list * 'a list * int

    let rec iter f = function
        | [], [], _n     -> ()
        | x::bef, aft, n -> iter f (bef, x::aft, n)
        | [], x::aft, n  -> f x ; iter f ([], aft, n) (* notice we do not take care of n *)

    include Iterable_impl.Of_gen (struct
        type 'a t' = 'a t
        let iter = iter
    end)

   	let empty = [], [], 0

    let is_empty = function
        | [], [], 0 -> true
        | _, _, n -> assert(n > 0) ; false

	let singleton x = [], [x], 1

	let length (_,_,n) = n

	let rec fold_left f c = function
        | [], [], _      -> c
        | x::bef, aft, n -> fold_left f c (bef, x::aft, n)
        | [], x::aft, n  -> fold_left f (f c x) ([], aft, n) (* again we do not update n *)

	let rec fold_right f t c = match t with
        | [], [], _      -> c
        | bef, x::aft, n -> fold_right f (x::bef, aft, n) c
        | x::bef, [], n  -> fold_right f (bef, [], n) (f x c) (* idem *)

    let iteri f t =
        fold_left (fun c x -> f c x ; c+1) 0 t |>
        ignore

    (* notice that f is not called in list order, and that the resulting list may not be sorted! *)
    let map f (bef, aft, n) =
        List.map f bef, List.map f aft, n
	
    (* f is not called in increasing index order! *)
    let mapi f (bef, aft, n) =
        let list_mapi f l =
            List.fold_left (fun (c,p) x -> c+1, f c x :: p) (0, []) l |>
            snd in
        let bef_len = List.length bef in
        list_mapi (fun i x -> f (bef_len - 1 - i) x) bef,
        list_mapi (fun i x -> f (bef_len + i) x) aft,
        n

    (* terminate even when the list is not already ordered according to [lt] *)
    let insert lt t x =
        (* function to rewind as much as necessary *)
        let rec rwd t = match t with
            | [], _, _       -> t
            | b::bef, aft, n -> if lt b x then t else rwd (bef, b::aft, n) in
        (* function to advance as much as necessary *)
        let rec fwd t = match t with
            | _, [], _       -> t
            | bef, a::aft, n -> if lt x a then t else fwd (a::bef, aft, n) in
        fwd (rwd t)

	let remove f ((bef, aft, n) as t) =
        let rec aux p = function
            | []    -> false, [], p
            | x::x' -> if f x then true, x', p
                              else aux (x::p) x' in
        let found, bef', aft' = aux aft bef in
        if found then bef', aft', n-1 else
        let found, aft', bef' = aux bef aft in
        if found then bef', aft', n-1 else
        t

end

module Gen : SORTLIST_GEN = Raw
