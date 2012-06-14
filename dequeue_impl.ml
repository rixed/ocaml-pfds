open Pfds_intf

module Dequeue_raw =
struct
    type 'a t =  'a list * 'a list

    let empty = [], []
    let is_empty t = t = empty
    let singleton x = [x], []

    (* Return the first half of l and the last half reverted *)
    let rev lst =
        let rec rev_count p s = function
            | [] -> p, s
            | x::l' -> rev_count (x::p) (s+1) l' in
        let rec split h t c =
            if c = 0 then h, t
            else split ((List.hd t)::h) (List.tl t) (c-1) in
        let rev_lst, len = rev_count [] 0 lst in
        split rev_lst [] (len/2)

    let check = function
        | [], r -> let new_r, new_f = rev r in new_f, new_r
        | f, [] -> let new_f, new_r = rev f in new_f, new_r
        | t -> t

    let cons (f, r) x = check (x::f, r)
    let head = function
        | x::_, _ -> x
        | [], x::r -> assert (r = []) ; x
        | [], [] -> raise Empty
    let tail = function
        | _::f, r -> check (f, r)
        | [], _::r -> assert (r = []) ; empty
        | [], [] -> raise Empty

    let snoc (f, r) x = check (f, x::r)
    let last = function
        | _, x::_ -> x
        | x::f, [] -> assert (f = []) ; x
        | [], [] -> raise Empty
    let init = function
        | f, _::r -> check (f, r)
        | _::f, [] -> assert (f = []) ; empty
        | [], [] -> raise Empty
end

module Dequeue : DEQUEUE = Dequeue_raw
