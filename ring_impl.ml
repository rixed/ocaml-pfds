open Pfds_intf

let (%) f g x = f (g x)

module Ring_raw =
struct
    type 'a t = 'a list * 'a list * int

    let empty = [], [], 0
    let is_empty (_, _, s) = s = 0
    let singleton a = [a], [], 1
    let length (_, _, s) = s

    let rewind (prevs, nexts, size) =
        match List.rev_append prevs nexts with
            | [] -> empty
            | fst :: rest -> [fst], rest, size

    let get = function
        | curr :: _, _, _ -> curr
        | _ -> raise Not_found

    let next = function
        | prevs, curr :: nexts, size -> curr :: prevs, nexts, size
        | lst -> rewind lst
    let prev = function
        | [curr], nexts, size -> List.rev (curr :: nexts), [], size
        | curr :: prevs, nexts, size -> prevs, curr :: nexts, size
        | [], [], size -> assert(size = 0) ; empty
        | _ -> assert false

    let insert_before lst e = match lst with
        | curr :: prevs, nexts, size -> e :: prevs, curr :: nexts, size+1
        | _ -> [e], [], 1
    let insert_after lst e = match lst with
        | curr :: prevs, nexts, size -> e :: curr :: prevs, nexts, size+1
        | _ -> [e], [], 1

    let remove lst = match lst with
        | _ :: prevs, n :: nexts, size -> n :: prevs, nexts, size-1
        | _ :: prevs, [], size -> rewind (prevs, [], size-1)
        | _ -> raise Not_found

    let iterr f t =
        let rec aux l s =
            if s > 0 then (f l ; aux (next l) (s-1)) in
        aux t (length t)
    let iter f t = iterr (f % get) t
    let iterir f t =
        let rec aux l s i =
            if s > 0 then (f i l ; aux (next l) (s-1) (i+1)) in
        aux t (length t) 0

    include Iterable_impl.Of_gen (struct
        type 'a t' = 'a t
        let iter = iter
    end)

    let iteri f t = iterir (fun i l -> f i (get l)) t (* better iteri than the above *)

    let map f (l1, l2, s) =
        let l2' = List.map f l2 in  (* we want to map l2 first *)
        let l1' = List.map f l1 in
        l1',l2', s
    let mapi f (l1, l2, s) =
        let idx = ref 0 in
        let map2mapi x = let res = f !idx x in incr idx ; res in
        let l2' = List.map map2mapi l2 in   (* we want to map l2 first *)
        let l1' = List.map map2mapi l1 in
        l1', l2', s

    let fold_leftr f b t =
        let rec aux b' t' s =
            if s > 0 then aux (f b' t') (next t') (s-1) else b' in
        aux b t (length t)
    let fold_left f b t = fold_leftr (fun b t -> f b (get t)) b t
    let fold_rightr f t b =
        let rec aux t' b' s =
            if s > 0 then aux (next t') (f t' b') (s-1) else b' in
        aux t b (length t)
    let fold_right f t b = fold_rightr (fun t b -> f (get t) b) t b

end

module Ring : RING_GEN = Ring_raw
