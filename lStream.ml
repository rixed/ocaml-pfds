open Pfds_intf  (* for exceptions *)
open Batteries

type 'a t = 'a cell Lazy.t
and 'a cell = Nil | Cons of 'a * 'a t

(* Variables named "x" are items of a lazy list,
 * variables named "f" are functions,
 * variables named "n" are integer counters while "l" are integer lengths,
 * variables named "t" are lazy lists,
 * variables named "tt" are lazy lists of lazy lists.
 * There are no such monstruosities (so far) as lazy lists of lazy lists of lazy lists. *)

let empty = lazy Nil
let is_empty = function
    | lazy Nil -> true
    | _ -> false

let is_singleton = function
    | lazy (Cons (_, lazy Nil)) -> true
    | _ -> false

(* Convertion / Creation *)

let singleton x = lazy (Cons (x, empty))

let to_list_rev t =
    let rec aux prev = function
        | lazy Nil -> prev
        | lazy (Cons (x, s')) -> aux (x::prev) s' in
    aux [] t

let to_list t = List.rev (to_list_rev t)

let rec unfold f c =
    lazy (
        try let x, c' = f c in Cons (x, unfold f c')
        with Exit -> Nil
    )

let rec of_list = function
    | [] -> empty
    | h :: t -> lazy (Cons (h, of_list t))

let rec of_succ succ first = lazy (Cons (first, of_succ succ (succ first)))

let rec of_nth ?limit f = match limit with
    | Some 0 -> empty
    | Some l -> lazy (Cons (f 0, of_nth ~limit:(l-1) (fun i -> f (i+1))))
    | None   -> lazy (Cons (f 0, of_nth (fun i -> f (i+1))))

let of_string str =
    let l = String.length str in
    of_nth ~limit:l (String.get str)

let of_array arr =
    let l = Array.length arr in
    of_nth ~limit:l (Array.get arr)

(* warning: this won't work most of the time since the entries are consumed *)
let rec of_file ic =
    lazy (try Cons (Legacy.input_line ic, of_file ic) with End_of_file -> Nil)

let of_directory d =
    of_array (Sys.readdir d)

(* again, beware that the entries are consumed! *)
let rec of_fun f =
    lazy (
        let x = try Some (f ()) with Exit -> None in
        match x with None -> Nil | Some x -> Cons (x, of_fun f)
    )

let nat = of_succ succ 0

(* List like interface *)

let head = function
    | lazy Nil -> raise Empty
    | lazy (Cons (x, _)) -> x

let tail = function
    | lazy Nil as nil -> nil
    | lazy (Cons (_, t)) -> t

let cons x t = lazy (Cons (x, t))

let rec nth t n =
    if n = 0 then head t
    else nth (tail t) (n-1)

let length t =
    let rec aux n = function
        | lazy Nil -> n
        | lazy (Cons (_, t')) -> aux (n+1) t' in
    aux 0 t

let rec cat t1 t2 = lazy (match t1 with
    | lazy Nil -> Lazy.force t2
    | lazy (Cons (x, t1')) -> Cons (x, cat t1' t2)
)

let (++) = cat

let append t x = t ++ singleton x

let prepend x t = lazy (Cons (x, t))

let rec map t f = lazy (match t with
    | lazy Nil -> Nil
    | lazy (Cons (x, t')) -> Cons (f x, map t' f)
)

let (/@) = map

let rec map2 t1 t2 f = lazy (match t1, t2 with
    | lazy Nil, _ -> Nil
    | _, lazy Nil -> Nil
    | lazy (Cons (x1, t1')), lazy (Cons (x2, t2')) ->
        Cons (f x1 x2, map2 t1' t2' f)
)

let mapi t f =
    let rec aux i t = lazy (match t with
            | lazy Nil -> Nil
            | lazy (Cons (x, t')) -> (Cons (f i x, aux (i+1) t'))
        ) in
    aux 0 t

let rec iter t f = match t with
    | lazy Nil -> ()
    | lazy (Cons (x, t')) -> f x ; iter t' f

let iteri t f =
    let i = ref 0 in
    iter t (fun x -> f !i x ; incr i)

let rec fold t b f = match t with
    | lazy Nil -> b
    | lazy (Cons (x, t')) -> fold t' (f x b) f

let reduce t f =
    if is_empty t then raise Empty else
    fold (tail t) (head t) f

let rec filter t f = match t with
    | lazy Nil -> empty
    | lazy (Cons (x, t')) -> if f x then lazy (Cons (x, filter t' f)) else filter t' f

let (//) = filter

let map_opt t f =
    let t = map t f in
    map (t // ((<>) None)) Option.get

let (//@) = map_opt

let rec mask t f d = match t with
    | lazy Nil -> empty
    | lazy (Cons (x, t')) -> lazy (Cons ((if f x then x else d), mask t' f d))

let rec exists t f = match t with
    | lazy Nil -> false
    | lazy (Cons (x, t')) -> f x || exists t' f

let mem t x = exists t ((=) x)

let to_string t =
    let str = String.create (length t) in
    iteri t (String.set str) ;
    str

let to_array t =
    let t' = ref t in
    let init i =
        (* This assume that Array.init will call this function for consecutive values of i *)
        ignore i ;
        let res = head !t' in
        t' := tail !t' ;
        res in
    Array.init (length t) init

(* Generators *)

let cycle t =
    if is_empty t then empty else
    let rec aux = function
        | lazy Nil -> aux t
        | lazy (Cons (x, t')) -> lazy (Cons (x, aux t')) in
    aux t

let rec repeat ?count x = match count with
    | None -> lazy (Cons (x, repeat x))
    | Some n -> if n <= 0 then empty else lazy (Cons (x, repeat ~count:(n-1) x))

let rec stammer n = function
    | lazy Nil as nil -> nil
    | lazy (Cons (x, t')) -> lazy (Cons (x, cat (repeat ~count:(n-1) x) (stammer n t')))

let rec dropwhile f t = match t with
    | lazy Nil as nil -> nil
    | lazy (Cons (x, t')) -> if f x then dropwhile f t' else t

let rec takewhile f = function
    | lazy Nil as nil -> nil
    | lazy (Cons (x, t')) -> if f x then lazy (Cons (x, takewhile f t')) else empty

let rec skip n t = if n = 0 then t else skip (n-1) (tail t)

let one_every n t =
    let rec skip' n t =
        if n = 0 then take_one t else match t with
        | Nil -> Nil
        | Cons (_, lazy t') -> skip' (n-1) t'
    and take_one = function
        | Nil -> Nil
        | Cons (x, t') -> (Cons (x, lazy (skip' (n-1) (Lazy.force t')))) in
    lazy (take_one (Lazy.force t))

let slice ?(step=1) ?dropwhile:du ?takewhile:da t =
    let cut_head = match du with
        | None -> t
        | Some f -> dropwhile f t in
    let chopped = match da with
        | None -> cut_head
        | Some f -> takewhile f cut_head in
    one_every step chopped

let rec zip2 t1 t2 = match t1 with
    | lazy Nil -> lazy Nil
    | lazy (Cons (h1, t1')) -> (match t2 with
        | lazy Nil -> lazy Nil
        | lazy (Cons (h2, t2')) -> lazy (Cons ((h1, h2), zip2 t1' t2')))

let rec zip2_longest t1 t2 x1 x2 =
    if is_empty t1 && is_empty t2 then empty else
        let h1 = if is_empty t1 then x1 else head t1
        and h2 = if is_empty t2 then x2 else head t2 in
        lazy (Cons ((h1, h2), zip2_longest (tail t1) (tail t2) x1 x2))

let rec altern2 t1 t2 = match t1 with
    | lazy Nil -> lazy Nil
    | lazy (Cons (h1, t1')) -> lazy (Cons (h1, altern2 t2 t1'))

let rec firsts n t = lazy (match t with
    | lazy Nil -> Nil
    | lazy (Cons (x, t')) -> if n <= 0 then Nil else Cons (x, firsts (n-1) t')
)

let lasts n t =
    let rec aux h' = function
        | lazy Nil -> h'
        | lazy (Cons (_, t')) -> aux (tail h') t' in
    aux t (skip n t)

let groupby t f_key =
    (* prep_key {1,2,3} = (f_key 1),{1,2,3} *)
    let prep_key t =
        assert (not (is_empty t)) ;
        (f_key (head t)), t in
    (* headtail k3,{3,2,1} k4,{4,5,6} = {(k4,{4,3,2,1}), (k5,{5,6})} *)
    let rec headtail p1 p2 =
        match p2 with
        | _, lazy Nil -> singleton p1
        | kb, lazy (Cons (b, t2')) ->
            let ka, t1 = p1 in
            if kb = ka then (
                let p1' = kb, cat t1 (singleton b) in
                if is_empty t2' then singleton p1'
                else headtail p1' (prep_key t2')
            ) else lazy (Cons (p1, groupby' p2))
    and groupby' = function
        | _, lazy Nil -> empty
        | k, t -> headtail (k,empty) (prep_key t) in
    if is_empty t then empty else groupby' (prep_key t)

let uniq t f_key =
    let rec aux prev_k = function
        | lazy Nil as nil -> nil
        | lazy (Cons (x, t')) -> let k_x = f_key x in
            if k_x = prev_k then aux prev_k t'
            else lazy (Cons (x, aux k_x t')) in
    if is_empty t then empty else
        let h = head t in lazy (Cons (h, aux (f_key h) t))

(* reservoir-sampling algorithm *)
let choose n t = if is_empty t then empty else
    let chosen = to_array (zip2 (firsts n t) nat) in
    let rec aux i = function
        | lazy Nil -> ()
        | lazy (Cons (x, t')) ->
            let r = Random.int i in
            if r < n then chosen.(r) <- x, i ;
            aux (i+1) t' in
    aux (n+1) (skip n t) ; (* we already kept the first n item *) ;
    Array.sort (fun (_, i1) (_, i2) -> compare i1 i2) chosen ;
    of_array (Array.map fst chosen)

(* More constructors *)
let range start stop =
    let n = stop - start + 1 in firsts n (of_succ succ start)

let (--) = range

(* Combinatoric generators *)

(* ziphead {{1,2},{3,4,5},{6,7}} = {1,3,6} *)
let rec ziphead = function
    | lazy Nil -> empty
    | lazy (Cons (t, tt')) -> lazy (Cons (head t, ziphead tt'))

(* ziptail {{1,2},{3,4,5},{6,7}} = {{2},{4,5},{7}} *)
let rec ziptail = function
    | lazy Nil -> empty
    | lazy (Cons (t, tt')) -> lazy (Cons (tail t, ziptail tt'))

(* zip {{1,2},{3,4,5},{6,7}} = {{1,3,6},{2,4,7}} *)
let zip tt =
    let rec aux tt' =
        let some_empty = fold tt' false (fun t e -> e || is_empty t) in (* FIXME: use a skip_until *)
        if some_empty then empty
        else lazy (Cons (ziphead tt', aux (ziptail tt'))) in
    if is_empty tt then empty else aux tt

(* product {{1,2,3},{3,4}} = {{1,3},{1,4},{2,3},{2,4},{3,3},..}
 * actually the other way around : {{1,3},{2,3},{3,3},{1,4},...} *)
let product tt =
    let req_len, stt =
        fold tt (1, empty) (fun t (next_stam, new_tt) ->
            let next_t = stammer next_stam t in
            let l = length t in
            next_stam * l, cat new_tt (singleton (cycle next_t))) in
    firsts req_len (zip stt)

let power t n =
    let rec rep n =
        if n = 0 then empty
        else lazy (Cons (t, rep (n-1))) in
    product (rep n)

let prepend_all x tt =
    if is_empty tt then singleton (singleton x) else
    map tt (fun t -> cat (singleton x) t)

let permutations ?len t =
    if is_empty t then empty else
    let li = length t in
    let lo = match len with
        | None -> li
        | Some n -> assert (n<=li) ; n in
    let rec perms' n li lo t =
        if n = li then empty else
        cat (prepend_all (head t) (perms (li-1) (lo-1) (firsts (li-1) (tail t))))
            (perms' (n+1) li lo (tail t))
    and perms li lo t =
        if lo = 0 then empty else
        let t' = cycle t in
        perms' 0 li lo t' in
    perms li lo t

(* combinations {1,2,3} 2 = {{1,2},{1,3},{2,3}} *)
let combinations len t =
    let li = length t in
    assert (len <= li) ;
    let rec combs t len ll =
        if len = 0 then singleton empty
        else if ll = 0 then empty
        else cat (prepend_all (head t) (combs (tail t) (len-1) ll))
                 (combs (tail t) len (ll-1)) in
    if is_empty t or len = 0 then empty
    else combs t len (li - len + 1)

(* {{1,2,3},{4,5},...} -> {1,2,3,4,5,...} *)
let rec flatten = function
    | lazy Nil -> empty
    | lazy (Cons (t, tt')) -> cat t (flatten tt')

let (+@+) s f = map s f |> flatten

(* Comparison *)

let cmp f a b =
    let some x = Some x and none = singleton None in
    let a = (map a some) ++ none
    and b = (map b some) ++ none in
    let s = map2 a b (fun a b -> match a, b with
        | Some a, Some b -> f a b
        | Some _, None   -> 1
        | None,   Some _ -> -1
        | None,   None   -> 0) in
    try head (dropwhile ((=) 0) s) with Empty -> 0

let cmp_shortest f a b =
    let s = map2 a b f in
    try head (dropwhile ((=) 0) s) with Empty -> 0

let rec merge f a b = match a, b with
    | lazy Nil, x -> x
    | x, lazy Nil -> x
    | lazy (Cons (ha, a')), lazy (Cons (hb, b')) ->
        if f ha hb <= 0 then lazy (Cons (ha, merge f a' b))
        else lazy (Cons (hb, merge f a b'))

