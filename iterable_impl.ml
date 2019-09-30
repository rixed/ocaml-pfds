module type ITER =
sig
    type t'
    type e'
    val iter : (e' -> unit) -> t' -> unit
end

module Of (I : ITER) =
struct
    include I

    let iteri f t =
        let c = ref (-1) in
        iter (fun x -> incr c ; f !c x) t

    let fold_left f b t =
        let b' = ref b in
        let f' x = b' := f !b' x in
        iter f' t ;
        !b'

    let fold_right f t b = fold_left (fun x b -> f b x) b t

    let length t = fold_left (fun c _ -> c+1) 0 t

    let find_first f t =
        let res = ref None in
        (try iter (fun x -> if f x then (res := Some x ; raise Exit)) t
        with Exit -> ()) ;
        match !res with None -> raise Not_found | Some x -> x

    let exists f t =
        try ignore (find_first f t) ; true
        with Not_found -> false

    let to_list t =
        fold_left (fun l x -> x::l) [] t |>
        List.rev

    let to_array t =
        Array.of_list (to_list t)

end

(* Now the generic version.
 * Because now the callback to the iter function must accept all types then the same
 * code as above must be copied. How sad. *)

module type ITER_GEN =
sig
    type 'a t'
    val iter : ('a -> unit) -> 'a t' -> unit
end

module Of_gen (I : ITER_GEN) =
struct
    include I

    let iteri f t =
        let c = ref (-1) in
        iter (fun x -> incr c ; f !c x) t

    let fold_left f b t =
        let b' = ref b in
        let f' x = b' := f !b' x in
        iter f' t ;
        !b'

    let fold_right f t b = fold_left (fun x b -> f b x) b t

    let length t = fold_left (fun c _ -> c+1) 0 t

    let find_first f t =
        let res = ref None in
        (try iter (fun x -> if f x then (res := Some x ; raise Exit)) t
        with Exit -> ()) ;
        match !res with None -> raise Not_found | Some x -> x

    let exists f t =
        try ignore (find_first f t) ; true
        with Not_found -> false

    let to_list t =
        fold_left (fun l x -> x::l) [] t |>
        List.rev

    let to_array t =
        Array.of_list (to_list t)

    let to_string t =
        let str = Bytes.create (length t) in
        iteri (Bytes.set str) t ;
        Bytes.to_string str

    let to_file fname t =
        let ochn = open_out fname in
        Pfds_misc.with_dispose close_out
            (fun ochn -> (iter (output_char ochn)) t)
            ochn
end

