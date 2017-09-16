open Pfds_intf

module Red_black_tree_raw (Ord : ORDERED) =
struct
    type e = Ord.t
    type color = R | B
    type t = E | T of (color * t * e * t)

    let empty = E
    let is_empty = function E -> true | _ -> false
    let singleton x = T (B, E, x, E)

    (* Returns the element that compares equal to the one given *)
    let rec find t x = match t with
        | E -> raise Not_found
        | T (_, l, y, r) ->
            let cmp = Ord.compare x y in
            if cmp < 0 then find l x
            else if cmp > 0 then find r x
            else y

    let member t x = match find t x with
      | exception Not_found -> false
      | _ -> true

    let find_best t x =
        let rec loop best_l best_r = function
            | E -> best_l, best_r
            | T (_, l, y, r) ->
                let cmp = Ord.compare x y in
                if cmp < 0 then loop best_l (Some y) l
                else if cmp > 0 then loop (Some y) best_r r
                else let sy = Some y in sy, sy
        in loop None None t

    let get_min t =
        let rec loop min = function
            | E ->
                (match min with None -> raise Not_found
                              | Some m -> m)
            | T (_, l, x, _) -> loop (Some x) l
        in loop None t

    let get_max t =
        let rec loop max = function
            | E ->
                (match max with None -> raise Not_found
                              | Some m -> m)
            | T (_, _, x, r) -> loop (Some x) r
        in loop None t

    let check_invariants t =
        let rec chk prec_col tot_B = function
            | E -> tot_B
            | T (R, l, _, r) ->
                if prec_col = R then failwith "2 successive Reds" ;
                let depth_l = chk R tot_B l
                and depth_r = chk R tot_B r in
                if depth_l <> depth_r then failwith "Not same Black count (R)" ;
                depth_l
            | T (B, l, _, r) ->
                let depth_l = chk B (tot_B+1) l
                and depth_r = chk B (tot_B+1) r in
                if depth_l <> depth_r then failwith "Not same Black count (B)" ;
                depth_l in
        ignore (chk B 0 t)

    let balance_l = function
        | B, T (R, a, x, T (R, b, y, c)), z, d
        | B, T (R, T (R, a, x, b), y, c), z, d ->
            T (R, T (B, a, x, b), y, T (B, c, z, d))
        | body -> T body

    let balance_r = function
        | B, a, x, T (R, b, y, T (R, c, z, d))
        | B, a, x, T (R, T (R, b, y, c), z, d) ->
            T (R, T (B, a, x, b), y, T (B, c, z, d))
        | body -> T body

    let insert t x =
        let rec ins = function
            | E -> T (R, E, x, E)
            | T (color, l, y, r) ->
                let cmp = Ord.compare x y in
                if cmp < 0 then balance_l (color, ins l, y, r)
                else if cmp > 0 then balance_r (color, l, y, ins r)
                else raise Exit in
        try match ins t with
            | T (_, l, y, r) -> T (B, l, y, r)
            | _ -> failwith "Someone messed up the compiler"
        with Exit -> t

    let insert' t x =
        let t' = insert t x in
        check_invariants t' ;
        t'

    (* Deletion will require to be able to decrease the depth (in black nodes) of a subtree
     * (to compensate for the removal of a black node on the other subtree).
     * Returns both the new tree and a bool indicating if the global depth has decreased.
     * Note: we follow the footsteps of J.C. Filliatre implementation as given in his
     * rbset.ml, v1.12 *)
    let decrease_depth_l = function
        (* Easiest way to decrease depth on left: if the root of left subtree is
         * black then recolor it to red (and rebalance *)
        | T (c, T (B, l', x', r'), x, r) ->
            balance_l (B, T (R, l', x', r'), x, r), c = B
        (* Otherwise, if we have a red at left (and then a black at root and
         * other blacks at left and right children of the red node) then rotate the
         * tree so that this red node became the new root and the previous black root
         * its new right child *)
        | T (_b, T (R, l', x', T (_b', l'', x'', r'')), x, r) ->
            (* Notice here that since the depth at left is 1 + the one a right, then
             * the height of l', l'', r'' and r are the same. *)
             T (B, l', x', balance_l (B, T (R, l'', x'', r''), x, r)), false
        | _ -> failwith "Invalid tree for decrease_depth_l"

    let decrease_depth_r = function
        | T (c, l, x, T (B, l', x', r')) ->
            balance_r (B, l, x, T (R, l', x', r')), c = B
        | T (_b, l, x, T (R, T (_b', l'', x'', r''), x', r')) ->
            T (B, balance_r (B, l, x, T (R, l'', x'', r'')), x', r'), false
        | _ -> failwith "Invalid tree for decrease_depth_r"

    (* Then we need a function to extract the minimum (aka leftmost) value out of a tree.
     * Returns the new tree, the min value and a flag indicating if the depth (again,
     * of black nodes only) was reduced *)
    let rec extract_min = function
        | T (R, E, m, r) -> r, m, false
        | T (B, E, m, r) ->
            (match r with
            | T (R, l, x, r) -> T (B, l, x, r), m, false (* recolor the root as black *)
            | _              -> r, m, true) (* Note: for some reason JCF does not believe in a black node here :-/ *)
        (* Apply recursively *)
        | T (c, l, x, r) ->
            let t', m, d = extract_min l in
            let t' = T (c, t', x, r) in
            if d then
                let t', d' = decrease_depth_r t' in t', m, d'
            else
                t', m, false
        | _ -> failwith "Invalid tree for extract_min"

    let delete t x =
        (* returns both t without x and a flag indicating if the depth has decreased *)
        let rec aux = function
            | E -> raise Not_found
            | T (c, l, y, r) ->
                let cmp = Ord.compare x y in
                if cmp < 0 then
                    let l', d = aux l in
                    let t' = T (c, l', y, r) in
                    if d then decrease_depth_r t' else t', false
                else if cmp > 0 then
                    let r', d = aux r in
                    let t' = T (c, l, y, r') in
                    if d then decrease_depth_l t' else t', false
                else ( (* x = y -> reduce right subtree *)
                    if r = E then match c, l with
                        | B, T (R, l, x, r) -> T (B, l, x, r), false
                        | B, x -> x, true
                        | R, x -> x, false
                    else
                        let r', m, d = extract_min r in
                        let t' = T (c, l, m, r') in
                        if d then decrease_depth_l t' else t', false
                ) in
        fst (aux t)

    let delete' t x =
        let t' = delete t x in
        check_invariants t' ;
        t'


    let rec iter f = function
        | E -> ()
        | T (_, l, x, r) ->
            iter f l ;
            f x ;
            iter f r

    include Iterable_impl.Of (struct type t' = t type e' = e let iter = iter end)
end

module Red_black_tree (Ord : ORDERED) :
    SET with type e = Ord.t = Set_ops_impl.Make (Red_black_tree_raw (Ord))
