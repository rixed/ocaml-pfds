open Pfds_intf

(* TODO: why not use stacks instead of these lists ? *)
module Binomial_heap_raw (Ord : ORDERED) =
struct
    type e = Ord.t

    module Tree =
    struct
        type lst = Empty | Cons of t * lst
        and t = e * lst

        let singleton x = x, Empty
        let root (x, _) = x
        (* Link two trees of same rank *)
        let link (x1, c1 as t1) (x2, c2 as t2) =
            if Ord.compare x1 x2 <= 0 then x1, Cons (t2, c1)
            else x2, Cons (t1, c2)
    end

    type t = (int * Tree.t) list

    let empty = []
    let is_empty = function [] -> true | _ -> false (* Stack.is_empty... *)
    let singleton x = [0, Tree.singleton x]

    (* Insert in the heap a new tree of rank <= min rank of the heap *)
    let rec ins_tree r t = function
        | [] -> [r, t]
        | (r', t') :: ts' as ts ->
            if r < r' then (r, t) :: ts
            else ins_tree (r+1) (Tree.link t t') ts'

    let insert ts x = ins_tree 0 (Tree.singleton x) ts

    let rec merge ts1 ts2 = match ts1 with
        | [] -> ts2
        | (r1, t1) :: ts1' -> (match ts2 with
            | [] -> ts1
            | (r2, t2) :: ts2' ->
                if r1 < r2 then (r1, t1) :: merge ts1' ts2
                else if r1 > r2 then (r2, t2) :: merge ts1 ts2'
                else ins_tree (r1+1) (Tree.link t1 t2) (merge ts1' ts2'))

    (* remove the tree with minimal root and return both this tree (with rank) and remaining heap *)
    let rec remove_min_tree = function
        | [] -> raise Empty
        | [x] -> x, []
        | (r, t) :: ts ->
            let (r', t'), ts' = remove_min_tree ts in
            if Ord.compare (Tree.root t) (Tree.root t') <= 0 then (r, t), ts else (r', t'), ts'

    let min ts =
        let (_, t), _ = remove_min_tree ts in
        Tree.root t

    let delete_min ts =
        let (r, (_, ts1)), ts2 = remove_min_tree ts in
        let rec makeH prev = function
            | Tree.Empty -> prev
            | Tree.Cons (t, ts) -> makeH ((r-1, t) :: prev) ts in
        merge (makeH [] ts1) ts2

end

module Binomial_heap (Ord : ORDERED) :
    HEAP with type e = Ord.t = Binomial_heap_raw (Ord)

