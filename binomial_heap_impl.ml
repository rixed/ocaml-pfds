open Pfds_intf

(* TODO: why not use stacks instead of these lists ? *)
module Binomial_heap_raw (Elem_: ORDERED) =
struct
	module Elem = Elem_

	module Tree =
	struct
		(* TODO: after testing that this works, perform exercise 3.6 *)
		type t = int * Elem.t * t list
	
		let singleton x = 0, x, []
		let rank (r, _, _) = r
		let root (_, x, _) = x
		(* Link two trees of same rank *)
		let link (r1, x1, c1 as t1) (r2, x2, c2 as t2) =
			assert (r1 = r2) ;
			if Elem.compare x1 x2 <= 0 then r1+1, x1, t2 :: c1
			else r2+1, x2, t1 :: c2
	end

	type t = Tree.t list

	let empty = []
	let is_empty = function [] -> true | _ -> false (* Stack.is_empty... *)
	let singleton x = [Tree.singleton x]

	(* Insert in the heap a new tree of rank <= min rank of the heap *)
	let rec ins_tree t = function
		| [] -> [t]
		| t' :: ts' as ts ->
			if Tree.rank t < Tree.rank t' then t :: ts
			else ins_tree (Tree.link t t') ts'
	
	let insert ts x = ins_tree (Tree.singleton x) ts

	let rec merge ts1 ts2 = match ts1 with
		| [] -> ts2
		| t1 :: ts1' -> (match ts2 with
			| [] -> ts1
			| t2 :: ts2' ->
				let rank1 = Tree.rank t1
				and rank2 = Tree.rank t2 in
				if rank1 < rank2 then t1 :: merge ts1' ts2
				else if rank1 > rank2 then t2 :: merge ts1 ts2'
				else ins_tree (Tree.link t1 t2) (merge ts1' ts2'))
	
	(* remove the tree with minimal root and return both this tree and remaining heap *)
	let rec remove_min_tree = function
		| [] -> raise Empty
		| [t] -> t, []
		| t :: ts ->
			let t', ts' = remove_min_tree ts in
			if Elem.compare (Tree.root t) (Tree.root t') <= 0 then t, ts else t', ts'
	
	let min ts =
		let t = fst (remove_min_tree ts) in
		Tree.root t
	
	let delete_min ts =
		let (_, _, ts1), ts2 = remove_min_tree ts in
		merge (List.rev ts1) ts2
end

module Binomial_heap (Elem_: ORDERED) :
	HEAP with module Elem = Elem_ = Binomial_heap_raw (Elem_)

