open Pfds_intf

module Heap_ops (Heap_: HEAP) :
	HEAP_OPS with module Heap = Heap_ =
struct
	module Heap = Heap_
	open Heap

	let of_list l =
		let rec merge_adjascent prevs = function
			| [] -> prevs
			| [ h ] -> h :: prevs
			| h1 :: h2 :: rest -> merge_adjascent (merge h1 h2 :: prevs) rest in
		let rec merge_all = function
			| [] -> empty
			| [ h ] -> h
			| l -> merge_all (merge_adjascent [] l) in
		merge_all (List.map singleton l)
end
