open Pfds_intf

module type INSERABLE =
sig
	type t
	type elmt = String.t
	val empty    : t
	val is_empty : t -> bool
	val insert   : t -> elmt -> t
end

module Ins1 : INSERABLE = Btree_impl.Unbalanced_set_raw (String)
module Ins2 : INSERABLE = Leftist_heap_impl.Leftist_heap_raw (String)
module Ins3 : INSERABLE = Weight_leftist_heap_impl.Weight_leftist_heap_raw (String)
module Ins4 : INSERABLE = Binomial_heap_impl.Binomial_heap_raw (String)

module Inserable_bench (Ins : INSERABLE) =
struct
	open Ins
	let insert_file fname =
		let cin = open_in fname in
		let rec insert_next_line i =
			insert_next_line (insert i (input_line cin)) in
		try insert_next_line empty
		with End_of_file -> close_in cin
	
	let rec insert_int h min max =
		if min < max then insert_int (insert h (string_of_int min)) (min+1) max
		else h

	let test () =
		insert_file "/usr/share/dict/words" ;
		Gc.compact ()
end

module Bench1 = Inserable_bench (Ins1)
module Bench2 = Inserable_bench (Ins2)
module Bench3 = Inserable_bench (Ins3)
module Bench4 = Inserable_bench (Ins4)

let () =
	let bench_res = Benchmark.latencyN ~repeat:3 4L
		[ "Binomial heap",  Bench4.test, () ;
		  "Weight heap",    Bench3.test, () ;
		  "Leftist heap",   Bench2.test, () ;
		  "Unbalanced set", Bench1.test, () ] in
	print_newline () ;
	Benchmark.tabulate bench_res

