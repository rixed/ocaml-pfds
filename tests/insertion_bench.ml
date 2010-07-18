open Pfds_intf

module type INSERABLE =
sig
	type t
	type e = String.t
	val empty    : t
	val is_empty : t -> bool
	val insert   : t -> e -> t
end

module Ins1 : INSERABLE = Btree_impl.Unbalanced_set_raw (String)
module Ins2 : INSERABLE = Leftist_heap_impl.Leftist_heap_raw (String)
module Ins3 : INSERABLE = Weight_leftist_heap_impl.Weight_leftist_heap_raw (String)
module Ins4 : INSERABLE = Binomial_heap_impl.Binomial_heap_raw (String)
module Ins5 : INSERABLE = Red_black_tree_impl.Red_black_tree_raw (String)

module Inserable_bench (Ins : INSERABLE) =
struct
	open Ins
	let rec string_rev s =
		let n = String.length s in
		for i = 0 to (n - 1) / 2 do
			let c = s.[i] in
			s.[i] <- s.[n-i-1] ;
			s.[n-i-1] <- c ;
		done ;
		s

	let insert_file fname =
		let cin = open_in fname in
		let rec insert_next_line i =
			(*Revert the word in order to unsort the list *)
			let word = string_rev (input_line cin) in
			insert_next_line (insert i word) in
		try insert_next_line empty
		with End_of_file -> close_in cin
	
	let rec insert_int h min max =
		if min < max then
			let s = string_of_int (Random.int 9999999) in
			insert_int (insert h s) (min+1) max
		else h
	
	let insert_n = insert_int empty 0

end

module Bench1 = Inserable_bench (Ins1)
module Bench2 = Inserable_bench (Ins2)
module Bench3 = Inserable_bench (Ins3)
module Bench4 = Inserable_bench (Ins4)
module Bench5 = Inserable_bench (Ins5)

let bench_words fname =
	let bench_res = Benchmark.latencyN ~repeat:3 4L
		[ "Unbalanced set", Bench1.insert_file, fname ;
		  "Leftist heap",   Bench2.insert_file, fname ;
		  "Weight heap",    Bench3.insert_file, fname ;
		  "Binomial heap",  Bench4.insert_file, fname ;
		  "Red-Black tree", Bench5.insert_file, fname ] in
	print_newline () ;
	Benchmark.tabulate bench_res

(* You ca plot this output with gnuplot using :
 * plot 'insert.log' using 1:2:(column(-2)) linecolor variable *)
let bench_time max =
	let step = max / 20 in
	let time title f =
		let rec aux n =
			if n >= max then Printf.printf "\n\n%!"
			else (
				let t0 = Benchmark.make 0L in
				ignore (f n) ;
				let b = Benchmark.sub (Benchmark.make 0L) t0 in
				Printf.printf "%d %f\n" n b.Benchmark.utime ;
				aux (n + step)
			) in
		Printf.printf "# %s\n" title ;
		aux 0 in
	time "Unbalanced set" Bench1.insert_n ;
	time "Leftist heap"   Bench2.insert_n ;
	time "Weight heap"    Bench3.insert_n ;
	time "Binomial heap"  Bench4.insert_n ;
	time "Red-Black tree" Bench5.insert_n
	
let () =
	Random.self_init () ;
	Arg.parse [
		"-words", Arg.String bench_words, "Compare insertions of the given file's lines" ;
		"-time", Arg.Int bench_time, "Times insertions of up to that many random strings" ]
		(fun str -> Printf.printf "What about %s ?\n" str)
		"Benchmark various insertables data structures.\n"
