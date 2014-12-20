open Pfds_intf

module Int = struct
    type t = int
    let compare = compare
end

module Tree = Red_black_tree_impl.Red_black_tree_raw (Int)

let () =
    let n = ref 1000 in
    Random.self_init () ;
    Arg.parse [ "-n", Arg.Set_int n, "n" ]
        (fun str -> Printf.printf "What about %s ?\n" str)
        "Benchmark RB tree.\n" ;
    let rec aux t = function
        | 0 -> t
        | i -> aux (Tree.insert t (Random.int !n)) (i-1) in
    ignore (aux Tree.empty !n)
