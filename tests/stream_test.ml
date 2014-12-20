open Pfds_intf
open LStream

(* We want to check how many values are forced *)
let count = ref 0 (* This counter is global for all users of this module *)
let reset () = count := 0
let succ x = incr count ; x + 1

(* Build the infinite stream of integers starting at 0 *)
let nat = of_succ succ 0

let empty_check () =
    assert (is_empty empty) ;
    assert (length empty = 0)

let singleton_check () =
    let check x =
        let s = singleton x in
        assert (not (is_empty s)) ;
        assert (length s = 1) ;
        assert (mem s x) ;
        assert (is_singleton s) in
    check 1 ;
    check empty

let list_check () =
    let check l =
        let t = of_list l in
        assert (List.length l = length t) ;
        assert (List.rev (to_list_rev t) = l) ;
        assert (to_list t = l) ;
        assert (head t = List.hd l) ;
        assert (to_list (tail t) = List.tl l) ;
        assert (nth t 0 = List.nth l 0) in
    let l = [ 1 ; 2 ; 3 ] in
    let t = of_list l in
    check l ;
    check [ 1 ] ;
    assert (is_empty (of_list [])) ;
    assert (to_list (map t (fun x -> -x)) = [-1 ; -2 ; -3 ]) ;
    assert (to_list (mapi t (fun i _ -> i)) = [ 0 ; 1 ; 2 ]) ;
    assert (fold t 0 (+) = 6) ;
    let even x = x land 1 = 0 in
    assert (to_list (t // even) = [ 2 ]) ;
    assert (to_list (3 -- 5) = [ 3 ; 4 ; 5 ]) ;
    assert (exists nat ((<=) 10)) ;
    assert (mem nat 5)

let array_check () =
    let arr = to_array (firsts 10 nat) in
    Array.iteri (fun i x -> assert (i = x)) arr ;
    let nat' = of_array arr in
    iteri nat' (fun i x -> assert (i = x))

let nth_check () =
    let s1 = of_nth ~limit:10 (fun i -> i) in
    assert (cmp compare s1 (firsts 10 nat) = 0) ;
    assert (to_string (of_string "Foo") = "Foo")

let cmp_check () =
    let s1 = of_list [ 1 ; 2 ; 3 ]
    and s2 = of_list [ 1 ; 2 ; 4 ]
    and s3 = of_list [ 1 ; 2 ; 3 ; 4 ]
    and s4 = of_list [ 1 ; 3 ]
    and c  = cmp compare
    and cs = cmp_shortest compare
    and m  = merge compare in
    assert ( 0 = c  empty empty) ;
    assert (-1 = c  empty nat ) ;
    assert ( 1 = c  nat empty ) ;
    assert ( 0 = c  (firsts 5 nat) (firsts 5 nat)) ;
    assert (-1 = c  (firsts 5 nat) (firsts 6 nat)) ;
    assert ( 1 = c  (firsts 6 nat) (firsts 5 nat)) ;
    assert (-1 = c  (firsts 5 nat) nat) ;
    assert ( 1 = c  nat (firsts 5 nat)) ;
    assert ( 0 = c  s1 s1) ;
    assert (-1 = c  s1 s2) ;
    assert ( 1 = c  s2 s1) ;
    assert (-1 = c  s1 s3) ;
    assert ( 1 = c  s3 s1) ;
    assert (-1 = c  s1 s4) ;
    assert ( 1 = c  s4 s1) ;
    assert ( 0 = cs (firsts 5 nat) (firsts 5 nat)) ;
    assert ( 0 = cs (firsts 5 nat) (firsts 6 nat)) ;
    assert ( 0 = cs (firsts 6 nat) (firsts 5 nat)) ;
    assert ( 0 = cs s1 s1) ;
    assert (-1 = cs s1 s2) ;
    assert ( 1 = cs s2 s1) ;
    assert ( 0 = cs s1 s3) ;
    assert ( 0 = cs s3 s1) ;
    assert (-1 = cs s1 s4) ;
    assert ( 1 = cs s4 s1) ;
    assert (to_list (m s1 s2) = [ 1 ; 1 ; 2 ; 2 ; 3 ; 4 ]) ;
    assert (0 = c s1 (m empty s1)) ;
    assert (to_list (m s4 s4) = [ 1 ; 1 ; 3 ; 3 ]) ;
    assert (0 = c (m s3 s3) (stammer 2 s3))

let map_check () =
    let none_even n = if n land 1 = 1 then Some n else None in
    let odds = map_opt nat none_even in
    assert (to_list (firsts 4 odds) = [ 1 ; 3 ; 5 ; 7 ]) ;
    let odds'= nat /@ none_even // ((<>) None) /@ Misc.get_option in
    assert (to_list (firsts 4 odds') = [ 1 ; 3 ; 5 ; 7 ])

let cat_check () =
    reset () ;
    let r1 = firsts 5 nat in
    let c = cat r1 r1 in
    assert (!count <= 2) ; (* We are allowed to have evaluated 1 nat here *)
    assert (length c = 10) ;
    assert (is_empty (cat empty empty)) ;
    assert (length (cat (singleton 1) empty) = 1) ;
    assert (length (cat empty (singleton 1)) = 1) ;
    assert (head (append empty 1) = 1) ;
    assert (head (prepend 1 empty) = 1)

let gen_check () =
    reset () ;
    let t = firsts 3 nat in
    let c = cycle t in
    let s = stammer 3 c in
    assert (!count <= 1) ;
    assert (to_list (firsts 12 s) = [ 0;0;0; 1;1;1; 2;2;2; 0;0;0 ]) ;
    assert (head (skip 10 nat) = 10) ;
    assert (head (dropwhile (fun x -> x < 20) nat) = 20) ;
    assert (length (takewhile (fun x -> x < 20) nat) = 20) ;
    assert (length (one_every 3 (firsts 30 nat)) = 10) ;
    assert (length (one_every 3 (firsts 1 nat)) = 1) ;
    assert (length (one_every 3 (firsts 0 nat)) = 0) ;
    assert (head (one_every 3 (firsts 30 nat)) = 0) ;
    assert (length (lasts 3 (firsts 30 nat)) = 3) ;
    assert (is_empty (cycle empty))

let group_check () =
    let t = of_list [ 1 ; 1 ; 2 ; 2 ; 2 ; 3 ; 4 ; 4 ] in
    let g = groupby t (fun x -> x land 1) in
    assert (to_list (map g (fun (b,t) -> b, to_list t)) =
        [ 1,[1;1] ; 0,[2;2;2] ; 1,[3] ; 0,[4;4] ]) ;
    let u = uniq t (fun x -> x) in
    assert (to_list u = [ 1 ; 2 ; 3 ; 4 ])

let zip_check () =
    let tt = of_list [nat ; skip 1 nat ; of_list [3;1;4]] in
    let z = zip tt in
    assert (to_list (map z to_list) =
        [ [0;1;3] ; [1;2;1] ; [2;3;4] ]) ;
    assert (is_empty (zip empty)) ;
    assert (is_empty (zip (singleton empty))) ;
    let z = zip (singleton (firsts 1 nat)) in
    assert (to_list (map z to_list) = [ [0] ])

let altern_check () =
    let t1 = firsts 3 nat in
    let t2 = of_list [ 6 ; 7 ] in
    let a = altern2 t1 t2 in
    assert (to_list a = [ 0 ; 6 ; 1 ; 7 ; 2 ])

let check_fail f p =
    try (f p ; assert false)
    with _ -> ()

let prod_check () =
    let check tt expected =
        let prods = product tt in
        assert (to_list (map prods to_list) = expected) in
    let t1 = (firsts 3 nat)
    and t2 = (firsts 2 (skip 3 nat)) in
    check (of_list [t1;t2]) [ [0;3] ; [1;3] ; [2;3] ; [0;4] ; [1;4] ; [2;4] ] ;
    check (of_list [t1]) [ [0] ; [1] ; [2] ] ;
    check empty [] ;
    check (of_list [t2;t2;t2]) [ [3;3;3] ; [4;3;3] ; [3;4;3] ; [4;4;3] ;
                                 [3;3;4] ; [4;3;4] ; [3;4;4] ; [4;4;4] ] ;
    check_fail product (singleton empty)

let perm_check () =
    let check t ?len expected =
        let perms = permutations ?len t in
        assert (to_list (map perms to_list) = expected) in
    let t = (firsts 3 nat) in
    check t [ [0;1;2] ; [0;2;1] ; [1;2;0] ; [1;0;2] ; [2;0;1] ; [2;1;0] ] ;
    check t ~len:2 [ [0;1] ; [0;2] ; [1;2] ; [1;0] ; [2;0] ; [2;1] ] ;
    check empty [] ;
    check_fail permutations (singleton empty) ;
    check_fail (permutations ~len:4) t ;
    check_fail permutations t

let comb_check () =
    let check t len expected =
        let combs = combinations len t in
        assert (to_list (map combs to_list) = expected) in
    let t = (firsts 5 nat) in
    check t 5 [ [0;1;2;3;4] ] ;
    check t 4 [ [0;1;2;3] ; [0;1;2;4] ; [0;1;3;4] ; [0;2;3;4] ; [1;2;3;4] ] ;
    check t 3 [ [0;1;2] ; [0;1;3] ; [0;1;4] ; [0;2;3] ; [0;2;4] ; [0;3;4] ;
                [1;2;3] ; [1;2;4] ; [1;3;4] ;
                [2;3;4] ] ;
    check t 2 [ [0;1] ; [0;2] ; [0;3] ; [0;4] ;
                [1;2] ; [1;3] ; [1;4] ;
                [2;3] ; [2;4] ;
                [3;4] ] ;
    check t 1 [ [0] ; [1] ; [2] ; [3] ; [4] ] ;
    check t 0 [] ;
    check empty 0 [] ;
    check_fail (combinations 1) empty ;
    check_fail (combinations 6) t

let flatten_check () =
    assert (firsts 4 nat +@+ (fun n -> firsts n nat) |> to_list = [ 0; 0;1; 0;1;2 ])

(* Check that in some interresting situations items are computed only once *)
let uniq_check () =
    (* one_every must not compute not required items *)
    let s = of_succ (fun x -> assert (x = 0) ; x + 1) 0 in
    let s' = one_every 4000 s in
    let s'' = one_every 4000 s' in
    ignore (head s'') ;
    ignore (head (map s'' float_of_int)) ;
    ignore (zip (of_list [ s ; s' ; s'' ]))

(* Check that the choose is sufficently random *)
let choose_check () =
    Random.self_init () ;
    let t_len = 10 in
    let t = firsts t_len nat in
    (* check basic properties *)
    assert (is_empty (choose 0 t)) ;
    assert (length (choose 5 t) = 5) ;
    (* check order is preserved *)
    assert (fst (fold (choose 5 t) (true, -1) (fun x (p, x') -> p && x > x', x))) ;
    (* check randomness *)
    let nb_choice = ref 0 in
    let nb_hit = Array.make t_len 0 in
    let incr_hit x = nb_hit.(x) <- nb_hit.(x)+1 ; incr nb_choice in
    let report_stats () =
        let _i, sum, sum2 = Array.fold_left (fun (x, s, s2) hits -> x+1, s+x*hits, s2+x*x*hits) (0, 0, 0) nb_hit in
        let avg = (float_of_int sum)/.(float_of_int !nb_choice) in
        let sigma = sqrt ((float_of_int sum2)/.(float_of_int !nb_choice) -. avg*.avg) in
        let sigma_theory = (float_of_int (t_len-1))/.sqrt (12.) in
        Printf.printf "Average number (from 0 to %d): %f (sigma=%f instead of %f)\n" (t_len-1) avg sigma sigma_theory in
    (* ask fo 1, then 2, then etc up to 10 items, and count how many times each number is chosen *)
    for run = 0 to 1000 do
        for size = 0 to t_len do
            let t' = choose size t in
            iter t' incr_hit
        done
    done ;
    Printf.printf "Choosing n from the 10 first nat numbers:\n" ;
    report_stats () ;
    (* compare with bare random *)
    Array.iteri (fun i _ -> nb_hit.(i) <- 0) nb_hit ;
    nb_choice := 0 ;
    for run = 0 to 1000 do incr_hit (Random.int t_len) done ;
    Printf.printf "Compare with mere random:\n" ;
    report_stats ()

(* Now for more realistic examples *)

let fibo_check () =
    let fibo =
        let rec fib a b = let s = a+b in lazy (Cons (s, fib b s)) in
        lazy (Cons (1, lazy (Cons (1, fib 1 1)))) in
    (* Fibonacci sequence is made of 2 odd numbers then 1 even, etc...
     * Let's check. *)
    let is_even x = x land 1 = 0 in
    let is_odd x = not (is_even x) in
    let max_succ p =
        (* What's the max number of consecutive even numbers in the first 100k ? *)
        let h = filter (groupby (firsts 10_000 fibo) p) fst in
        fold h 0 (fun (_, t) -> max (length t)) in
    assert (max_succ is_even = 1) ;
    assert (max_succ is_odd = 2)

type root_test = { f : float -> float ; start : float ; root : float }
let root_check () =
    (* This list converges toward the root of f (or not) *)
    let converg f df start =
        let newtons f df x = x -. ((f x) /. (df x)) in
        of_succ (newtons f df) start in
    (* Approx derivative of any function *)
    let df f epsilon x = (f (x +. epsilon) -. f x) /. epsilon in
    (* Check we reach function's root *)
    let check_f params =
        let epsilon = 0.00001 in
        let c = converg params.f (df params.f epsilon) params.start in
        let root = head (dropwhile (fun x -> abs_float (params.f x) > epsilon) (firsts 10_000 c)) in
        (*Printf.printf "root found at %f (instead of %f)\n%!" root params.root ;*)
        assert (abs_float (root -. params.root) < 5. *. epsilon) in
    (* Now we define some functions and their starting points and expected root *)
    let tests = [ { f = (fun x -> x *. x -. 1.) ; start = 0.5 ; root = 1. } ;
                  { f = (fun x -> sin x)        ; start = 1.  ; root = 0. } ] in
    List.iter check_f tests

let puzzle_check () =
    let numbers = 2 -- 99 in
    let all = (product (repeat ~count:2 numbers) /@ (fun s -> nth s 0, nth s 1)) // (fun (a, b) -> a >= b) in
    let factors  p t = (* returns the factors of p that are in t *)
        t // (fun (a, b) -> a*b = p)
    and summands s t = (* returns the summands of p that are in t *)
        t // (fun (a, b) -> a+b = s)
    and print_stream oc t =
        let maxlen = 8 in
        let l = to_list (firsts maxlen t) in
        let rec aux = function [] -> () | (a, b) :: l' -> Printf.fprintf oc "(%d,%d), " a b ; aux l' in
        Printf.fprintf oc "[ " ; aux l ; Printf.fprintf oc "%s ]" (if length t > maxlen then " ..." else "") in
    let print_rem t =
        Printf.printf "\t%d possibilities (%a)\n%!" (length t) print_stream t
    in
    Printf.printf "Let's a and b be two numbers between 2 to 99, with a >= b.\n" ;
    print_rem all ;
    Printf.printf "But Mr. P is told the product while Mr. S is told the sum.\n%!" ;
    Printf.printf "Mr. P: \"I do not know the numbers.\"\n%!" ; (* ie. there are more than one factorization *)
    let rem = all // (fun (a, b) -> not (is_singleton (factors (a*b) all))) in
    print_rem rem ;
    Printf.printf "Mr. S: \"I knew you do not know.\"\n%!" ; (* amongst all the summands of S, none have only one factorization *)
    let rem = rem (* we take only s1 to avoid having to perform the intersection afterward *) // (fun (a, b) ->
        let summands = summands (a+b) all in
        not (exists summands (fun (a, b) -> is_singleton (factors (a*b) all)))) in
    print_rem rem ;
    Printf.printf "Mr. P: \"I know the numbers!\"\n%!" ; (* there is now only one way to factorize P *)
    let rem = rem // (fun (a, b) -> is_singleton (factors (a*b) rem)) in
    print_rem rem ;
    Printf.printf "Mr. S: \"I know the numbers too!\"\n%!" ; (* there is now only one way to obtain S *)
    let rem = rem // (fun (a, b) -> is_singleton (summands (a+b) rem)) in
    print_rem rem ;
    assert (to_list rem = [ 13, 4 ]) (* so P was 52 and S was 17 *)

let () =
    empty_check () ;
    singleton_check () ;
    list_check () ;
    array_check () ;
    nth_check () ;
    cmp_check () ;
    map_check () ;
    cat_check () ;
    gen_check () ;
    group_check () ;
    zip_check () ;
    altern_check () ;
    prod_check () ;
    perm_check () ;
    comb_check () ;
    flatten_check () ;
    uniq_check () ;
    choose_check () ;
    fibo_check () ;
    root_check () ;
    puzzle_check ()

