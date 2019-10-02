open Prefix_tree

module M = Make (StringKey)

let not_found t k =
  let open M in
  try
    ignore (lookup t k) ;
    false
  with Not_found ->
    true

let () =
  let open M in
  let p_data oc d =
    Printf.fprintf oc "%d" d
  and p_key oc k =
    Printf.fprintf oc "%S" (String.sub k.data k.offset k.length) in
  let dump = dump p_key p_data in
  ignore dump ;
  let assert_not_found f =
    try
      f () ;
      assert false
    with Not_found -> () in
  (* Test empty tree: *)
  assert (is_empty empty) ;
  assert (not_found empty "bar") ;
  assert (length empty = 0) ;
  (* Test with the empty key: *)
  let t = add "" 42 empty in
  assert (length t = 1) ;
  assert (not (is_empty t)) ;
  assert (lookup t "" = 42) ;
  assert (not_found t "f") ;
  assert (not_found t "foobar") ;
  (* Tree with a non trivial key: *)
  let t = add "foobar" 17 t in
  assert (length t = 2) ;
  assert (lookup t "" = 42) ;
  assert (lookup t "foobar" = 17) ;
  assert (not_found t "barbaz") ;
  assert (not_found t "fooba") ;
  assert (not_found t "foobarz") ;
  (* Test splitting a prefix: *)
  let t = add "foo" 51 t in
  assert (length t = 3) ;
  assert (lookup t "" = 42) ;
  assert (lookup t "" = 42) ;
  assert (lookup t "foo" = 51) ;
  assert (not_found t "fo") ;
  assert (not_found t "foob") ;
  assert (lookup t "foobar" = 17) ;
  assert (not_found t "barbaz") ;
  assert (not_found t "fooba") ;
  assert (not_found t "foobarz") ;
  assert (not_found t "bar") ;
  (* Test replacing values: *)
  let t = add "foo" 52 t in
  assert (length t = 3) ;
  assert (lookup t "" = 42) ;
  assert (lookup t "" = 42) ;
  let t = add "" 43 t in
  assert (length t = 3) ;
  assert (lookup t "" = 43) ;
  assert (lookup t "foo" = 52) ;
  (* Test removing values: *)
  let t' = remove "" t in
  assert (length t' = 2) ;
  assert (not_found t' "") ;
  assert (lookup t' "foo" = 52) ;
  assert (lookup t' "foobar" = 17) ;
  let t' = remove "foo" t' in
  assert (length t' = 1) ;
  assert (not_found t' "foo") ;
  assert (lookup t' "foobar" = 17) ;
  let t' = remove "foobar" t' in
  assert (length t' = 0) ;
  assert (not_found t' "foobar") ;
  assert (is_empty t') ;
  (* Test removing in a different order: *)
  let t' = remove "foobar" t in
  assert (length t' = 2) ;
  assert (not_found t' "foobar") ;
  assert (lookup t' "foo" = 52) ;
  assert (lookup t' "" = 43) ;
  let t' = remove "foo" t' in
  assert (length t' = 1) ;
  assert (not_found t' "foo") ;
  assert (lookup t' "" = 43) ;
  let t' = remove "" t' in
  assert (length t' = 0) ;
  assert (not_found t' "") ;
  assert (is_empty t') ;
  (* Test removing an unbound key raises Not_found: *)
  assert_not_found (fun () -> remove "pas glop" t) ;
  (* Test that we get the same tree regardless of order of insertion: *)
  let rec insert_loop val_of_idx i t =
    if i >= 1000 then t else
    let v = val_of_idx i in
    let k = string_of_int v in
    insert_loop val_of_idx (i+1) (add k v t) in
  let t1 = insert_loop (fun i -> i) 0 empty
  and t2 = insert_loop ((-) 999) 0 empty in
  assert (length t1 = 1000) ;
  assert (length t2 = 1000) ;
  assert (compare t1 t2 = 0) ;
  (* Test fold by prefix: *)
  let t =
    empty |>
    add "foo" 0x1 |>
    add "bar" 0x2 |>
    add "baz" 0x4 |>
    add "foobar" 0x8 |>
    add "foobaz" 0x10 |>
    add "glop" 0x20 |>
    add "glop glop" 0x40 in
  let merge_keys prefix =
    fold t ~prefix (fun k v u -> u lor v) 0 in
  assert (merge_keys "zzz" = 0) ;
  assert (merge_keys "" = 0x7F) ;
  assert (merge_keys "foo" = 0x19) ;
  assert (merge_keys "fo" = 0x19) ;
  assert (merge_keys "f" = 0x19) ;
  assert (merge_keys "glop glo" = 0x40) ;
  assert (merge_keys "glop glop" = 0x40) ;
  assert (merge_keys "glop glop glop" = 0)
