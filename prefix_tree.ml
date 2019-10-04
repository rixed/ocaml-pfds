(* As the prefix tree will compact keys without value we expect to be able
 * to efficiently be able to compare subkeys.
 * Keys are expected to be immutable. *)

let debug = false

type 'a slice = { data : 'a ; offset : int ; length : int }

let slice_consume slice l =
  assert (l <= slice.length) ;
  { slice with offset = slice.offset + l ;
               length = slice.length - l }

let make_empty_slice data =
  { data ; offset = 0 ; length = 0 }

let sub_slice slice o l =
  assert (o <= slice.length) ;
  assert (o + l <= slice.length) ;
  { data = slice.data ; offset = slice.offset + o ; length = l }

module type KEY =
sig
  type t

  val empty : t

  val length : t -> int
  (** [length k] returns the length of key [k]. *)

  val compare_slice : t slice -> t slice -> int
  (** [compare_slice s1 s2] compares key slices [s1] and [s2] and returns and
   * integer that's smaller, equal or greater to 0 depending on how those
   * slices compare. *)

  val common_prefix : t slice -> t slice -> int
  (** [common_prefix s1 s2] returns the length of the longest
   * common prefix of [s1] and [s2]. *)

  val cat_slices : t slice -> t slice -> t slice
  (** [cat_slices s1 s2] returns a slice representing the concatenation of
   * [s1] and [s2], trying not to copy data needlessly. *)

  val of_slice : t slice -> t
  (** [of_slice s] extract the slice from the data *)
end

(* Some possible implementations:
 * TODO: a faster one based on ropes *)

module StringKey : KEY with type t = string =
struct
  type t = string

  let empty = ""

  let length = String.length

  let compare_slice s1 s2 =
    let i_max = min s1.length s2.length in
    assert (i_max <= String.length s1.data && i_max <= String.length s2.data) ;
    let rec loop i =
      if i >= i_max then 0 else
      let c = compare s1.data.[s1.offset + i] s2.data.[s2.offset + i] in
      if c <> 0 then c
      else loop (i + 1) in
    loop 0

  let common_prefix s1 s2 =
    let i_max = min s1.length s2.length in
    let rec loop i =
      if i >= i_max || s1.data.[s1.offset + i] <> s2.data.[s2.offset + i]
      then i
      else loop (i + 1) in
    loop 0

  let of_slice s =
    if s.offset = 0 && s.length = String.length s.data then
      s.data
    else
      String.sub s.data s.offset s.length

  let cat_slices s1 s2 =
    if s1.data == s2.data &&
       s1.offset + s1.length = s2.offset
    then
      { s1 with length = s1.length + s2.length }
    else
      let data = of_slice s1 ^ of_slice s2 in
      { data ; offset = 0 ; length = String.length data }
end

module Make (Key : KEY) =
struct
  type 'a t =
    { subkey : Key.t slice ;
      value : 'a option ;
      (* Children are ordered by key, with no common prefix: *)
      children : 'a t list }

  let rec compare t1 t2 =
    let c = Key.compare_slice t1.subkey t2.subkey in
    if c <> 0 then c else
    let c = Pervasives.compare t1.value t2.value in
    if c <> 0 then c else
    let rec compare_children = function
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: children1', c2 :: children2' ->
          let c = compare c1 c2 in
          if c <> 0 then c
          else compare_children (children1', children2')
    in
    compare_children (t1.children, t2.children)

  let empty =
    let subkey = { data = Key.empty ; offset = 0 ; length = 0 } in
    { subkey ; value = None ; children = [] }

  let is_empty t =
    t.value = None && t.children = []

  let rec dump p_key p_data ?(indent="") t =
    Printf.printf "%s%a => {\n" indent p_key t.subkey ;
    (match t.value with
    | None -> ()
    | Some v -> Printf.printf "%s  DATA = %a\n" indent p_data v) ;
    List.iter (fun child ->
      let indent = indent ^ "  " in
      dump p_key p_data ~indent child
    ) t.children ;
    Printf.printf "%s}\n" indent

  let make ?value subkey =
    { subkey ; value ; children = [] }

  let subkey_of_key key =
    { data = key ; offset = 0 ; length = Key.length key }

  (* Add this new binding (or replace previous one is that key was already
   * bound), maybe splitting subkeys already stored: *)
  let add key value t =
    if debug then Printf.printf "\n-- add (key len=%d) --\n" (Key.length key) ;
    (* Add the subkey (with value) into t and return the new t: *)
    let rec add_subkey subkey t =
      if debug then Printf.printf "  -- add_subkey offs=%d len=%d --\n" subkey.offset subkey.length ;
      if subkey.length = 0 then (
        (* Then we are where the value is supposed to be added: *)
        { t with value = Some value }
      ) else
        (* Iterate over the children, and either modify one or add one, returning
         * the new list of children (in two chunks: the inverted head and the
         * tail): *)
        let rec loop_children prevs = function
          | [] ->
              (* Couldn't find a suitable children, add at the end: *)
              (make ~value subkey :: prevs), []
          | (child :: children') as children->
              if debug then Printf.printf "    test child (subkey: offs=%d/%d len=%d)\n" child.subkey.offset (Key.length child.subkey.data) child.subkey.length ;
              let l = Key.common_prefix subkey child.subkey in
              if debug then Printf.printf "      common prefix length = %d\n" l ;
              (* Or Key.compare_slice is wrong: *)
              assert (l <= subkey.length) ;
              assert (l <= child.subkey.length) ;
              if l = child.subkey.length then
                (* Continue child: *)
                let subkey' = slice_consume subkey l in
                if debug then Printf.printf "      continue child...\n" ;
                (add_subkey subkey' child :: prevs), children'
              else if l = 0 then
                let c = Key.compare_slice subkey child.subkey in
                if debug then Printf.printf "      compare_subkey = %d\n" c ;
                assert (c <> 0) ; (* Or l = child.key.length *)
                if c < 0 then
                  (* Insert a new child right here: *)
                  (make ~value subkey :: prevs), children
                else
                  (* Insert it later: *)
                  loop_children (child :: prevs) children'
              else
                (* Split that child: *)
                let suffix_child =
                  let subkey = slice_consume child.subkey l in
                  { child with subkey } in
                let child =
                  { subkey = { child.subkey with length = l } ;
                    value = None ;
                    children = [ suffix_child ] } in
                if debug then Printf.printf "      split this child!\n" ;
                let subkey = slice_consume subkey l in
                (add_subkey subkey child :: prevs), children' in
        let daeh, tail = loop_children [] t.children in
        let children = List.rev_append daeh tail in
        { t with children }
    in
    add_subkey (subkey_of_key key) t

  (* Remove this binding, maybe compacting subkeys: *)
  let remove key t =
    if debug then Printf.printf "\n-- remove (key len=%d) --\n" (Key.length key) ;
    (* Returns the new tree without the given binding. Caller must filter
     * out the result if it's empty and absorb the key if it's a singleton. *)
    let rec lookup_prefix t subkey =
      if debug then Printf.printf "  -- lookup_prefix offs=%d len=%d --\n" subkey.offset subkey.length ;
      if subkey.length = 0 then
        match t.value with
        | Some _ ->
            { t with value = None }
        | None ->
            raise Not_found
      else
        (* Return the new tree *)
        let rec loop_children prevs = function
          | [] ->
              raise Not_found
          | child :: children' ->
              if debug then Printf.printf "    test child (subkey: offs=%d/%d len=%d)\n" child.subkey.offset (Key.length child.subkey.data) child.subkey.length ;
              let l = Key.common_prefix subkey child.subkey in
              if debug then Printf.printf "      common prefix length = %d\n" l ;
              if l = child.subkey.length then
                let subkey' = slice_consume subkey l in
                if debug then Printf.printf "      continue child...\n" ;
                (* [sub] is the continuation of child: *)
                let sub = lookup_prefix child subkey' in
                if debug then Printf.printf "      returned a new child with %sdata and %d children\n"
                  (if sub.value = None then "no " else "")
                  (List.length sub.children) ;
                let prevs =
                  if sub.value = None then
                    match sub.children with
                    | [] ->
                        (* No more need for this child *)
                        prevs
                    | [ singleton ] ->
                        (* Merge sub key with that of its only child: *)
                        if debug then Printf.printf "      merging the subkeys...\n" ;
                        let subkey = Key.cat_slices sub.subkey singleton.subkey in
                        { singleton with subkey } :: prevs
                    | _ ->
                        prevs
                  else
                    sub :: prevs in
                  { t with children = List.rev_append prevs children' }
              else if l > 0 then
                raise Not_found
              else
                let c = Key.compare_slice subkey child.subkey in
                if c <= 0 then
                  raise Not_found
                else
                  loop_children (child :: prevs) children' in
        loop_children [] t.children
    in
    lookup_prefix t (subkey_of_key key)

  let lookup t key =
    let rec lookup_prefix t subkey =
      if subkey.length = 0 then
        match t.value with
        | Some v -> v
        | None -> raise Not_found
      else
        let rec loop_children = function
          | [] ->
              raise Not_found
          | child :: children' ->
              let l = Key.common_prefix subkey child.subkey in
              if l = child.subkey.length then
                let subkey' = slice_consume subkey l in
                lookup_prefix child subkey'
              else if l > 0 then
                raise Not_found
              else
                let c = Key.compare_slice subkey child.subkey in
                if c <= 0 then
                  raise Not_found
                else
                  loop_children children' in
        loop_children t.children
    in
    lookup_prefix t (subkey_of_key key)

  let rec length t =
    List.fold_left (fun l child ->
      length child + l
    ) (if t.value = None then 0 else 1) t.children

  (* Fold over values starting with given prefix in key order: *)
  let fold t ?(prefix=Key.empty) f u =
    (* Fold over t and all its children (once the prefix have been reached): *)
    let rec do_fold k u t =
      let k = Key.cat_slices k t.subkey in
      let u =
        match t.value with
        | None -> u
        | Some v ->
            let s = Key.of_slice k in
            f s v u in
      List.fold_left (do_fold k) u t.children
    in
    (* Locate the child that continue the subkey, and fold over it.
     * [k] is the prefix so far, up to the beginning of the children. *)
    let rec loop_children k subkey = function
      | [] ->
          u
      | child :: children' ->
          assert (subkey.length > 0) ;
          assert (child.subkey.length > 0) ;
          let next_child () =
            let c = Key.compare_slice subkey child.subkey in
            if c <= 0 then u
            else loop_children k subkey children'
          in
          let l = Key.common_prefix subkey child.subkey in
          if l = 0 then next_child () else (* No such key *)
          if subkey.length > child.subkey.length then
            (* In that case we must match the prefix in full: *)
            if l = child.subkey.length then
              (* Keep looking down the tree: *)
              let k' =
                (* Append what's been matched: the first l chars: *)
                let s = sub_slice subkey 0 l in
                Key.cat_slices k s in
              let subkey' = slice_consume subkey l in
              loop_children k' subkey' child.children
            else
              next_child () (* no need to continue *)
          else
            (* This child starts with the prefix: *)
            do_fold k u child
    in
    let subkey = subkey_of_key prefix in
    let k_empty = make_empty_slice prefix in
    if subkey.length = 0 then do_fold k_empty u t else
    let l = Key.common_prefix subkey t.subkey in
    if l = t.subkey.length then
      let subkey' = slice_consume subkey l in
      if subkey'.length = 0 then do_fold k_empty u t else
      loop_children t.subkey subkey' t.children
    else
      if l = 0 then u else
      do_fold k_empty u t

end
