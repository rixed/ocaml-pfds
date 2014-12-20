let finally handler f x =
    let r = (try f x
             with e -> handler () ; raise e) in
    handler () ;
    r

let with_dispose dispose f x =
    finally (fun () -> dispose x) f x

let get_option = function
    | Some x -> x
    | None -> invalid_arg "get_option"
