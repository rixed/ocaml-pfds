open Pfds_intf

module Make (Stack : STACK_BASE) =
struct
    include Stack
    exception Out_of_bound

    let rec suffixes stack =
        if is_empty stack then cons stack empty
        else cons stack (suffixes (tail stack))

    let rec append s1 s2 =
        if is_empty s1 then s2
        else cons (head s1) (append (tail s1) s2)

    let rec update stack idx v =
        if is_empty stack then raise Out_of_bound
        else if idx = 0 then cons v (tail stack)
        else cons (head stack) (update (tail stack) (idx-1) v)
end

