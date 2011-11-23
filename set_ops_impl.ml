open Pfds_intf

module Make (Set : SET_BASE) =
struct
	include Set
	let merge a b = fold_left insert a b
end
