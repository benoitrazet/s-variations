type elm = int;;
type set;;

val add : elm -> set -> set;;
val add_list : elm list -> set -> set;;
val union : set -> set -> set;;
val empty_set: set;;
val is_empty : set -> bool
val singleton : elm -> set
val normalize : int list -> set
val intersection : set -> set -> set
val difference : set -> set -> set
val inclusion : set -> set -> bool
val equal_set : set -> set -> bool
val next : set -> (elm * set) option
val mem : elm -> set -> bool
val exists : (elm -> bool) -> set -> bool
val fold_left : ('a -> elm -> 'a) -> 'a -> set -> 'a
val size : set -> int
val print : set -> unit
