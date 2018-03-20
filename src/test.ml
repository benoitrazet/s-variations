open Nfa;;
open Variation0_npspace;;
open Variation1_savitch_theorem;;

let test_equiv_nfa n =
  let rec aux i n size =
    if i >= n then ()
    else
	let nfa1 = generate_nfa size in
	let nfa2 = generate_nfa size in
	let () = print_nfa nfa1 in
	let () = print_nfa nfa2 in
	(*match (npspace_eq nfa1 nfa2) with*)
        match (pspace_eq nfa1 nfa2) with
        | false -> print_string "NEQ\n\n";
                   aux (i+1) n size
        | true  -> print_string "EQ\n\n";
                   aux (i+1) n size
  in
  let nb_iteration_per_size = 100 in
  let rec iterate i =
    if i >= n
    then ()
    else let () = aux 1 nb_iteration_per_size i in
	 iterate (i+1)
  in
  iterate 2
;;

test_equiv_nfa 4;;
