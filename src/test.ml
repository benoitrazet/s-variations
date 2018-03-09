open Nfa;;
open Variation0_npspace;;

let test_equiv_nfa n =
  let rec aux i n size =
    if i >= n then ()
    else
	let aut1 = generate_nfa size in
	let aut2 = generate_nfa size in
	let () = print_aut aut1 in
	let () = print_aut aut2 in
	match (npspace_eq aut1 aut2) with
        | false -> print_string "NEQ \n";
                   flush_all ();
                   (*print_input (); *)
                   aux (i+1) n size
        | true  -> print_string "EQ \n";
                   (*print_aut aut1; print_aut aut2;*)
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
