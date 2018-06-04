open Nfa;;
open Variation0_npspace;;
open Variation1_savitch_theorem;;
open Variation2_counting;;
open Variation3;;

let test_equiv_nfa algo n nb_tests=
  let rec aux i n size =
    if i >= n then ()
    else
	let nfa1 = generate_nfa size in
	let nfa2 = generate_nfa size in
	(*let () = print_nfa nfa1 in
	let () = print_nfa nfa2 in*)
        match (algo nfa1 nfa2) with
        | false -> print_string "."; flush_all ();
                   aux (i+1) n size
        | true  -> print_string "EQ"; flush_all ();
                   aux (i+1) n size
  in
  let rec iterate i =
    if i >= n
    then ()
    else
      let () = print_string "\nSize "; print_int i; print_newline () in
      let () = aux 1 nb_tests i in
      iterate (i+1)
  in
  iterate 2
;;

test_equiv_nfa npspace_eq 3 10;;
test_equiv_nfa pspace_eq 3 100;;
test_equiv_nfa pspace_eq_counting 4 1000;;
test_equiv_nfa pspace_eq_accessible 5 100;;
