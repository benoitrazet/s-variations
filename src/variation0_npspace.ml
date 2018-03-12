open Utils;;
open Set;;
open Nfa;;


(* Decide the equivalence of two automata using an algorithm that
   non-deterministic and polynomial space. In this implementation, the
   non-deterministic component ends up being executed deterministically,
   therefore it belongs more to TISP(2^{2^(n)}, idem) then NTISP(2^n,n). *)
let npspace_eq aut1 aut2 =
  let (i1, delta1, f1) = aut1 in
  let (i2, delta2, f2) = aut2 in
  let n1 = nb_of_states aut1 in
  let n2 = nb_of_states aut2 in
  let max_steps = pow2 (n1 + n2) in
  let () = print_string "length = "; print_int max_steps; print_newline () in
  let rec loop i s1 s2 =
    if i > max_steps
    then true
    else
      if accept aut1 s1 = accept aut2 s2
      then
	let branch c =
	  let s1' = trans_char aut1 c s1 in
	  let s2' = trans_char aut2 c s2 in
	  loop (i+1) s1' s2' in
        (* non-deterministic branch, forall *)
	branch 'a' && branch 'b'
      else false
  in
  loop 0 (singleton_set i1) (singleton_set i2)
;;

