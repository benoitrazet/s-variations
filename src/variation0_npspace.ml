open Utils;;
open Set;;
open Nfa;;


(* Decide the equivalence of two automata using an algorithm that
   non-deterministic and polynomial space. In this implementation, the
   non-deterministic component ends up being executed deterministically,
   therefore it belongs more to TISP(2^{2^(n)}, idem) then NTISP(2^n,n). *)
let npspace_eq nfa1 nfa2 =
  let n1 = nb_of_states nfa1 in
  let n2 = nb_of_states nfa2 in
  let max_steps = pow2 (n1 + n2) in
  (*let () = print_string "length = "; print_int max_steps; print_newline () in*)
  let rec loop i s1 s2 =
    if i > max_steps
    then true
    else
      if accept nfa1 s1 = accept nfa2 s2
      then
	let branch c =
	  let s1' = trans_char nfa1 c s1 in
	  let s2' = trans_char nfa2 c s2 in
	  loop (i+1) s1' s2' in
        (* non-deterministic branch, forall *)
	branch 'a' && branch 'b'
      else false
  in
  loop 0 (singleton_set nfa1.initial) (singleton_set nfa2.initial)
;;

