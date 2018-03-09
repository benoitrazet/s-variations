open Utils;;
open Set;;
open Nfa;;


(* compute the equivalence of 2 automata using a non-deterministic
   polyspace algorithm. On this computer the non-deterministically is
   simulated deterministically, then the space complexity is actually
   exponential. *)
let npspace_eq aut1 aut2 =
  let (i1, delta1, f1) = aut1 in
  let (i2, delta2, f2) = aut2 in
  let n1 = List.length delta1 in
  let n2 = List.length delta2 in
  let n = pow2 (n1 + n2) in
  (*let n = 8 in*)
  let () = print_string "length = "; print_int n; print_newline () in
  let rec loop i s1 s2 =
    if i > n
    then true
    else
      if accept aut1 s1 = accept aut2 s2
      then
	let branch c =
	  let s1' = trans_char aut1 c s1 in
	  let s2' = trans_char aut2 c s2 in
	  loop (i+1) s1' s2' in
        (*non-deterministic guess, forall *)
	branch 'a' && branch 'b'
      else false
  in
  loop 0 (singleton_set i1) (singleton_set i2)
;;

