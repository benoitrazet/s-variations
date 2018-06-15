open Nfa;;

(* This is from npspace_eq3. It is in TISP(2^n,2^n) *)
(* Remark that I don't even need the bound anymore. Maybe the bound is hidden in the memory *)
let npspace_eq_memo nfa1 nfa2 =

  (* the memory has 2 functions, to check membership and to add: *)
  let in_memory (s1,s2) mem = List.mem (s1,s2) mem in
  let add_to_memory (s1,s2) mem = (s1,s2) :: mem in
  let empty_memory = [] in
  let rec loop s1 s2 mem =
    if in_memory (s1,s2) mem
    then Some mem
    else
      if accept nfa1 s1 = accept nfa2 s2
      then
	let aux a mem =
	  let s1' = trans_char nfa1 a s1 in
	  let s2' = trans_char nfa2 a s2 in
	  loop s1' s2' (add_to_memory (s1,s2) mem) in
	(* non-deterministic guess, forall quantifier *)
	(match aux 'a' mem with
	 | Some mem1 -> aux 'b' mem1
	 | None -> None)
      else None
  in
  match loop (Set.singleton nfa1.initial)
          (Set.singleton nfa2.initial) empty_memory with
  | Some l -> true
  | None -> false
;;
