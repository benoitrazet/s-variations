open Utils;;
open Set;;
open Nfa;;

let pspace_eq nfa1 nfa2 =
  let q1 = set_of_states nfa1 in
  let q2 = set_of_states nfa2 in
  let n1 = Set.size q1 in
  let n2 = Set.size q2 in
  let bound = pow2 (n1+ n2) in
  (*let bound = 4 in*)
  (*let () = print_string "BOUND: "; print_int bound; print_newline () in*)
  let startq1 = List.map (fun x -> (x,false)) q1 in
  let startq2 = List.map (fun x -> (x,false)) q2 in
  
  let initstate1 = List.map (fun x-> (x, x = nfa1.initial)) q1 in
  let initstate2 = List.map (fun x-> (x, x = nfa2.initial)) q2 in

  let next_pair (s,s') = next (s,s') startq2 in  
  let start_pair = (startq1, startq2) in
  
  let yield1step (s1,s2) (s1',s2') a =
    let s3 = trans_char nfa1 a (filt s1) in
    let s4 = trans_char nfa2 a (filt s2) in
    (Set.set_adds s3 [], Set.set_adds s4 []) = (filt s1', filt s2') in

  let yield_in_one_step (s1,s2) (s1',s2') =
    yield1step (s1,s2) (s1',s2') 'a' || yield1step (s1,s2) (s1',s2') 'b' in

  let same_acceptance (s1,s2) =
    (*let () = print_string "S"; print_set s1; print_set s2; print_newline() in*)
    List.exists (fun (q,b) -> b && List.mem q nfa1.acceptings) s1 = List.exists (fun (q,b) -> b && List.mem q nfa2.acceptings) s2
     in

  (* in this context canyield means it finds a string that has not the
     same recognition for both nfas. *)
  let rec canyield (s1,s2) (s1',s2') n1 n2 =
    let print_call () = if n2 - n1 > 2 then (print_set s1; print_set s2; print_set s1'; print_set s2'; print_int n1; print_string " "; print_int n2; print_newline ()) else () in
    (*let () =  print_call () in*)
    if n2 - n1 <= 1
    then
      if (s1,s2) = (s1',s2') then `Reachable
      else 
	if yield_in_one_step (s1,s2) (s1',s2')
        then `Reachable
	else `NotReachable
    else (* n2 - n1 is greater than 1 *)
      let n_half = (n1 + n2) / 2 in
      let rec iterate s_s' =
	let b = 
	  match canyield (s1,s2) s_s' n1 n_half with
	  | `NotReachable -> `NotReachable
	  | `Reachable -> canyield s_s' (s1',s2') n_half n2
	  in
	match b with
	| `NotReachable ->
	  (match next_pair s_s' with
	  | None -> `NotReachable
	  | Some new_s_s' -> iterate new_s_s')
	| `Reachable -> `Reachable
      in
      iterate start_pair in

  let rec run_can_yield s_s' =
    (*let () = print_string "!"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
    let next_yield () =
      match next_pair s_s' with
      | None -> true
      | Some p -> run_can_yield p in
    if same_acceptance s_s'
    then next_yield ()
    else
      (*let () = print_string "?"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
      match canyield (initstate1, initstate2) s_s' 0 bound with
      | `Reachable -> false
      | `NotReachable -> next_yield ()
  in

  if same_acceptance (initstate1, initstate2)
  then run_can_yield start_pair
  else false
;;
