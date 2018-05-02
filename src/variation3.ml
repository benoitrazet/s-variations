open Utils;;
open Set;;
open Nfa;;
open SubsetS;;

let enum_states_aut aut1 aut2 =
  let (i1, delta1, f1) = aut1 in
  let (i2, delta2, f2) = aut2 in

  let q1 = set_of_states aut1 in
  let q2 = set_of_states aut2 in
  let n1 = Set.size q1 in
  let n2 = Set.size q2 in

  let startq1 =  (List.map (fun x -> (x,false)) q1) in
  let startq2 =  (List.map (fun x -> (x,false)) q2) in
  
  let initstate1 = List.map (fun x-> (x, x = i1)) q1 in
  let initstate2 = List.map (fun x-> (x, x = i2)) q2 in

  let next (s,s') = next (s,s') startq2 in

  let start aut1 aut2 = (startq1, startq2) in

  let rec next_subset_partial s subset =
    let rec help s =
      match s with
      | [] -> None
      | (q,b) :: xs ->
	match help xs with
	| None ->
	  if List.mem q subset
	  then if b then None else Some ((q,true) :: List.map (fun (x,_) -> (x,false)) xs)
	  else None
	| Some s' -> Some ((q,b) :: s') in
    help s in

  let next_partial (s,s') subset1 subset2 =
    match next_subset_partial s' subset2 with
    | None -> (match next_subset_partial s subset1 with
      | None -> None
      | Some s1 -> Some (s1, startq2) )
    | Some s2 -> Some (s,s2)
  in

  (* compute the accessible states from s1, accessible in less than n steps. *)
  let accessible aut s1 n =
    let rec aux (currents,n) acc =
      if n = 0
      then
	let newacc = Set.set_union currents acc in
	newacc
      else let new1 = trans_char aut 'a' currents in
	   let new2 = trans_char aut 'b' currents in
	   let newcurrents = Set.set_union new2 new1 in
	   let newacc = Set.set_adds currents acc in
           aux (newcurrents, n-1) newacc in
    aux (Set.normalize_set s1,n) []
  in

  (* compute the accessible states from s1, accessible in exactly n steps. *)
  let accessible_exact aut s1 n =
    let rec aux (currents,n) =
      if n = 0
      then currents 
      else let new1 = trans_char aut 'a' currents in
	   let new2 = trans_char aut 'b' currents in
	   let newcurrents = Set.set_union new2 new1 in
           aux (newcurrents, n-1) in
    aux (s1,n)
  in
  
  
  (start, next_partial, accessible, accessible_exact)
;;

(* based on variation2 and doing an enumeration of the subsets using
   the accessible states at a given depth. *)
let pspace_eq_accessible nfa1 nfa2 =
  let n1 = nb_of_states nfa1 in
  let n2 = nb_of_states nfa2 in
  let bound = pow2 (n1 + n2) in
  (*let bound = 4 in*)
  (*let () = print_string "BOUND: "; print_int bound; print_newline () in*)
  
  let initstate1 = initial_subset nfa1 in
  let initstate2 = initial_subset nfa2 in

  let (start_pair, next_pair) = iterator_pair_subset nfa1 nfa2 in

  (* in this context canyield means it finds a string that has not the
     same acceptance for both nfas. *)
  let rec canyield (s1,s2) (s1',s2') n1 n2 =
    let print_call () = if n2 - n1 > 2 then (print_set s1; print_set s2; print_set s1'; print_set s2'; print_int n1; print_string " "; print_int n2; print_newline ()) else () in
    (*let () =  print_call () in*)
    if n2 - n1 <= 1
    then
      if (s1,s2) = (s1',s2')
      then `Reachable
      else 
	if yield_in_one_step nfa1 nfa2 (s1,s2) (s1',s2')
        then `Reachable
	else `NotReachable
    else (* n2 - n1 is greater than 1 *)
      let n_half = (n1 + n2) / 2 in
      (* There exists a middle conf *)
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

  let rec reachable_are_accepting s_s' bound cnt_reached =
    (*let () = print_string "!"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
    let next_yield cnt_reached =
      match next_pair s_s' with
      | None -> Some cnt_reached
      | Some p -> reachable_are_accepting p bound cnt_reached in
    
    match canyield (initstate1, initstate2) s_s' 0 bound with
    | `Reachable -> if same_acceptance nfa1 nfa2 s_s'
                    then next_yield (cnt_reached+1)
                    else None
    | `NotReachable -> next_yield cnt_reached
  in

  let rec iter_run n prev_cnt =
    (*let () = print_int n; flush_all () in*)
    if n > bound
    then true
    else
      let res_n = reachable_are_accepting start_pair n 0 in
      match res_n with
      | None -> false
      | Some cnt_reached ->
         let () = print_int cnt_reached; print_string "-" in
	 if cnt_reached = prev_cnt
         then true (* fixpoint reached *)
         else iter_run (n+1) cnt_reached
  in
  
  if same_acceptance nfa1 nfa2 (initstate1,initstate2)
  then iter_run 1 0
  else false
;;
