open Utils;;
open Set;;
open Nfa;;
open SubsetS;;

let pspace_eq_counting nfa1 nfa2 =
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
    let print_call () =
      if n2 - n1 > 2
      then (print_subsets s1; print_subsets s2;
            print_subsets s1'; print_subsets s2';
            print_int n1; print_string " ";
            print_int n2; print_newline ())
      else () in
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
    (*let () = print_string "!"; print_subsets (fst s_s'); print_subsets (snd s_s'); print_newline() in*)
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
