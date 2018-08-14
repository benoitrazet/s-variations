open Utils;;
open Set;;
open Nfa;;
open SubsetS;;
open Transitive_closure;;


(* The function sat_enum_subset enumerate subsets of
   allstates not naively, such that each subset is able to generate all the states
   in [list.map fst tab]. The [tab] parameter is the association table
   such that if [(q,l)] is in [tab] then it means that every state in
   [l] can reach state [q].

   The enumeration works by backtracking. It is worth noticing that
   there are similarities with this problem and #SAT which aims at
   counting the number of satisfiability assignments. I leave as
   future work to further investigate the connection between the 2
   problems.
*)

(* allstates is a constant parameter *)
let rec smart_enum_subset (tab:(int * (int * Interval.interval) list * Interval.interval) list) global_itv (allstates: int list) =
  sat_enum_subset tab global_itv [] [] allstates []

and sat_enum_subset (tab:(int * (int * Interval.interval) list * Interval.interval) list)
    global_itv selected notselected (allstates: int list) choices =
  if Interval.is_empty global_itv
  then backtrack choices
  else
  match tab with
  | [] ->  finalize global_itv selected notselected allstates choices
  | _ ->
    let tab1 = List.map (fun (q,lst,itv) -> (q,lst,itv,List.length lst)) tab in
    let orderedtab = List.sort
      (fun (q,l,itv,score) (q',l',itv',score') -> score - score') tab1 in
    let orderedtab = List.map (fun (x,y,z,t) -> (x,y,z)) orderedtab in
    let (q,lst,_itv(*,score*)) = List.hd orderedtab in
    let assoc_itv_in_trans q1 tr =
      let rec aux accu tr =
	match tr with
	| [] -> None
	| (q,itv) :: rest -> if q1 = q then Some (itv,List.rev_append accu rest) else aux ((q,itv)::accu) rest in
      aux [] tr in

    let clean_tab_and_update_global_itv tr =
      List.fold_left (fun (tr,global_itv) (q,lst,itv_q) ->
	if lst = []
	then (tr,Interval.interval_intersection itv_q global_itv)
	else ((q,lst,itv_q) :: tr,global_itv)) ([],global_itv) tr in
    let filter_selected_tab q1 =
      let remove_q1_from_transitions_and_update =
	List.map (fun (q,lst,itv_q) -> (match assoc_itv_in_trans q1 lst with
	| None -> (q,lst,itv_q)
	| Some (itv, lst') -> (q,lst',Interval.interval_union itv itv_q)) ) orderedtab
      in
      clean_tab_and_update_global_itv remove_q1_from_transitions_and_update in
    let remove_q1_from_transitions_no_update q1 =
      List.map (fun (q,lst,itv_q) -> (q, List.filter (fun (r,_) -> not (r = q1)) lst, itv_q)) orderedtab
      in
    let filter_notselected_tab q1 =
      let tab2 = remove_q1_from_transitions_no_update q1 in
      clean_tab_and_update_global_itv tab2  in
    (match List.length ((fun (x,y,z) -> y) (List.hd orderedtab)) with
    | 0 -> let (new_tab, new_global_itv) = clean_tab_and_update_global_itv orderedtab in
	   sat_enum_subset new_tab new_global_itv selected notselected allstates choices
    | _ ->
      let (state_selected, _itv) = List.hd lst in
      let (tab_selected, global_itv_selected) = filter_selected_tab state_selected in
      let (tab_notselected, global_itv_notselected) = filter_notselected_tab state_selected in
      let newchoices = (tab_notselected, global_itv_notselected, selected, state_selected :: notselected,allstates):: choices in
       sat_enum_subset tab_selected global_itv_selected (state_selected :: selected) notselected allstates newchoices)
and backtrack choices =
  match choices with
  | [] -> `Empty
  | (tab, global_itv, selected, notselected,allstates) :: other ->
    sat_enum_subset tab global_itv selected notselected allstates other
and finalize itv selected notselected allstates choices =
  let vector_of_selected = List.map (fun q -> (q,true)) selected in
  let may_be_selected = List.filter (fun q -> not (List.mem q selected) && not (List.mem q notselected)) allstates in
  let enum_may_be_selected = Some (List.map (fun q -> (q,false)) may_be_selected) in
  (*let () = print_string "Size of Selected: "; print_int (List.length vector_of_selected);
    print_string " vs "; print_int (List.length allstates); print_newline () in*)
  finalize2 itv vector_of_selected enum_may_be_selected choices
and finalize2 itv vector_of_selected enum_may_be_selected choices =
  match enum_may_be_selected with
  | None -> backtrack choices
  | Some may_be_selected ->
    `Elm (
      (vector_of_selected@may_be_selected,itv),
      fun () ->
        finalize2 itv vector_of_selected (next_subset may_be_selected) choices)
;;

(* Enumerate the product of two enumerations as the enumeration of the
   product. Both enumerations are functions of an interval and the
   interval produced by the first enumeration is piped into the second
   enumeration. *)
let enum_pair f1 initial_itv1 f2 initial_itv2 =
  let rec help str1 =
    match str1 with
    | `Empty -> `Empty
    | `Elm ((v1,itv1),next1) ->
       let itv2 = Interval.interval_intersection itv1 initial_itv2 in
       let rec help2 str2 =
	 match str2 with
	 | `Empty -> help (next1 ())
	 | `Elm ((v2,itv),next2) -> `Elm (((v1,v2),itv), fun () -> help2 (next2 ())) in
       help2 (f2 itv2)
  in
  help (f1 initial_itv1)
;;

let eq_subset s s' =
  let s1 = Set.normalize (filt s) in
  let s1' = Set.normalize (filt s') in
  Set.equal_set s1 s1'
;;

let eq_pair_subset (s1,s2) (s1',s2') =
  eq_subset s1 s1' && eq_subset s2 s2'
;;

(* computes the union interval that a list of states s can lead to state q' *)
let get_itv_to_state mat s q' itv =
  List.fold_left (fun t q ->
      Interval.interval_union t (Interval.interval_intersection mat.(q).(q') itv))
    Interval.interval_zero s
;;
  
(* Given a matrix mat and an initial subset s and an arrival subset s'
   and an itv, the function get_interval computes the interval that
   can lead from s to s' conditioned by itv. *)
let get_interval mat s s' itv =
  let get_col_part1 q' = get_itv_to_state mat s q' itv in
  let get_all_cols_part1 = List.map get_col_part1 s' in
  let inter_all_cols = List.fold_left (fun res t -> Interval.interval_intersection t res) Interval.interval_all get_all_cols_part1 in
  let result_itv1 = Interval.interval_intersection inter_all_cols itv in
  result_itv1

(* apply get_interval to the first pair of subsets and the second pair
   of subsets. *)
let get_both_intervals mat s s_mid s' itv1 itv2 =
  (get_interval mat s s_mid itv1, get_interval mat s_mid s' itv2)
;;

(* Given a matrix mat and a list of states s, it computes the list of
   states that are accessible from s given an interval itv. *)
let accessible_between mat s itv =
  let size = Array.length mat in
  let rec aux i =
    if i >= size
    then []
    else let itv1 = get_itv_to_state mat s i itv in
	 if Interval.is_empty itv1
         then aux (i+1)
	 else i :: aux(i+1) in
  aux 0
;;

(* Computes the matrix of transitive closure of intervals for a given nfa. *)
let nfa_to_mat_closure nfa =
  let q = set_of_states nfa in
  let size = Set.fold_left (fun m q -> if q > m then q else m) 0 q in
  (*let () = print_string "Size "; print_int size; print_newline () in*)
  let mat = Array.make_matrix (size+1) (size+1) IntervalIsSemi.zero in
  let () = List.iter (fun (q,trans) ->
    List.iter (fun (c,q') ->
      mat.(q).(q') <- Interval.interval_d1) trans) nfa.transitions in
  let cmat = GraphInterval.closure_tab (Array.length mat, mat) in
  cmat
;;


(* The commented code in this function is a cleanup from pspace_eq9 *)
let enum_sat nfa1 nfa2 mat1 mat2 (s1,s2) (s1',s2') interval1 interval2 =
  let f_s1 = filt s1 in
  let f_s2 = filt s2 in
  let f_s1' = filt s1' in
  let f_s2' = filt s2' in

  let (subset1,subset2) =
    let subset1 = accessible_between mat1 f_s1 interval1 in
    let subset2 = accessible_between mat2 f_s2 interval1 in
    (subset1,subset2) in

  (* For each states in s1', associate the states in subset1 that can yield to s1' *)
  let s1'_reachable_from = List.map (fun q' -> (q',
    List.fold_left (fun tr q ->
      let itv = Interval.interval_intersection interval2 mat1.(q).(q') in
      if Interval.is_empty itv
      then tr
      else (q,itv) :: tr) [] subset1)) f_s1' in

  let s2'_reachable_from = List.map (fun q' -> (q',
    List.fold_left (fun tr q ->
      let itv = Interval.interval_intersection interval2 mat2.(q).(q') in
      if Interval.is_empty itv
      then tr
      else (q,itv) :: tr) [] subset2)) f_s2' in
  let init m = List.map (fun (q,lst) -> (q,lst,Interval.interval_zero)) m in
  let enum1 itv = smart_enum_subset (init s1'_reachable_from) itv subset1 in
  let enum2 itv = smart_enum_subset (init s2'_reachable_from) itv subset2 in

  let enum_1_2 = enum_pair enum1 interval2 enum2 interval2 in

  let rec enum_interval f =
    (match f with
    | `Empty -> `Empty
    | `Elm (((s_mid1,s_mid2),itv2), next) ->
      let f_s_mid1 = filt s_mid1 in
      let f_s_mid2 = filt s_mid2 in
      let (t1,t1') = get_both_intervals mat1 f_s1 f_s_mid1 f_s1' interval1 itv2 in
      let (t2,t2') = get_both_intervals mat2 f_s2 f_s_mid2 f_s2' interval1 itv2 in
      let t_prev = Interval.interval_intersection t1 t2 in
      let t_post = Interval.interval_intersection t1' t2' in
      let print_call () =
        print_string "EnumInt "; print_set s1; print_set s2; print_set s1'; print_set s2';
	print_set s_mid1; print_set s_mid2; Interval.print_interval itv2; print_string " ";
        Interval.print_interval interval1; Interval.print_interval interval2; print_string " ";
	Interval.print_interval t1; Interval.print_interval t2;
	Interval.print_interval t1'; Interval.print_interval t2'; Interval.print_interval t_prev; Interval.print_interval t_post
      ; print_newline () in
      (*let () =  print_call () in*)
      if Interval.is_empty t_prev
      then enum_interval (next ())
      else
	if Interval.is_starting_zero interval2 && eq_subset s_mid1 s1' && eq_subset s_mid2 s2'
	then `Elm (((s_mid1,s_mid2),t_prev, Interval.interval_union Interval.interval_one t_post), (fun () -> enum_interval (next())))
	else
	  if Interval.is_empty t_post
	  then enum_interval (next ())
	  else (*let () = print_call () in*)
	       `Elm (((s_mid1,s_mid2),t_prev,t_post), (fun () -> enum_interval (next())))
    ) in

  (* this function adds (s1',s2') in the enumeration if it does not appear in the enumeration *)
  let add_if_not_in_enum f =
    let rec aux f b =
      match f with
      | `Empty -> if not b then `Elm (((s1',s2'), Interval.interval_one), fun () -> `Empty) else `Empty
      | `Elm (((s_mid1,s_mid2),itv2), next) ->
	let b1 = eq_subset s_mid1 s1' && eq_subset s_mid2 s2' in
	`Elm (((s_mid1,s_mid2),itv2), fun () -> aux (next()) (b || b1))
    in


    if Interval.is_starting_zero interval2 then
      aux f false
    else f in

  enum_interval (add_if_not_in_enum enum_1_2)
;;


(* based on variation2 and doing an enumeration of the subsets using
   the accessible states at a given depth. *)
let pspace_eq_accessible nfa1 nfa2 =
  let initstate1 = initial_subset nfa1 in
  let initstate2 = initial_subset nfa2 in

  let mat1 = nfa_to_mat_closure nfa1 in
  let mat2 = nfa_to_mat_closure nfa2 in

  let (start_pair, next_pair) = iterator_pair_subset nfa1 nfa2 in

  (* in this context canyield means it finds a string that has not the
     same acceptance for both nfas. *)
  let rec canyield (s1,s2) (s1',s2') (m, n) =
    let print_call () = print_string "CY "; print_set s1; print_set s2; print_set s1'; print_set s2';
      print_string " ("; print_int m; print_string " "; print_int n; print_string ")"; print_newline () in
    (*let () =  print_call () in*)

    match m,n with
    | 0,0 -> if eq_pair_subset (s1,s2) (s1',s2')
             then `Reachable
             else `NotReachable
    | 0,_ -> if eq_pair_subset (s1,s2) (s1',s2')
             then `Reachable
             else canyield (s1,s2) (s1',s2') (1, n)
    | 1,1 ->
      if yield_in_one_step nfa1 nfa2 (s1,s2) (s1',s2')
      then `Reachable
      else `NotReachable
    | _ ->
      let n_half = n / 2 in

      let (interval1,interval2) =
	match compare m n_half <= 0 with
	| true ->
	  let interval1 = (m, n_half) in
	  let interval2 = (0, n - n_half) in
	  (interval1,interval2)
	| false ->
	  let interval1 = (n_half, n_half) in
	  let interval2 = (m - n_half, n - n_half) in
	  (interval1,interval2)
      in

      let () = if fst interval1 = 0 then assert false else () in

      let enum_mid =
        enum_sat nfa1 nfa2 mat1 mat2 (s1,s2) (s1',s2')
          (Interval.between interval1) (Interval.between interval2) in
      let rec iterate stream_s_s' =
	(*let () = print_string "!!\n" in*)
	match stream_s_s' with
	| `Empty -> `NotReachable
	| `Elm ((s_s',itv1,itv2),next) ->
	  let b =
	    match canyield (s1,s2) s_s' (Interval.to_pair itv1) with
	    | `NotReachable -> `NotReachable
	    | `Reachable -> canyield s_s' (s1',s2') (Interval.to_pair itv2) in
	  match b with
	  | `NotReachable -> iterate (next ())
	  | `Reachable -> `Reachable
      in
      iterate enum_mid in

  let rec reachable_are_accepting s_s' bound cnt_reached =
    (*let () = print_string "!"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
    let next_yield cnt_reached =
      match next_pair s_s' with
      | None -> Some cnt_reached
      | Some p -> reachable_are_accepting p bound cnt_reached in

    match canyield (initstate1, initstate2) s_s' (0, bound) with
    | `Reachable ->
       if same_acceptance nfa1 nfa2 s_s'
       then next_yield (cnt_reached+1)
       else None
    | `NotReachable -> next_yield cnt_reached
  in

  let rec iter_run n prev_cnt =
    (*let () = print_int n; flush_all () in*)
    let res_n = reachable_are_accepting start_pair n 0 in
    match res_n with
    | None -> false
    | Some cnt_reached ->
       let () = print_int cnt_reached; print_string "-"; flush_all () in
       if cnt_reached = prev_cnt
       then true (* fixpoint reached *)
       else iter_run (n+1) cnt_reached
  in

  if same_acceptance nfa1 nfa2 (initstate1,initstate2)
  then iter_run 1 0
  else false
;;
