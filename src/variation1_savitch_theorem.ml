open Utils;;
open Set;;
open Nfa;;
open SubsetS;;


let pspace_eq nfa1 nfa2 =
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

  let rec reachable_are_accepting s_s' =
    (*let () = print_string "!"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
    let next_yield () =
      match next_pair s_s' with
      | None -> true
      | Some p -> reachable_are_accepting p in
    if same_acceptance nfa1 nfa2 s_s'
    then next_yield ()
    else
      (*let () = print_string "?"; print_set (fst s_s'); print_set (snd s_s'); print_newline() in*)
      match canyield (initstate1, initstate2) s_s' 0 bound with
      | `Reachable -> false
      | `NotReachable -> next_yield ()
  in

  if same_acceptance nfa1 nfa2 (initstate1, initstate2)
  then reachable_are_accepting start_pair
  else false
;;


open Stream;;
(*open Engine;;*)

module Var1Enum = struct

type cfg = (Nfa.SubsetS.t * Nfa.SubsetS.t);;
let eq cfg1 cfg2 = cfg1 = cfg2;;

type machine = (nfa * nfa);;
let one_step (nfa1, nfa2) cfg_prev cfg_post =
  yield_in_one_step nfa1 nfa2 cfg_prev cfg_post


type itv = (int * int);;
let is_unit (m, n) = n - m = 1;;
let is_zero (m, n) = n - m = 0;;
let split_itv (m, n) =
  let n2 = (n - m) / 2 in
  let _ =
    Logger.debug (fun () ->
        string_of_int m ^ "," ^ string_of_int (m + n2) ^ "," ^
        string_of_int (m+n2) ^ "," ^ string_of_int n) in
  ((m, m + n2), (m + n2, n))
;;

type enum = (cfg -> (cfg option)) * (cfg option) ;;

let create_mid_cfg_enum m cfg_prev cfg_post itv_prev itv_post =
  let (nfa1, nfa2) = m in
  if is_zero itv_prev
  then ((fun _ -> None), Some cfg_prev)
  else if is_zero itv_post
  then ((fun _ -> None), Some cfg_post)
  else
    let (start_pair, next_pair) = iterator_pair_subset nfa1 nfa2 in
    (next_pair, Some start_pair)
;;

let next_elm (next_cfg, cfg) =
  match cfg with
  | None -> None
  | Some cfg1 -> Some (cfg1, (next_cfg, next_cfg cfg1))
;;

let init_cfg m =
  let (nfa1, nfa2) = m in
  let initstate1 = initial_subset nfa1 in
  let initstate2 = initial_subset nfa2 in
  (initstate1, initstate2)
;;

let bound_itv m =
  let (nfa1, nfa2) = m in
  let n1 = nb_of_states nfa1 in
  let n2 = nb_of_states nfa2 in
  let bound = pow2 (n1 + n2) in
  (0, bound)
;;

let get_cfg_enum m =
  let (nfa1, nfa2) = m in
  let initstate1 = initial_subset nfa1 in
  let initstate2 = initial_subset nfa2 in
  let (start_pair, next_pair) = iterator_pair_subset nfa1 nfa2 in
  streamify next_pair (initstate1, initstate2)
;;

let unit_itv = let dummy = -1 in (dummy, dummy);;
let increase_itv = fun _ -> assert false;;

end;; (*Var1Enum*)

module VarEngine1 = Engine.Make(Var1Enum);;


(* Decide how to use that for the nfa_eq_pb *)
let pspace_eq2 nfa1 nfa2 =
  let cond (s1, s2) = not (same_acceptance nfa1 nfa2 (s1, s2)) in
  VarEngine1.exists cond (nfa1, nfa2)
