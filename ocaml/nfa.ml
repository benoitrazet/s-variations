type state = int;;

type fanout =  (state * (char option * state) list)
and transitions = fanout list;;

type nfa = ( int * transitions * int list );;

let trans_char (int, t, final) letter states =
  let candidate_trans = List.flatten (List.map snd (List.filter (fun (q,tr) -> List.mem q states) t)) in
  let outputstates = List.map snd (List.filter (fun (b,q') -> b = letter) candidate_trans) in
  Set.normalize_set outputstates
;;

let accept aut s =
  let (_,_,f) = aut in
  List.exists (fun q -> List.mem q f) s
;;
