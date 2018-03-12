open Set;;

type state = int;;

type fanout =  (state * (char option * state) list)
and transitions = fanout list;;

type nfa = ( int * transitions * int list );;

let trans_char (int, t, final) letter states =
  let candidate_trans =
    List.flatten (List.map snd (List.filter (fun (q,tr) -> List.mem q states) t)) in
  let outputstates =
    List.map snd (List.filter (fun (b,q') -> b = letter) candidate_trans) in
  Set.normalize_set outputstates
;;

let accept aut s =
  let (_,_,f) = aut in
  List.exists (fun q -> List.mem q f) s
;;

let nb_of_states nfa =
  let (_, t, _) = nfa in
  List.length t
;;

let print_aut (initial, transitions, acceptings) =
  print_int initial; print_string ":init\n";
  List.iter (fun (i, l) -> print_int i; print_string "-> [";
    List.iter (fun (c,j) -> print_char c; print_int j; print_string " ") l;
    print_string "]"; print_newline ()) transitions;
  List.iter (fun q -> print_int q; print_string " ") acceptings;
  print_newline ()
;;


let generate_nfa n =
  let rec gen_int i =
    if i >= n then []
    else i :: gen_int (i+1) in
  let random_one_25 () =
    let i = Random.int 15 in
    if i < 5
    then 0
    else if i < 10
    then 1
    else if i < 12
    then 2
    else if i < 13
    then 3
    else 4 in
  let rec gen_trans fanout c =
    if fanout = 0
    then []
    else (c, Random.int n) :: gen_trans (fanout-1) c in
  let rec gen to_explore trans =
    match to_explore with
    | [] -> trans
    | q :: qs ->
      let fanout1 = random_one_25 () in
      let fanout2 = random_one_25 () in
      let trans_a = gen_trans fanout1 'a' in
      let trans_b = gen_trans fanout2 'b' in
      let transab = trans_a@trans_b in
      (*let states = List.map snd trans in
	let states' = List.filter (fun q' -> not (q = q') && not (List.mem q' already_explored)) states in*)
      gen qs ((q,transab) :: trans) in
  let rec accepting q =
    if q < n
    then if Random.int 3 = 0 then q :: accepting (q+1) else accepting (q+1)
    else [] in
  let states = gen_int 0 in
    (0, gen (List.rev states) [], accepting 0)
  (*(*all states, non-accepting*)
    (0, gen (List.rev states) [], [])*)
	
