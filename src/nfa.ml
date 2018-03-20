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

let string_of_nfa (initial, transitions, acceptings) =
  string_of_int initial ^ ": init\n" ^
    (List.fold_left (fun acc (i, l) ->
         acc ^ string_of_int i ^ "-> [" ^
           (List.fold_left (fun acc (c,j) -> acc ^ String.make 1 c ^ string_of_int j ^ " ") "" l) ^
             "]" ^ "\n") "" transitions) ^
      List.fold_left (fun acc q -> acc ^ string_of_int q ^ " ") "" acceptings ^
        ": accepting \n"
;;

let print_nfa aut =
  print_string (string_of_nfa aut); flush_all ()
;;

let nb_of_states nfa =
  let (_, t, _) = nfa in
  List.length t
;;

let set_of_states aut =
  let (i,t,f) = aut in
  let rec help l acc =
    match l with
    | [] -> acc
    | (q,tr) :: rest ->
      let acc' = Set.set_adds (List.map snd tr) (Set.set_add q acc) in
      help rest acc' in
  Set.set_adds f (help t (Set.set_add i []))
;;

let is_empty s = List.for_all (fun (q,b) -> b = false) s
;;

let filt s = List.map fst (List.filter (fun (x,b) -> b) s)
;;

let print_set s =
  print_string "{"; (List.iter (fun (q,b) -> if b then (print_int q; print_string ",") else ()) s); print_string "}"
;;

let rec next_subset s =
  match s with
  | [] -> None
  | (q,b) :: xs ->
    match next_subset xs with
    | None -> if b then None else Some ((q,true) :: List.map (fun (x,_) -> (x,false)) xs)
    | Some s' -> Some ((q,b) :: s')
;;

let next (s,s') startq2 =
  match next_subset s' with
  | None -> (match next_subset s with
    | None -> None
    | Some s1 -> Some (s1, startq2) )
  | Some s2 -> Some (s,s2)
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
	
