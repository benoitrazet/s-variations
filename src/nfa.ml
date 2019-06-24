open Set;;

type state = int;;

type fanout =  (state * (char * state) list)
and transitions = fanout list;;

type nfa =
  { initial : state ;
    transitions : transitions ;
    acceptings : int list
  }
;;

let trans_char nfa letter states =
  let candidate_trans =
    List.flatten (List.map snd (List.filter (fun (q,tr) -> Set.mem q states) nfa.transitions)) in
  let outputstates =
    List.map snd (List.filter (fun (b,q') -> b = letter) candidate_trans) in
  Set.normalize outputstates
;;

let accept nfa s =
  Set.exists (fun q -> List.mem q nfa.acceptings) s
;;

let string_of_nfa nfa =
  string_of_int nfa.initial ^ ": init\n" ^
    (List.fold_left (fun acc (i, l) ->
         acc ^ string_of_int i ^ "-> [" ^
           (List.fold_left (fun acc (c,j) -> acc ^ String.make 1 c ^ string_of_int j ^ " ") "" l) ^
             "]" ^ "\n") "" nfa.transitions) ^
      List.fold_left (fun acc q -> acc ^ string_of_int q ^ " ") "" nfa.acceptings ^
        ": accepting \n"
;;

let print_nfa nfa =
  print_string (string_of_nfa nfa); flush_all ()
;;

let nb_of_states nfa =
  List.length nfa.transitions
;;

let set_of_states nfa =
  let rec help l acc =
    match l with
    | [] -> acc
    | (q,tr) :: rest ->
       let acc' = Set.add_list (List.map snd tr) (Set.add q acc) in
       help rest acc' in
  Set.add_list nfa.acceptings (help nfa.transitions (Set.add nfa.initial Set.empty_set))
;;

let nb_of_states nfa =
  Set.size (set_of_states nfa)
;;

module SubsetS = struct

  type t = (Set.elm * bool) list;;

  let rec set_to_list set =
    match Set.next set with
    | None -> []
    | Some (e, n) -> e :: set_to_list n
  ;;

  let is_empty s = List.for_all (fun (q,b) -> b = false) s
  ;;

  let filt s = List.map fst (List.filter (fun (x,b) -> b) s)
  ;;

  let print_subsets s =
    print_string "{";
    (List.iter (fun (q,b) ->
         if b then (print_int q; print_string ",") else ()) s
    );
    print_string "}"
  ;;

  let rec next_subset s =
    match s with
    | [] -> None
    | (q,b) :: xs ->
       match next_subset xs with
       | None -> if b then None
                 else Some ((q,true) :: List.map (fun (x,_) -> (x,false)) xs)
       | Some s' -> Some ((q,b) :: s')
  ;;

  let next (s,s') startq2 =
    match next_subset s' with
    | None -> (match next_subset s with
               | None -> None
               | Some s1 -> Some (s1, startq2) )
    | Some s2 -> Some (s,s2)
  ;;

  let start_subset nfa =
    List.map (fun x -> (x, false)) (set_to_list (set_of_states nfa))

  let initial_subset nfa =
    List.map (fun x-> (x, x = nfa.initial)) (set_to_list (set_of_states nfa))

  let iterator_pair_subset nfa1 nfa2 =
    let startq1 = start_subset nfa1 in
    let startq2 = start_subset nfa2 in
    let start_pair = (startq1, startq2) in
    let next_pair (s,s') = next (s,s') startq2 in
    (start_pair, next_pair)

  let same_acceptance nfa1 nfa2 (s1,s2) =
    (*let () = print_string "S"; print_set s1; print_set s2; print_newline() in*)
    List.exists (fun (q,b) -> b && List.mem q nfa1.acceptings) s1 =
    List.exists (fun (q,b) -> b && List.mem q nfa2.acceptings) s2

  let yield1step nfa1 nfa2 (s1,s2) (s1',s2') a =
    let s3 = trans_char nfa1 a (Set.normalize (filt s1)) in
    let s4 = trans_char nfa2 a (Set.normalize (filt s2)) in
    Set.equal_set s3 (Set.normalize (filt s1')) &&
      Set.equal_set s4 (Set.normalize (filt s2'))

  let yield_in_one_step nfa1 nfa2 (s1,s2) (s1',s2') =
    yield1step nfa1 nfa2 (s1,s2) (s1',s2') 'a' ||
      yield1step nfa1 nfa2 (s1,s2) (s1',s2') 'b'

end;;

let generate_nfa n =
  let rec gen_int i =
    if i >= n then []
    else i :: gen_int (i+1) in
  let random_fanout () =
    let i = Random.int 15 in
    if i < 5
    then 0
    else if i < 10
    then int_of_float (float_of_int 1 /. 3. *. float_of_int n)
    else if i < 12
    then int_of_float (float_of_int 2 /. 3. *. float_of_int n)
    else if i < 13
    then int_of_float (float_of_int 3 /. 3. *. float_of_int n)
    else int_of_float (float_of_int 4 /. 3. *. float_of_int n) in
  let rec gen_trans fanout c =
    if fanout = 0
    then []
    else (c, Random.int n) :: gen_trans (fanout-1) c in
  let rec gen to_explore trans =
    match to_explore with
    | [] -> trans
    | q :: qs ->
      let fanout1 = random_fanout () in
      let fanout2 = random_fanout () in
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
  { initial = 0 ;
    transitions = gen (List.rev states) [] ;
    acceptings = accepting 0
  }
  (*(*all states, non-accepting*)
    { initial =0; transitions = gen (List.rev states) []; acceptings = []} *)
