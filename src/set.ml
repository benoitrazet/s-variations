type set = int list;;

let set_add elm (s: int list) =
  let s' = elm::s in
  let s'' = List.sort (fun x -> fun y -> x - y) s' in
  let rec remove_dupl l = match l with
    | [] -> []
    | [x] -> [x]
    | x :: y :: l -> if x = y then remove_dupl (y::l) else x :: remove_dupl (y::l) in
  remove_dupl s''
;;

let set_add elm (s: int list) =
  let rec help s =
    match s with
    | [] -> [elm]
    | x::xs -> if elm = x then s else if elm < x then elm :: s else x :: help xs
  in
  help s
;;

let rec set_adds l s =
  match l with
  | [] -> s
  | x::xs -> set_adds xs (set_add x s)
;;
(*
let set_adds l (s: int list) =
  let ls = List.sort (fun x -> fun y -> x-y) l in
  let rec help l s =
    match l with
    | [] -> s
    | elm :: elms ->
      match s with
      | [] -> help elms [elm]
      | x::xs -> if elm = x then help elms s else if elm < x then help elms (elm :: s) else x :: help l xs
  in
  help ls s
;;
*)
let rec set_union s1 s2 =
  match s1,s2 with
  | [],[] -> []
  | _,[] -> s1
  | [],s2 -> s2
  | x::xs,y::ys ->
    if x = y
    then set_union s1 ys
    else if x < y
         then x :: set_union xs s2
         else y :: set_union s1 ys        
;;

let is_empty s = s = [];;

let singleton_set elm = [elm] ;;

(*
let normalize_set l = set_adds l [];;
*)
let normalize_set l =
  let rec split l = 
    match l with
    | [] -> [ l ]
    | [x] -> [ l ]
    | x::y::ys -> if x = y then [x] :: split ys else if x < y then [x;y] :: split ys else [y;x] :: split ys in
  let rec merge2list ll =
    match ll with
    | [] -> []
    | [x] -> [x]
    | l1 :: l2 :: lls -> (set_union l1 l2 :: merge2list lls) in
  let rec merge_all ll =
    match ll with
    | [] ->  assert false
    | [ x ] -> x
    | _ -> merge_all (merge2list ll) in
  merge_all (split l)
;;

let rec intersection_set s1 s2 =
  match s1 with
  | [] -> []
  | x::xs -> if List.mem x s2 then x :: (intersection_set xs s2) else intersection_set xs s2
;;

let rec difference_set s1 s2 =
  match s1 with
  | [] -> []
  | x::xs -> if List.mem x s2 then difference_set xs s2 else x :: (difference_set xs s2)
;;

let rec inclusion_set s1 s2 =
  match s1 with
  | [] -> true
  | x::xs -> if List.mem x s2 then inclusion_set xs s2 else false
;;

let equal_set s1 s2 =
  inclusion_set s1 s2 && inclusion_set s2 s1
;;

let next s = match s with
  | [] ->  None
  | x :: xs -> Some (x,xs)
;;

let size s = List.length s;;

let print s = print_string "{";List.iter (fun i-> print_int i;print_string " ") s; print_string "}";;

