type elm = int;;

type set = elm list;;

let add elm (s: int list) =
  let rec help s =
    match s with
    | [] -> [elm]
    | x::xs -> if elm = x then s else if elm < x then elm :: s else x :: help xs
  in
  help s
;;

let rec add_list l s =
  match l with
  | [] -> s
  | x::xs -> add_list xs (add x s)
;;

let rec union s1 s2 =
  match s1,s2 with
  | [],[] -> []
  | _,[] -> s1
  | [],s2 -> s2
  | x::xs,y::ys ->
    if x = y
    then union s1 ys
    else if x < y
         then x :: union xs s2
         else y :: union s1 ys
;;

let empty_set = [];;

let is_empty s = s = [];;

let singleton elm = [elm] ;;

let normalize l =
  let rec split l =
    match l with
    | [] -> [ l ]
    | [x] -> [ l ]
    | x::y::ys -> if x = y then [x] :: split ys else if x < y then [x;y] :: split ys else [y;x] :: split ys in
  let rec merge2list ll =
    match ll with
    | [] -> []
    | [x] -> [x]
    | l1 :: l2 :: lls -> (union l1 l2 :: merge2list lls) in
  let rec merge_all ll =
    match ll with
    | [] ->  assert false
    | [ x ] -> x
    | _ -> merge_all (merge2list ll) in
  merge_all (split l)
;;

let rec intersection s1 s2 =
  match s1 with
  | [] -> []
  | x::xs -> if List.mem x s2 then x :: (intersection xs s2) else intersection xs s2
;;

let rec difference s1 s2 =
  match s1 with
  | [] -> []
  | x::xs -> if List.mem x s2 then difference xs s2 else x :: (difference xs s2)
;;

let rec inclusion s1 s2 =
  match s1 with
  | [] -> true
  | x::xs -> if List.mem x s2 then inclusion xs s2 else false
;;

let equal_set s1 s2 =
  inclusion s1 s2 && inclusion s2 s1
;;

let next s = match s with
  | [] ->  None
  | x :: xs -> Some (x,xs)
;;

let rec mem elm s =
  match s with
  | [] -> false
  | x :: xs -> if x > elm then false
               else if x = elm then true
               else mem elm xs
;;

let rec exists f s = match s with
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

let rec fold_left f z s =
  match s with
  | [] -> z
  | x :: xs -> fold_left f (f z x) xs
;;

let size s = List.length s;;

let print s = print_string "{";List.iter (fun i-> print_int i;print_string " ") s; print_string "}";;
