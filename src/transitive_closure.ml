(* The module interval is for defining a semiring of intervals, where
   an interval [i,j] means "is reachable from i to j". *)
module Interval = struct

type number =
  | Val of int
  | Infinity
;;

let print_number v = match v with Val i ->  print_int i | Infinity -> print_string "*";;

type comp = Less | Equal | Greater;;
let compare_number i1 i2 =
  match i1, i2 with
  | Val n1, Val n2 -> if n1 = n2 then Equal else if n1 < n2 then Less else Greater
  | Val n1, Infinity -> Less
  | Infinity, Val n2 -> Greater
  | Infinity, Infinity -> Equal
;;


let max n1 n2 =
  match compare_number n1 n2 with
  | Less  -> n2
  | Equal   -> n1 (* or n2 *)
  | Greater -> n1
;;

let min n1 n2 =
  match compare_number n1 n2 with
  | Less -> n1
  | Equal -> n1 (* or n2 *)
  | Greater -> n2
;;

let add n1 n2 =
  match n1 , n2 with
  | Infinity, _ | _, Infinity ->Infinity
  | Val i, Val j -> Val (i+j)
;;

type interval =
 | Between of (number * number)
;;

let upper_bound itv =
  match itv with
  | Between (_, Val i) -> i
  | _ -> assert false
;;

let interval_d1 = Between(Val 1, Val 1);;
let interval_all = Between(Val 0, Infinity);;

let interval_zero = Between (Infinity,Val 0);;
let interval_one = Between (Val 0,Val 0);;

let interval_union t1 t2 =
  match t1, t2 with
  | Between (Infinity,i2), Between (j1,j2) -> t2
  | Between (i1,i2), Between (Infinity,j2) -> t1
  | Between (i1,i2), Between (j1,j2) -> Between (min i1 j1, max i2 j2)
;;

let interval_conc t1 t2 =
  match t1,t2 with
  | Between (i1,i2), Between(j1,j2) -> Between (add i1 j1, add i2 j2) 
;;

let interval_star t =
  match t with
  | Between (Infinity,_) -> interval_one
  | Between (Val 0,Val 0) -> interval_one
  | Between (i1,i2) -> Between (Val 0,Infinity)
;;

let interval_intersection t1 t2 =
  match t1,t2 with
  | Between (Infinity,_), _ -> interval_zero
  | _, Between (Infinity,_) -> interval_zero
  | Between(i1,i2), Between (j1,j2) ->
    let a = max i1 j1 in
    let b = min i2 j2 in
    (match b with
    | Infinity -> Between(a,b)
    | _ -> if a > b
           then interval_zero
      else Between (a,b)
    )
;;

let is_empty t =
  match t with
  | Between(Infinity,_) -> true
  | _ -> false
;;

let is_starting_zero t =
  match t with
  | Between(Val 0, _) -> true
  | _ -> false
;;

let interval_compare t1 t2 =
  match t1,t2 with
  | Between (Infinity,_), _ | _, Between (Infinity,_) -> assert false
  | Between (i1,i2), Between (j1,j2) ->
    match compare_number i1 j1 with
    | Less -> Less
    | Equal -> compare_number i2 j2
    | Greater -> Greater
;;

let print_interval v =
  if is_empty v then print_string " --- "
  else
    match v with
    | Between (v1,v2) -> print_string "["; print_number v1; print_string ";"; print_number v2; print_string "]"
;;

let between (m, n) = Between (Val m, Val n);;

let to_pair t =
  match t with
  | Between (Infinity,_) -> assert false
  | Between (_,Infinity) -> assert false
  | Between(Val i1, Val i2) -> (i1,i2)
;;

end;; (* Interval *)


module Closure
  (Semi: sig
    type semiring;;
    val zero: semiring;;
    val one: semiring;;
    val sum: semiring -> semiring -> semiring;;
    val prod: semiring -> semiring -> semiring;;
    val star: semiring -> semiring;;
   end
  ) = struct

open Semi;;

type 'a graph = int * (int -> int -> 'a);;

(* val closure: 'a graph -> ('a semiring) graph *)
let closure_i g i =
  fun x -> fun y -> sum (g x y) (prod (g x i) (prod (star (g i i)) (g i y)))
;;

let closure (size,g) =
  let rec iter i g =
    if i = size then g
    else iter (i+1) (closure_i g i) in
  iter 0 g
;;

let iter_mat size f =
  let rec iter_lin x =
    let rec iter_col y =
      if y >= size
      then ()
      else (f x y; iter_col (y+1))
    in
    if x >= size
    then ()
    else (iter_col 0; iter_lin (x+1)) in
  iter_lin 0
;;

let iter_mat2 size f g h =
  let rec iter_lin x =
    let rec iter_col y =
      if y >= size
      then ()
      else (f x y; iter_col (y+1))
    in
    if x >= size
    then ()
    else (g (); iter_col 0; iter_lin (x+1)) in
  iter_lin 0
;;

    
let closure_tab_i size pre_tab post_tab i =
  let rec iter_lin x =
    let rec iter_col y =
      if y >= size
      then ()
      else let tab_x_y = pre_tab.(x).(y) in
	   let tab_x_i = pre_tab.(x).(i) in
	   let tab_i_i = pre_tab.(i).(i) in
	   let tab_i_y = pre_tab.(i).(y) in
	   let () = post_tab.(x).(y) <- sum tab_x_y (prod tab_x_i (prod (star (tab_i_i)) tab_i_y)) in
	   iter_col (y+1)
    in
    if x >= size
    then ()
    else let () = iter_col 0 in
	 iter_lin (x+1) in
  iter_lin 0
;;

let print_matrix mat =
  let size = Array.length mat in
  (iter_mat2 size (fun x y -> Interval.print_interval (Obj.magic (mat.(x).(y))))
    print_newline print_newline;
  print_newline ())
;;

let closure_tab (size,g) =
  let tab1 = Array.make_matrix size size zero in
  let tab2 = Array.make_matrix size size zero in
  let rec iter i pre_tab post_tab =
    (*let () = print_int i;flush_all () in
      let () = print_matrix mat in*)
    if i >= size then pre_tab
    else let () = closure_tab_i size pre_tab post_tab i in
         iter (i+1) post_tab pre_tab in

  let f x y = tab1.(x).(y) <- g.(x).(y) in
  let () = iter_mat size f in
  iter 0 tab1 tab2
;;


(* For the calculation of the closure, use 2 arrays, and swap them. It may even be possible to use a functional data-structure for this problem. *)
  
(* for all i in domain 'a graph *)

  end;;(* Closure *)


module IntervalIsSemi = struct
  type semiring = Interval.interval;;
  let zero = Interval.interval_zero;;
  let one = Interval.interval_one;;
  let sum = Interval.interval_union;;
  let prod = Interval.interval_conc;;
  let star = Interval.interval_star;;
end;; (*IntervalIsSemi*)

module GraphInterval = Closure(IntervalIsSemi);;
