type 'a unit_comput = unit -> 'a
;;

type _ step =
  | Elm: ('a * 'a stream) -> 'a step
  | Empty: 'a step

and 'a stream = ('a step) unit_comput
;;

let empty = fun () -> Empty
;;

let add_elm e s = Elm (e, s)
;;

let make_single_elm e = add_elm e empty
;;

let get_step s = s ()
;;

let rec streamify f init =
  let rec internal v =
    match v with
    | None -> Empty
    | Some e -> Elm (e, fun () -> internal (f e))
  in
  fun () -> internal (Some init)
;;

let app_stream f s = fun () -> f (get_step s)
;;

let rec filter f s =
  match get_step s with
  | Empty -> empty
  | Elm (a, s') ->
     if f a
     then fun () -> add_elm a (filter f s')
     else filter f s'
;;

(* Cartesian dependent enumeration *)
let cart_dep_enum inter f1 e1 f2 e2 =
  let rec help str1 =
    match str1 with
    | Empty -> Empty
    | Elm ((v1, itv1), next1) ->
       let itv2 = inter itv1 e2 in
       let rec help2 str2 =
          match str2 with
         | Empty -> help (get_step next1)
         | Elm ((v2, itv), next2) -> Elm (((v1, v2), itv), app_stream help2 next2) in
       help2 (f2 itv2)
  in
  help (f1 e1)
;;
