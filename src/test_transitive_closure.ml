open Transitive_closure;;

let example1 =
  let tab = Array.make_matrix 5 5 IntervalIsSemi.zero in
  let inter0 = Interval.Between (Interval.Val 0,Interval.Val 0) in
  let inter1 = Interval.interval_d1 in
  let () =
    tab.(0).(0) <- inter0
    ; tab.(1).(1) <- inter0
    ; tab.(2).(2) <- inter0
    ; tab.(3).(3) <- inter0
    ; tab.(4).(4) <- inter0

  in
  tab
;;

let res1 = GraphInterval.closure_tab (5,example1);;
GraphInterval.print_matrix example1;;
GraphInterval.print_matrix res1;
print_newline();;

let example2 =
  let tab = Array.make_matrix 5 5 IntervalIsSemi.zero in
  let inter0 = Interval.Between (Interval.Val 0,Interval.Val 0) in
  let inter1 = Interval.interval_d1 in
  let () =
    tab.(0).(1) <- inter1
    ; tab.(0).(2) <- inter1
    ; tab.(1).(2) <- inter1
    ; tab.(1).(3) <- inter1
    ; tab.(3).(4) <- inter1
    ; tab.(4).(3) <- inter1
  in
  tab
;;

let res2 = GraphInterval.closure_tab (5,example2);;
GraphInterval.print_matrix example2;;
GraphInterval.print_matrix res2;
print_newline();;

