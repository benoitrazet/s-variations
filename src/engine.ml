open Stream

module type MachineEnum = sig
  type cfg;;
  val eq: cfg -> cfg -> bool;;

  type machine;;
  val one_step: machine -> cfg -> cfg -> bool;;

  type itv;;
  val is_unit: itv -> bool;;
  val is_zero: itv -> bool;;
  val split_itv: itv -> (itv * itv);;

  type enum;;
  val create_mid_cfg_enum: machine -> cfg -> cfg -> itv -> itv -> enum;;
  val next_elm: enum -> (cfg * enum) option;;

  val init_cfg: machine -> cfg;;
  val bound_itv: machine -> itv;;

  val get_cfg_enum: machine -> cfg stream;;

  val unit_itv: itv;;
  val increase_itv: itv -> itv;;
end;; (* MachineEnum *)

module Make(Enum: MachineEnum) = struct

  open Enum;;

  let rec reachable m cfg1 cfg2 itv =
    if is_zero itv
    then eq cfg1 cfg2
    else
      if is_unit itv then one_step m cfg1 cfg2
      else
        let itv1, itv2 = split_itv itv in
        let enum_mid = create_mid_cfg_enum m cfg1 cfg2 itv1 itv2 in
        let rec iterate enum =
	  match next_elm enum with
	  | None -> false
	  | Some (cfg_mid, new_enum) ->
	     let b =
	       match reachable m cfg1 cfg_mid itv1 with
	       | false -> false
	       | true -> reachable m cfg_mid cfg2 itv2 in
	     match b with
	     | false -> iterate new_enum
             | true -> true
        in
        iterate enum_mid
  ;;

  let exists cond m =
    let icfg = init_cfg m in
    let cfgs = get_cfg_enum m in
    let itv = bound_itv m in
    let rec loop enum =
      match get_step enum with
      | Empty -> false
      | Elm (cfg, stream1) ->
         let r = reachable m icfg cfg itv in
         if r && cond cfg
         then true
         else loop stream1 in
    loop cfgs;;

  let count_reachable_cfgs cond m itv =
    let icfg = init_cfg m in
    let cfgs = get_cfg_enum m in
    let rec loop enum cnt =
      match get_step enum with
      | Empty -> `NotFound cnt
      | Elm (cfg, stream1) ->
         let r = reachable m icfg cfg itv in
         let cnt = if r then cnt + 1 else cnt in
         if r && cond cfg
         then `Found
         else loop stream1 cnt in
    loop cfgs 0;;

  let iterative_exists cond m =
    let rec loop prev_cnt itv =
      match count_reachable_cfgs cond m itv with
      | `Found -> true
      | `NotFound cnt ->
         let _ = assert (cnt >= prev_cnt) in
         if cnt == prev_cnt
         then false (* fixpoint is reached *)
         else loop cnt (increase_itv itv) in
    loop (-1) unit_itv;;

end;; (* Engine *)
