let rec pow2 n =
  if n = 0 then 1 else 2 * pow2 (n-1)
;;
