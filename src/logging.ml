type logLevel =
  | Info
  | Warning
  | Debug
  | Error
;;

let _loglvl = ref Warning;;

module Logger = struct

  let loglevel_to_string lvl =
    match lvl with
    | Info -> "INFO"
    | Warning -> "WARNING"
    | Debug -> "DEBUG"
    | Error -> "ERROR"
  ;;

  let aux s lst =
    let loglevel = ! _loglvl in
    if List.mem loglevel lst
    then print_string (loglevel_to_string loglevel ^ ": " ^ (s ()))
    else ()

  let warning s = aux s [Warning; Debug; Error]
  ;;

  let info s = aux s [Info; Warning; Debug; Error]
  ;;

  let debug s = aux s [Debug; Error]
  ;;

  let error s = aux s [Error]
  ;;

end;;
