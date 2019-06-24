type 'a stream;;
type _ step =
  | Elm: ('a * 'a stream) -> 'a step
  | Empty: 'a step
;;

val empty : 'a stream
val add_elm : 'a -> 'a stream -> 'a step
val make_single_elm : 'a -> 'a step
val get_step : 'a stream -> 'a step
val streamify : ('a -> 'a option) -> 'a -> 'a stream
val filter : ('a -> bool) -> 'a stream -> 'a stream 
val app_stream : ('a step -> 'b step) -> 'a stream -> 'b stream
val cart_dep_enum :
  ('a -> 'b -> 'c) ->
  ('d -> ('e * 'a) step) ->
  'd ->
  ('c -> ('f * 'g) step) ->
  'b ->
  (('e * 'f) * 'g) step
