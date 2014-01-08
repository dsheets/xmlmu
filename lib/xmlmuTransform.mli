type transform = (Xmlmu.t -> unit) -> Xmlmu.t -> unit

module type INTERP = sig
  type dtd
  type xmlns
  type element
  type 'a queue
  type 'a stack
  type 't t
  type 'p patt_spec =
  | Element : element t patt_spec
  | Xmlns : xmlns t patt_spec
  | Dtd : dtd t patt_spec
  | This : 'p t -> 'p t patt_spec

  val dtd : string option -> dtd t
  val xmlns : string -> xmlns t
  val element : string -> element t
  val dtd_signal : dtd t -> Xmlmu.t t
  val data_signal : string -> Xmlmu.t t

  val queue : string -> 'a queue
  val push_queue : 'a queue -> 'a t -> transform t
  val drain_queue : 'a queue -> ('a t -> transform t) -> transform t
  val close_queue : 'a queue -> transform t
  val if_queue_open : 'a queue -> transform t -> transform t -> transform t

  val stack : ?base:'a t -> string -> 'a stack
  val push_stack : 'a stack -> 'a t -> transform t
  val peek_stack :
    'a stack -> ('a t -> transform t) -> transform t -> transform t
  val run_stack : 'a stack -> transform t

  val identity : ('a -> 'a) t

  val replace_xmlns : xmlns t -> transform t
  val declare_xmlns : string -> xmlns t -> transform t

  val select_ : 'p patt_spec -> ('p -> transform t) -> transform t
  val match_ : 'p patt_spec -> ('p * 'a t) list -> 'a t -> 'a t

  val pipe : transform t list -> transform t

  val emit_before : Xmlmu.t t -> transform t
  val emit_after : Xmlmu.t t -> transform t
  val drop : transform t

  val tap : string -> transform t

  val eval : (unit -> transform t) -> transform
end

module Interpreter : INTERP

val to_input : Xmlmu.input -> transform -> Xmlmu.input
val pump : Xmlmu.input -> transform -> Xmlmu.output -> unit
