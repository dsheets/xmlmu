(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type k = env -> Xmlmu.t -> unit
and env = { enter_k : k; exit_k : k; input : Xmlmu.input; }
type transform = k -> k

module type INTERP = sig
  type dtd
  type xmlns
  type element
  type 'a queue
  type 'a stack
  type 't t
  type _ patt_spec =
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

module Interpreter : INTERP = struct
  type dtd = string option
  type xmlns = string
  type element = string
  type 'a queue = {
    qdoc : string;
    mutable head : 'a t list;
    mutable tail : 'a t list;
    mutable closed : bool;
  }
  and 'a stack = {
    sdoc : string;
    mutable stack : (int * 'a t) list;
    base : (int * 'a t) list;
  }
  and 't t =
  | Push_queue : 'a queue * 'a t -> transform t
  | Drain : 'a queue * ('a t -> transform t) -> transform t
  | Close : 'a queue -> transform t
  | If_open : 'a queue * transform t * transform t -> transform t
  | Push_stack : 'a stack * 'a t -> transform t
  | Run : 'a stack -> transform t
  | Peek : 't stack * ('t t -> transform t) * transform t -> transform t
  | Identity : ('a -> 'a) t
  | Replace_xmlns : xmlns t -> transform t
  | Declare_xmlns : string * xmlns t -> transform t
  | Select : 'p patt_spec * ('p -> transform t) -> transform t
  | Match : 'p patt_spec * ('p * 't t) list * 't t -> 't t
  | Add_attr : Xmlm.attribute t -> transform t
  | Pipe : transform t list -> transform t
  | Emit_before : Xmlmu.t t -> transform t
  | Emit_after : Xmlmu.t t -> transform t
  | Drop : transform t
  | Dtd_signal : dtd t -> Xmlmu.t t
  | Literal : 't -> 't t
  | Tap : string -> transform t
  and 'p patt_spec =
  | Element : element t patt_spec
  | Xmlns : xmlns t patt_spec
  | Dtd : dtd t patt_spec
  | This : 'p t -> 'p t patt_spec

  let new_stack sdoc base = { sdoc; stack=base; base; }

  let dtd so = Literal so
  let xmlns s = Literal s
  let element s = Literal s
  let dtd_signal dtd = Dtd_signal dtd
  let data_signal s =
    Literal Xmlmu.(synthesize (`Data s) (empty_src,0)) (* TODO: prov *)
  let attr s = Literal s

  let queue qdoc = { qdoc; head=[]; tail=[]; closed=false; }
  let push_queue q v = Push_queue (q,v)
  let drain_queue q f = Drain (q,f)
  let close_queue q = Close q
  let if_queue_open q t f = If_open (q,t,f)

  let stack ?base sdoc = match base with
    | None   -> new_stack sdoc []
    | Some b -> new_stack sdoc [0,b]
  let push_stack s v = Push_stack (s,v)
  let peek_stack s top def = Peek (s,top,def)
  let run_stack s = Run s

  let identity = Identity

  let replace_xmlns x = Replace_xmlns x
  let declare_xmlns prefix ns = Declare_xmlns (prefix,ns)

  let select_ patt f = Select (patt,f)
  let match_ patt cases default = Match (patt,cases,default)

  let add_attr attr = Add_attr attr

  let pipe seq = Pipe seq

  let emit_before t = Emit_before t
  let emit_after t = Emit_after t
  let drop = Drop

  let tap msg = Tap msg

  let drain :
  type a. (transform t -> transform) -> a queue -> (a t -> transform t)
    -> transform =
    fun eval q f k ->
      let rec pump env event = match q.head with
        | [] -> (match q.tail with
          | [] -> k env event
          | tl -> q.head <- List.rev tl; q.tail <- []; pump env event)
        | x::xs ->
          q.head <- xs;
          eval (f x) pump env event
      in pump

  let eval exprf =
    let expr = exprf () in
    let rec eval_transform expr = (fun k env event ->
      let signal = Xmlmu.signal event in
      match signal with
      | `Dtd d -> dtd_event d event expr k env event
      | `Data _ -> k env event
      | `El_start el -> el_start el event expr k env event
      | `El_end -> el_end expr k env event
    )
    and el_start : type a. Xmlm.tag -> Xmlmu.t -> a t -> a = begin
      fun (((ns,el),attrs) as tag) event -> function
      | Tap msg -> Printf.eprintf "%20s: start <%s>:%s\n%!" msg ns el; (@@)
      | Push_queue (q,v) ->
        if q.closed then (@@) else (q.tail <- v::q.tail; (@@))
      | Drain (q,f) -> drain eval_transform q f
      | Close q -> q.closed <- true; (@@)
      | If_open (q,t,f) -> el_start tag event (if q.closed then f else t)
      | Push_stack (s,v) -> s.stack <- (0,v)::s.stack; (@@)
      | Run s -> begin match s.stack with
        | [] -> (@@)
        | (d,v)::r -> s.stack <- (d+1,v)::r; (@@)
      end
      | Peek (s,top,def) -> begin match s.stack with
        | [] -> el_start tag event def
        | (_,v)::_ -> el_start tag event (top v)
      end
      | Identity -> fun x -> x
      | Replace_xmlns ns_expr -> fun k env event ->
        let s = el_start tag event ns_expr in
        k env (Xmlmu.transform event
                 (`El_start ((s,el),attrs))
                 (Xmlmu.(src event, line event))) (* TODO: prov? *)
      | Declare_xmlns (prefix,ns) ->
        let s = el_start tag event ns in
        el_start tag event (Add_attr (attr ((Xmlm.ns_xmlns,prefix),s)))
      | Select (Dtd,_) -> (@@)
      | Select (Xmlns,f_xmlns) ->
        el_start tag event (f_xmlns (xmlns ns))
      | Select (Element,f_element) ->
        el_start tag event (f_element (element el))
      | Select (This t,f_t) ->
        el_start tag event (f_t t)
      | Match (Dtd,_,default) -> el_start tag event default (* TODO: default? *)
      | Match (Xmlns,cases,default) ->
        let branch = try List.assoc (xmlns ns) cases
          with Not_found -> default in
        el_start tag event branch
      | Match (Element,cases,default) ->
        let branch = try List.assoc (element el) cases
          with Not_found -> default in
        el_start tag event branch
      | Match (This t, cases,default) ->
        let branch = try List.assoc t cases
          with Not_found -> default in
        el_start tag event branch
      | Add_attr attr -> fun k env event ->
        k env (Xmlmu.transform event
                 (`El_start ((ns,el),(el_start tag event attr)::attrs))
                 (Xmlmu.(src event, line event))) (* TODO: prov? *)
      | Pipe stages -> List.fold_right eval_transform stages
      | Emit_before before_event -> fun k env event ->
        eval_transform expr k env (el_start tag event before_event); k env event
      | Emit_after after_event -> fun k env event ->
        k env event; eval_transform expr k env (el_start tag event after_event)
      | Drop -> fun _ _ _ -> ()
      | Dtd_signal dtd ->
        let so = el_start tag event dtd in
        Xmlmu.(synthesize (`Dtd so) (empty_src,0)) (* TODO: prov *)
      | Literal n -> n
    end
    and el_end : type a. a t -> a = function
      | Tap msg -> Printf.eprintf "%20s: end\n%!" msg; (@@)
      | Push_queue (q,v) ->
        if q.closed then (@@) else (q.tail <- v::q.tail; (@@))
      | Drain (q,f) -> drain eval_transform q f
      | Close q -> q.closed <- true; (@@)
      | If_open (q,t,f) -> el_end (if q.closed then f else t)
      | Push_stack (s,v) -> s.stack <- (0,v)::s.stack; (@@)
      | Run s -> begin match s.stack with
        | [] -> (@@)
        | (d,v)::r -> (if d=0 then s.stack <- r else s.stack <- (d-1,v)::r); (@@)
      end
      | Peek (s,top,def) -> begin match s.stack with
        | [] -> el_end def
        | (_,v)::_ -> el_end (top v)
      end
      | Identity        -> fun x -> x
      | Replace_xmlns _ -> (@@)
      | Declare_xmlns _ -> (@@)
      | Select (This t,f_t) -> el_end (f_t t)
      | Select (_,_)    -> (@@)
      | Match (This t,cases,default) ->
        let branch = try List.assoc t cases
          with Not_found -> default in
        el_end branch
      | Match (_,_,default) -> el_end default (* TODO: default? *)
      | Add_attr _ -> (@@)
      | Pipe stages     -> List.fold_right eval_transform stages
      | Emit_before before_event -> fun k env event ->
        eval_transform expr k env (el_end before_event); k env event
      | Emit_after after_event -> fun k env event ->
        k env event; eval_transform expr k env (el_end after_event)
      | Drop -> fun _ _ _ -> ()
      | Dtd_signal dtd ->
        let so = el_end dtd in
        Xmlmu.(synthesize (`Dtd so) (empty_src,0)) (* TODO: prov *)
      | Literal n -> n
    and dtd_event : type a. string option -> Xmlmu.t -> a t -> a = begin
      fun dtdo event -> function
      | Tap msg -> Printf.eprintf "%20s: dtd %s\n%!" msg
        (match dtdo with None -> "none" | Some dtd -> "some "^dtd); (@@)
      | Push_queue (q,v) ->
        if q.closed then (@@) else (q.tail <- v::q.tail; (@@))
      | Drain (q,f) -> drain eval_transform q f
      | Close q -> q.closed <- true; (@@)
      | If_open (q,t,f) -> dtd_event dtdo event (if q.closed then f else t)
      | Push_stack (s,v) -> s.stack <- (0,v)::s.stack; (@@)
      | Run _ -> (@@)
      | Peek (s,top,def) -> begin match s.stack with
        | [] -> dtd_event dtdo event def
        | (_,v)::_ -> dtd_event dtdo event (top v)
      end
      | Identity -> fun x -> x
      | Replace_xmlns _ -> (@@)
      | Declare_xmlns _ -> (@@)
      | Select (Dtd,f_dtd) -> dtd_event dtdo event (f_dtd (dtd dtdo))
      | Select (Xmlns,_) -> (@@)
      | Select (Element,_) -> (@@)
      | Select (This t,f_t) -> dtd_event dtdo event (f_t t)
      | Match (Dtd,cases,default) ->
        let branch = try List.assoc (dtd dtdo) cases
          with Not_found -> default in
        dtd_event dtdo event branch
      | Match (Xmlns,_,default) ->
        dtd_event dtdo event default (* TODO: default? *)
      | Match (Element,_,default) ->
        dtd_event dtdo event default (* TODO: default? *)
      | Match (This t,cases,default) ->
        let branch = try List.assoc t cases
          with Not_found -> default in
        dtd_event dtdo event branch
      | Add_attr _ -> (@@)
      | Pipe stages -> List.fold_right eval_transform stages
      | Emit_before before_event -> fun k env event ->
        eval_transform expr k env (dtd_event dtdo event before_event);
        k env event
      | Emit_after after_event -> fun k env event ->
        k env event;
        eval_transform expr k env (dtd_event dtdo event after_event)
      | Drop -> fun _ _ _ -> ()
      | Dtd_signal dtd ->
        let so = dtd_event dtdo event dtd in
        Xmlmu.(synthesize (`Dtd so) (empty_src,0)) (* TODO: prov *)
      | Literal n -> n
    end
    in
    eval_transform expr
end

let to_input input transform =
  let buffer_head = ref [] in
  let buffer_tail = ref [] in
  let rec spool () = match !buffer_head with
    | [] -> (match !buffer_tail with
      | [] -> transform (* TODO: use env to modulate output *)
        (fun _ event -> buffer_tail := event::!buffer_tail)
        { enter_k=(fun _ _ -> ()); exit_k=(fun _ _ -> ()); input; }
        (Xmlmu.input input)
      | xs -> buffer_head := List.rev xs
    ); spool ()
    | h::t -> buffer_head := t; h
  in
  Xmlmu.input_of_generator spool

let pump input transform output =
  while not (Xmlmu.eoi input) do
    transform
      (fun _ -> (Xmlmu.output output)) (* TODO: use env to modulate output *)
      { enter_k=(fun _ _ -> ()); exit_k=(fun _ _ -> ()); input; }
      (Xmlmu.input input)
  done
