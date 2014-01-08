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

module M = Map.Make(String)

type pos = Uri.t * int
type t = { signal : Xmlm.signal; src : Uri.t; line : int; }

type 'a name_map = 'a M.t
type scope = { prefix : string M.t; entity : string M.t; }

type input = {
  input : unit -> t;
  peek  : unit -> t;
  eoi   : unit -> bool;
  pos   : unit -> pos;
  stack : scope list ref;
}

type output = {
  output : t -> unit;
  depth  : unit  -> int;
}

type signal = Xmlm.signal

let empty_src = Uri.of_string ""

(* TODO:
   check initial pos
   check pos after first line
   check xmlm errors
   check pipe errors
   prov stack
*)

let empty_scope = { prefix=M.empty; entity=M.empty; }

let lookup stack key proj = match !stack with
  | [] -> None
  | s::_ -> (try Some (M.find key (proj s)) with Not_found -> None)

let make_input src ic =
  let stack = ref [] in
  let ns prefix = lookup stack prefix (fun s -> s.prefix) in
  let entity ent = lookup stack ent (fun s -> s.entity) in
  let inp = Xmlm.make_input ~ns ~entity (`Channel ic) in
  let pos () = let (line,_) = Xmlm.pos inp in (src,line) in
  let input () =
    let signal = Xmlm.input inp in
    { signal; src; line=snd (pos ())}
  in
  let peek () =
    let signal = Xmlm.peek inp in
    { signal; src; line=snd (pos ())}
  in
  let eoi () = Xmlm.eoi inp in
  { input; peek; eoi; pos; stack; }
let input_of_in_channel ic = make_input empty_src ic
let input_of_file filename =
  make_input (Uri.of_string filename) (open_in filename)

let input_of_generator gen =
  let last = ref { signal=`Dtd None; src=empty_src; line=0; } in
  let peek = ref None in
  let pos () = let { src; line } = !last in (src,line) in
  let input () = match !peek with
    | None -> let i = gen () in last := i; i
    | Some p -> last := p; peek := None; p
  in
  let peek () = match !peek with
    | None -> let i = gen () in peek := Some i; i
    | Some p -> p
  in
  let eoi () = false in (* TODO: catch errors *)
  { input; peek; eoi; pos; stack=ref []; }

let input_of_pipe ic =
  input_of_generator ((fun () -> Marshal.from_channel ic) : unit -> t)

let signal ({ signal }) = signal
let src ({ src }) = src
let line ({ line }) = line

let input i = i.input ()
let peek i = i.peek ()
let eoi i = i.eoi ()
let pos i = i.pos ()
let push_scope i scope = i.stack := scope::!(i.stack)
let peek_scope i = match !(i.stack) with [] -> empty_scope | s::_ -> s
let drop_scope i = match !(i.stack) with [] -> () | _::t -> i.stack := t

let transform t signal (src,line) =
  { signal; src; line; } (* TODO: prov stack *)
let synthesize signal (src,line) = { signal; src; line; }

let make_output ~decl ~indent oc =
  let nl = true in
  let outp = Xmlm.make_output ~nl ~decl ~indent (`Channel oc) in
  let output i = Xmlm.output outp i.signal in
  let depth () = Xmlm.output_depth outp in
  { output; depth; }
let output_of_out_channel ?(decl=true) ?(indent=None) oc =
  make_output ~decl ~indent oc
let output_of_file ?(decl=true) ?(indent=None) filename =
  make_output ~decl ~indent (open_out filename)

let output_of_pipe oc =
  let depth_counter = ref 0 in
  let output i =
    (match i.signal with
    | `Data _ | `Dtd _ -> ()
    | `El_start _ -> incr depth_counter
    | `El_end -> decr depth_counter);
    Marshal.to_channel oc i []
  in
  let depth () = !depth_counter in
  { output; depth; }

let output o t = o.output t
let depth o = o.depth ()
