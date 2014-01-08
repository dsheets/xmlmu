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

type pos = Uri.t * int
type t
type input
type output
type signal = Xmlm.signal

val empty_src : Uri.t

val make_input : Uri.t -> in_channel -> input
val input_of_in_channel : in_channel -> input
val input_of_file : string -> input
val input_of_generator : (unit -> t) -> input
val input_of_pipe : in_channel -> input

val signal : t -> signal
val src : t -> Uri.t
val line : t -> int

val input : input -> t
val peek : input -> t
val eoi : input -> bool
val pos : input -> pos

val transform : t -> signal -> pos -> t
val synthesize: signal -> pos -> t

val output_of_out_channel :
  ?decl:bool -> ?indent:int option -> out_channel -> output
val output_of_file :
  ?decl:bool -> ?indent:int option -> string -> output
val output_of_pipe : out_channel -> output

val output : output -> t -> unit
val depth : output -> int
