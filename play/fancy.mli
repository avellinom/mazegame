(** This module [Fancy] provides support for generating large ASCII letters and
    messages. *)

val fancy_message : string -> unit
(** [fancy_message s] prints s to the console with big letters (i.e., as a fancy
    message). Requires: s is lowercase letters. *)
