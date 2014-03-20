(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Model

val init : unit -> unit
val fini : unit -> unit

val emitl : mudobject -> string -> unit
val current_players : unit -> mudobject list
val add_current_player : mudobject -> unit
val remove_current_player : mudobject -> unit
val output_iter : (mudobject -> string -> unit) -> unit

(* wrapper for workqueue *)
type thunk = unit -> unit

val pump_till_current : unit -> unit
val top_priority : unit -> float
val workqueue_post : delay : float -> thunk -> unit

(* wrapper for reset *)
val do_shutdown : unit -> bool
val set_shutdown : unit -> unit
