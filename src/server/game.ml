(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let emission_queue = Queue.create ()
let tmp_queue = Queue.create ()

(* let emitl mo msg = Socket.emitl mo msg *)

let emitl mo msg =
	Queue.add (mo, msg) emission_queue

let current_players = Socket.current_players

let output_iter f =
	let f' (mo, msg) = f mo msg in
		Queue.clear tmp_queue;
		Queue.transfer emission_queue tmp_queue;
		Queue.iter f' tmp_queue

let pump_till_current = Workqueue.pump_till_current
let top_priority = Workqueue.top_priority
