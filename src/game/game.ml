(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type thunk = unit -> unit

let init () =
	()

let fini () =
	()

let emission_queue = Queue.create ()
let tmp_queue = Queue.create ()

(* let emitl mo msg = Socket.emitl mo msg *)

let emitl mo msg =
	Queue.add (mo, msg) emission_queue

let player_list = ref []

let current_players () = !player_list

let add_current_player mo =
	assert (not (List.memq mo !player_list));
	player_list := mo :: !player_list

let remove_current_player mo =
	assert (List.memq mo !player_list);
	player_list := Utils.Ext.remove mo !player_list

let output_iter f =
	let f' (mo, msg) = f mo msg in
		Queue.clear tmp_queue;
		Queue.transfer emission_queue tmp_queue;
		Queue.iter f' tmp_queue

let pump_till_current = Workqueue.pump_till_current
let top_priority = Workqueue.top_priority
let workqueue_post = Workqueue.post

let do_shutdown = Reset.do_shutdown
let set_shutdown = Reset.set_shutdown


