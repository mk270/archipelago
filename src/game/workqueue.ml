(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type thunk = unit -> unit

let queue = ref Prioqueue.empty

let post ~delay task =
	let prio = Unix.gettimeofday () +. delay in
		queue := Prioqueue.insert !queue prio task

let apply i = i ()

let pump_till_current () =
	let buffer = Queue.create () in
	let now = Unix.gettimeofday () in
	let add_to_buffer _ elt =
		Queue.add elt buffer
	in
		queue := Prioqueue.iter ~comparand:now add_to_buffer !queue;
		Queue.iter apply buffer

let top_priority () =
	Prioqueue.top_priority !queue
