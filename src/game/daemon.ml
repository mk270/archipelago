(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type daemon = {
	mutable dm_running : bool;
	dm_frequency : float;
	dm_tick : self : daemon -> unit;
}

let daemons = ref []

let daemon_template = {
	dm_running = true;
	dm_frequency = 60.0;
	dm_tick = fun ~self:_ -> ()
}

let create ~frequency ~tick_fn =
	let dmn = { daemon_template 
				with 
					dm_frequency = frequency;
					dm_tick = tick_fn }
	in
		daemons := dmn :: !daemons;
		dmn

(* is the fun -> tick thing properly tail-recursive? *)
let rec enqueue dmn =
	Workqueue.post ~delay:dmn.dm_frequency (fun () -> tick dmn)
and tick dmn =
	dmn.dm_tick ~self:dmn;
	if dmn.dm_running
	then enqueue dmn

let start dmn =
	enqueue dmn

let stop' dmn = 
	dmn.dm_running <- false

let stop dmn =
	stop' dmn;
	daemons := Utils.Ext.remove dmn !daemons

let fini () =
	List.iter stop' !daemons;
	daemons := []
