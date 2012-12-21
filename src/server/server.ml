(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


open Socket

let callbacks = [
	Connection.check_logout;
	Monster.pump_events;
	Events.pump_all;
	Leaving.check_deaths;
]

let run () =
	let h = Connection.line_protocol in
	let l = Socket.create (Listener (2500, h)) in
	let m = Multiplexer.create () in
	let register cb = Multiplexer.add_timeout_callback m (fun () -> cb ()) in
		Multiplexer.add_socket m l;
		List.iter register callbacks;
		Multiplexer.run m
