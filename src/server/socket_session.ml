(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let handle_read s =
	match Socket.is_listener s with
		| true -> Session.new_connection s (* return new socket to multiplexer *)
		| false -> Socket.read s s; None

