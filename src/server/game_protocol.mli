(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type game_proto_msg =
	| NewLine of Socket.socket * string
	| Hangup of Socket.socket
	| Logout of Session.session * Model.mudobject

val dispatch : game_proto_msg -> unit
val transition_state : Session.session -> Session.connection_state -> unit
