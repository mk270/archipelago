(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type structure =
	| Graph
	| Tree

type ('a, 'b) node

val create : ('b -> structure) -> 'a -> ('a, 'b) node
val remove_from : ('a, 'b) node -> ('a, 'b) node -> 'b -> unit
val insert_into : ('a, 'b) node -> ('a, 'b) node -> 'b -> unit
val contained : ('a, 'b) node -> 'a
val destinations_of : ('a, 'b) node -> 'b -> ('a, 'b) node list
val sources_of : ('a, 'b) node -> 'b -> ('a, 'b) node list
