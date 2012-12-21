(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type t =
	| Vanilla
	| Management
	| Seer
	| Necro
	| Fighter
	| Thief

val guild_of_string : string -> t
val joinable : t -> bool
val can_join : t -> bool
val string_of_guild : t -> string

