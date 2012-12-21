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

let joinable = function
	| Vanilla
	| Management -> false
	| _ -> true

let can_join = function
	| Vanilla -> true
	| _ -> false

let guild_of_string = function
	| "thieves"
	| "thief" -> Thief
	| "necromancers"
	| "necromancer"
	| "necro" -> Necro
	| "seer"
	| "seers" -> Seer
	| "fighers" -> Fighter
	| "fighter" -> Fighter
	| "management" -> Management
	| "vanilla" -> Vanilla
	| _ -> raise Not_found

let string_of_guild = function
	| Vanilla -> "vanilla"
	| Management -> "management"
	| Seer -> "seer"
	| Necro -> "necromancer"
	| Fighter -> "fighter"
	| Thief -> "thief"
