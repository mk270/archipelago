(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type climate =
	| Hardground
	| Softground
	| Marsh
	| Woods
	| Jungle
	| Water
	| Arctic
	| Sand
	| Cyclone

type terrain

exception No_climate

val create : string -> terrain

val has_weather : terrain -> bool
val is_indoors : terrain -> bool
val climate : terrain -> climate
val is_permalit : terrain -> bool
val has_trees : terrain -> bool
