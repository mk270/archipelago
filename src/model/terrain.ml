(*
  Archipelago, a multi-user dungeon (MUD) server, by Martin Keegan

  Copyright (C) 2009-2012  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)


type permalight =
	| Permalit
	| Permadark

type coverage =
	| Indoors
	| Outside

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

type terrain =
	| Noweather of permalight
	| Weather of coverage * climate

let coverage = function
	| "0" | "1" | "2" | "3"	| "4" | "5"	| "6" | "7" | "8" | "9" -> Outside
	|       "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" -> Indoors
	| _ -> assert false

let ground = function
	| "1" | "a" -> Hardground
	| "2" | "b" -> Softground
	| "3" | "c" -> Marsh
	| "4" | "d" -> Woods
	| "5" | "e" -> Jungle
	| "6" | "f" -> Water
	| "7" | "g" -> Arctic
	| "8" | "h" -> Sand
	| "9" | "i" -> Cyclone
	| _ -> assert false

let create = function
	| "-" -> Noweather Permalit
	| "0" -> Noweather Permadark
	| _ as s -> Weather ((coverage s), (ground s))

let has_weather = function
	| Noweather _ -> false
	| Weather _ -> true

let is_indoors = function
	| Weather (Indoors, _) -> true
	| _ -> false

exception No_climate

let climate = function
	| Noweather _ -> raise No_climate
	| Weather (_, climate) -> climate

let is_permalit = function
	| Noweather Permalit -> true
	| _ -> false

let has_trees = function
	| Weather (_, Jungle) -> true
	| Weather (_, Woods) -> true
	| _ -> false
